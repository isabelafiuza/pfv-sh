#' Executa Pipeline de Previsao
#'
#' Gera previsoes de geracao solar usando modelos pre-treinados.
#' Registra proveniencia, metricas e relatorio de saude.
#'
#' @param args Lista com `input`, `output`, `ids_usinas`, `data_referencia`, `horizonte_dias`,
#'   `modelos_NWP`, `modelos_previsao`, `parametros_periodo_geracao`
#' @param parallel logical (não implementado ainda)
#' @param resume logical (não implementado ainda)
#'
#' @return data.table com previsoes (colunas: id_usina, id_modelo_prev, id_modelo_nwp,
#'   data_hora_rodada, data_hora_previsao, valor);
#'   também salvo em `output/previsao_geracao_fotovoltaica.parquet`
#'
#' @export
predict_main <- function(args, parallel = FALSE, resume = FALSE) {
    provenance <- create_provenance(args, "predict", parallel)
    metrics <- create_metrics(provenance$run_id, "predict")
    set_log_context(provenance$run_id, "predict")
    lg <- get_pkg_logger()

    on.exit({
        if (provenance$status == "running") {
            finalize_provenance(provenance, "failed")
        }
        write_provenance(provenance, args$output)
        finalize_metrics(metrics)
        write_metrics(metrics, args$output)
        report <- build_health_report(provenance, metrics)
        write_health_report(report, args$output)
        clear_log_context()
    }, add = TRUE)

    conn <- conectamock_pfv(args$input)

    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP

    data_prev <- define_hor_prev(args$data_referencia, v_horizonte)

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn)

    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    prev <- lapply(v_usinas, function(iu) {
        lg$info("Processando usina: %s", iu)
        t0 <- proc.time()["elapsed"]
        result <- tryCatch({
            file_name <- paste0(iu, "_modelos_ajustados")
            artifact <- pfvIO:::get_model_artifact(file_name, args$artifact)
            validate_artifact(artifact)
            models <- if (!is.null(artifact$models)) artifact$models else artifact
            predict_usina(iu,
                dt_usinas = dt_usinas,
                dt_ger_obs = data_set_ger,
                dt_prev = data_set_met,
                v_modelos_nwp = v_modelos_nwp,
                v_horizonte = v_horizonte,
                parametros_modelo_previsao = args$modelos_previsao,
                parametros_periodo_geracao = args$parametros_periodo_geracao,
                models = models,
                data_prev = data_prev
            )
        }, error = function(e) {
            lg$warn("Falha na usina %s: %s", iu, conditionMessage(e))
            plant_error(iu, e)
        })
        duration <- proc.time()["elapsed"] - t0
        record_plant_timing(metrics, iu, duration)

        if (is_plant_error(result)) {
            update_plant_status(provenance, iu, "failed")
        } else {
            update_plant_status(provenance, iu, "completed")
        }

        result
    })

    success_idx <- which(!vapply(prev, is_plant_error, logical(1L)))
    if (length(success_idx) == 0L) {
        lg$error("Todas as usinas falharam na predicao")
        finalize_provenance(provenance, "failed")
        return(invisible(NULL))
    }

    dt_final <- rbindlist(
        lapply(success_idx, function(i) {
            id_usina <- v_usinas[i]
            prev_usina <- prev[[i]]

            rbindlist(
                lapply(prev_usina, monta_dt_prev,
                    id_usina = id_usina,
                    data_referencia = args$data_referencia
                )
            )
        })
    )

    dt_final <- completa_datas(dt = dt_final, discretizacao = "30 min")
    dt_final <- combina_media(dt = dt_final)

    setorder(
        dt_final,
        id_usina,
        id_modelo_prev,
        id_modelo_nwp,
        data_hora_previsao
    )

    write_previsao_geracao_fotovoltaica(
        dt = dt_final,
        output_dir = args$output
    )

    n_failed <- sum(vapply(prev, is_plant_error, logical(1L)))
    final_status <- if (n_failed == length(v_usinas)) "failed" else "completed"
    finalize_provenance(provenance, final_status)
}

#' Gera Previsoes para Uma Usina
#'
#' Executa previsao para uma usina especifica.
#'
#' @param iu character, identificador da usina
#' @param dt_usinas data.table, cadastro de usinas
#' @param dt_ger_obs data.table, geracao observada
#' @param dt_prev list, previsoes meteorologicas
#' @param v_modelos_nwp character, modelos NWP
#' @param v_horizonte character, horizontes
#' @param parametros_modelo_previsao list, parametros dos modelos
#' @param parametros_periodo_geracao list, parametros do periodo solar
#' @param models list, modelos ajustados
#' @param data_prev Date, datas-alvo
#'
#' @return list de previsoes (uma por combinacao treinada)
#'
#' @keywords internal
predict_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev, v_modelos_nwp, v_horizonte, parametros_modelo_previsao,
    parametros_periodo_geracao, models, data_prev
) {
    dad_usi <- dt_usinas[id_usina == iu]
    ger_usi <- dt_ger_obs[id_usina == iu]

    dt_prev <- lapply(dt_prev, function(dt) dt[id_modelo_nwp %in% v_modelos_nwp])
    dt_prev <- lapply(dt_prev, associa_nwp_usina, dt_usinas = dt_usinas)
    dt_prev <- lapply(dt_prev, interpola_previsao_nwp)
    dt_prev <- lapply(dt_prev, preenche_ausencia_previsao)
    dt_prev <- lapply(dt_prev, adicionar_passo_previsao)
    dt_prev <- copy(dt_prev)
    dt_prev <- lapply(dt_prev, function(dt) dt[id_usina == iu])

    ger_usi[, hora_min := format(data_hora_observacao, "%H:%M")]
    prev_met_usi <- lapply(dt_prev, function(dt) {
        dt[, hora_min := format(data_hora_previsao, "%H:%M")]
    })

    ger_prev <- lapply(seq_along(models), function(i) {
        pars <- models[[i]]$combinacao_ajuste

        modelo_despacho <- pars$modelo_prev
        modelo_parametros <- parametros_modelo_previsao[[modelo_despacho]]

        prev <- parse_predict(
            modelo = models[[i]]$modelo,
            pars = pars, 
            data_prev = data_prev,
            ger_usi = ger_usi, 
            prev_met_usi = prev_met_usi, 
            v_horizonte = v_horizonte,
            modelo_parametros = modelo_parametros
        )
        list(
            combinacao_ajuste = pars,
            prev = prev
        )
    })
}

#' Metodo Generico para Previsao
#'
#' Funcao generica S3 que despacha para metodo especifico de previsao.
#'
#' @param modelo Objeto do modelo treinado (com classe S3)
#' @param ... Argumentos adicionais passados aos metodos especificos
#'
#' @return Vetor numerico com valores previstos
#'
#' @export
parse_predict <- function(modelo, ...) UseMethod("parse_predict")

#' Metodo Default para parse_predict
#'
#' @param modelo Objeto do modelo
#' @param ... Argumentos adicionais (ignorados)
#'
#' @return gera erro com tipo de modelo desconhecido
#'
#' @export
parse_predict.default <- function(modelo, ...) {
    stop("Unknown model type for parse_predict")
}

#' Previsao com Modelo ARIMAX
#'
#' Gera previsoes usando modelo ARIMA/ARIMAX recalibrado.
#'
#' @param modelo list com `modelo_escolhido` ("ARIMA" ou "ARIMAX"), `modelo_final`
#' @param ... Argumentos: `pars`, `data_prev`, `ger_usi`, `prev_met_usi`, `modelo_parametros`, `v_horizonte`
#'
#' @return Vetor numerico com valores previstos (MW); NA se dados insuficientes
#'
#' @export
parse_predict.arimax <- function(modelo, ...) {
    args <- list(...)
    pars <- args$pars
    data_prev <- args$data_prev
    ger_usi <- args$ger_usi
    prev_met_usi <- args$prev_met_usi
    modelo_parametros <- args$modelo_parametros
    v_horizonte <- args$v_horizonte

    dt_filt <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    dt_filt[, (names(dt_filt)) := lapply(.SD, function(x) fifelse(x == 999, NA, x))]

    dt_treino_filt <- dt_filt[as.Date(data_hora) < data_prev[1]]
    pos_hor <- which(v_horizonte == pars$horiz_prev)
    dt_prev_filt <- dt_filt[as.Date(data_hora) == data_prev[pos_hor]]
    setnames(dt_prev_filt, "valor", "ger_obs")

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(dt_treino_filt,
        data_prev,
        janela_dias_treinamento = modelo_parametros$n_dias_treino
    )
    setnames(dt_treino_filt, "valor", "ger_obs")

    if (dados_suficientes(dt_treino_filt, num_min_dados = modelo_parametros$amos_min)) {
        norm_resultado <- normaliza_variaveis(dt_treino_filt)
        dt_treino_norm <- norm_resultado$dados
        stats_norm <- norm_resultado$stats

        cols_norm <- grep("_norm$", names(dt_treino_norm), value = TRUE)
        dt_treino_norm <- dt_treino_norm[, ..cols_norm]

        dt_prev_norm <- copy(dt_prev_filt)
        dt_prev_norm[, data_hora := NULL]

        for (i in seq_len(nrow(stats_norm))) {
            var <- stats_norm$variavel[i]

            media <- stats_norm$med[i]
            desvio <- stats_norm$sd[i]
            dt_prev_norm[, paste0(var, "_norm") := (get(var) - media) / desvio]
            dt_prev_norm[, (var) := NULL]
        }

        var_exog <- setdiff(names(dt_prev_norm), "ger_obs_norm")

        mod_selec <- modelo$modelo_escolhido
        if (mod_selec == "ARIMAX") {
            nlmod1 <- recalibra_arimax(dt_treino_norm, modelo$modelo_final)

            xreg_fut <- as.matrix(dt_prev_norm[, ..var_exog])
            prev_norm <- forecast(nlmod1, xreg = xreg_fut, h = nrow(dt_prev_norm))$mean
        } else {
            nlmod1 <- recalibra_arima(dt_treino_norm, modelo$modelo_final)

            prev_norm <- forecast(nlmod1, h = nrow(dt_prev_norm))$mean
        }

        dt_prev_out <- data.table(ger_obs_norm = prev_norm)
        dt_prev_out <- desnormaliza_variaveis(dt_prev_out, stats_norm)

        prev_final <- dt_prev_out$ger_obs
    } else {
        prev_final <- NA_real_
    }
}

#' Previsao com Modelo Fisico-Estimado
#'
#' Gera previsoes usando modelo de regressao linear (RLS ou RLM).
#'
#' @param modelo list com `modelo_escolhido` ("RLS" ou "RLM"), `modelo_final`, `variaveis_usadas`
#' @param ... Argumentos: `pars`, `prev_met_usi`, `ger_usi`, `v_horizonte`, `data_prev`
#'
#' @return Vetor numerico com valores previstos (MW)
#'
#' @export
parse_predict.fisico_estimado <- function(modelo, ...) {
    args <- list(...)
    pars <- args$pars
    prev_met_usi <- args$prev_met_usi
    ger_usi <- args$ger_usi
    v_horizonte <- args$v_horizonte
    data_prev <- args$data_prev
    mod_aju_comb <- modelo

    dt_filt <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    pos_hor <- which(v_horizonte == pars$horiz_prev)
    dt_prev_filt <- dt_filt[as.Date(data_hora) == data_prev[pos_hor]]
    setnames(dt_prev_filt, "valor", "ger_obs")

    nlmod <- mod_aju_comb$modelo_final
    variaveis_usadas <- mod_aju_comb$variaveis_usadas
    cols <- names(dt_prev_filt)[names(dt_prev_filt) %in% variaveis_usadas]
    variaveis_previstas <- dt_prev_filt[, .SD, .SDcols = cols]

    dt_prev <- as.data.table(predict(nlmod, variaveis_previstas, interval = "prediction"))
    dt_prev$fit
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Carrega Artefato de Modelo
#'
#' Le arquivo RDS com modelo treinado.
#'
#' @param modelo_previsao character, tipo do modelo
#' @param id_usina character, identificador da usina
#' @param diretorio character, caminho do diretorio
#'
#' @return Objeto do modelo carregado
#'
#' @keywords internal
carrega_modelo_rds <- function(modelo_previsao, id_usina, diretorio) {
    readRDS(paste(diretorio, paste0(id_usina, "_", modelo_previsao, "_ajustado.rds"), sep = "/"))
}

#' Recalibra Modelo ARIMA
#'
#' Reajusta ARIMA mantendo estrutura (p,d,q) com dados atualizados.
#'
#' @param dt data.table com coluna `ger_obs_norm` (geracao normalizada)
#' @param nlmod0 Modelo ARIMA original
#'
#' @return Objeto Arima recalibrado
#'
#' @keywords internal
recalibra_arima <- function(dt, nlmod0) {
    y_validos <- dt$ger_obs_norm[!is.na(dt$ger_obs_norm)]
    Arima(y_validos, model = nlmod0)
}

#' Recalibra Modelo ARIMAX
#'
#' Reajusta ARIMAX mantendo estrutura com dados atualizados.
#'
#' @param dt data.table com `ger_obs_norm` e variaveis exogenas normalizadas
#' @param nlmod0 Modelo ARIMAX original
#'
#' @return Objeto Arima recalibrado com variaveis exogenas
#'
#' @keywords internal
recalibra_arimax <- function(dt, nlmod0) {
    var_exog <- setdiff(names(dt), "ger_obs_norm")
    dt_valido <- dt[complete.cases(dt[, c("ger_obs_norm", ..var_exog)])]

    xreg <- dt_valido[, ..var_exog]

    Arima(dt_valido$ger_obs_norm,
        xreg = as.matrix(xreg),
        model = nlmod0
    )
}

#' Desnormaliza Variaveis
#'
#' Reverte normalizacao z-score, retornando valores a escala original.
#'
#' @param dt data.table com colunas normalizadas (sufixo `_norm`)
#' @param stats_norm data.table com `variavel`, `med`, `sd`
#'
#' @return data.table desnormalizado (sem sufixo `_norm`)
#'
#' @keywords internal
desnormaliza_variaveis <- function(dt, stats_norm) {
    dt_out <- copy(dt)

    var <- sub("_norm$", "", names(dt_out))
    if (var %in% stats_norm$variavel) {
        media <- stats_norm[variavel == var, med]
        desvio <- stats_norm[variavel == var, sd]
        dt_out[, (var) := get(names(dt_out)) * desvio + media]
    }

    return(dt_out)
}
