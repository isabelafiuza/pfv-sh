#' Carrega Estado de Retomada da Previsao
#'
#' Le o checkpoint de previsao e identifica usinas ja completadas cujos
#' resultados intermediarios existem em disco. Apenas usinas com checkpoint
#' *e* resultado RDS sao consideradas concluidas -- a verificacao extra e
#' obrigatoria porque a consolidacao final exige o resultado real, nao
#' apenas o status.
#'
#' @param args lista de argumentos do pipeline (deve conter `output` e
#'     `ids_usinas`)
#' @param provenance environment de proveniencia criado por
#'     [create_provenance()]
#'
#' @return lista com `provenance` (atualizado) e `completed` (character
#'     vector de IDs de usinas ja processadas com resultado disponivel)
#'
#' @seealso [read_checkpoint()], [get_pending_plants()],
#'     [read_plant_result()]
#'
#' @export
load_predict_resume_state <- function(args, provenance) {
    checkpoint <- read_checkpoint(args$output, args)
    if (is.null(checkpoint)) {
        return(list(provenance = provenance, completed = character(0L)))
    }
    candidate_completed <- setdiff(
        args$ids_usinas, get_pending_plants(checkpoint)
    )
    completed_plants <- Filter(function(iu) {
        !is.null(read_plant_result(iu, args$output))
    }, candidate_completed)
    for (iu in completed_plants) {
        update_plant_status(provenance, iu, "completed")
    }
    list(provenance = provenance, completed = completed_plants)
}

#' Carrega Artefato e Executa Previsao para Uma Usina
#'
#' Funcao wrapper compativel com `run_plants()`: carrega o artefato de
#' modelo, valida, extrai os modelos ajustados e delega a execucao a
#' [predict_usina()].
#'
#' @param iu character, identificador da usina
#' @param dt_usinas data.table, cadastro de usinas
#' @param dt_ger_obs data.table, geracao observada
#' @param dt_prev list, previsoes meteorologicas
#' @param v_modelos_nwp character, modelos NWP
#' @param v_horizonte character, horizontes
#' @param parametros_modelo_previsao list, parametros dos modelos
#' @param parametros_periodo_geracao list, parametros do periodo solar
#' @param artifact_dir character, diretorio de artefatos
#' @param data_prev Date, datas-alvo
#' @param ... argumentos adicionais (ignorados)
#'
#' @return list de previsoes (retorno de [predict_usina()])
#'
#' @keywords internal
predict_plant_wrapper <- function(
    iu,
    dt_usinas,
    dt_ger_obs,
    dt_prev,
    v_modelos_nwp,
    v_horizonte,
    parametros_modelo_previsao,
    parametros_periodo_geracao,
    artifact_dir,
    data_prev,
    ...
) {
    file_name <- paste0(iu, "_modelos_ajustados")
    artifact <- pfvIO:::get_model_artifact(file_name, artifact_dir)
    validate_artifact(artifact)
    models <- if (!is.null(artifact$models)) artifact$models else artifact
    predict_usina(
        iu,
        dt_usinas = dt_usinas,
        dt_ger_obs = dt_ger_obs,
        dt_prev = dt_prev,
        v_modelos_nwp = v_modelos_nwp,
        v_horizonte = v_horizonte,
        parametros_modelo_previsao = parametros_modelo_previsao,
        parametros_periodo_geracao = parametros_periodo_geracao,
        models = models,
        data_prev = data_prev
    )
}

#' Processa Resultados de Previsao por Usina
#'
#' Itera sobre os resultados de `run_plants`, atualiza status de
#' proveniencia e, quando `resume = TRUE`, persiste resultado intermediario
#' e checkpoint apos cada usina bem-sucedida.
#'
#' @param resultados lista de resultados de [run_plants()] (um por usina)
#' @param v_usinas character vector de IDs de usinas, na mesma ordem que
#'     `resultados`
#' @param provenance environment de proveniencia criado por
#'     [create_provenance()]
#' @param metrics objeto de metricas criado por [create_metrics()]
#' @param lg objeto logger
#' @param resume logical, se `TRUE` chama [write_plant_result()] e
#'     [write_checkpoint()] apos cada usina concluida
#' @param output_dir character, diretorio de saida para checkpoint e
#'     resultados intermediarios
#'
#' @return inteiro com o numero de usinas que falharam
#'
#' @keywords internal
tally_predict_results <- function(
    resultados, v_usinas, provenance, metrics, lg, resume, output_dir
) {
    n_failed <- 0L
    n_total <- length(v_usinas)
    for (i in seq_along(v_usinas)) {
        iu <- v_usinas[i]
        result <- resultados[[i]]
        if (is_plant_error(result)) {
            update_plant_status(provenance, iu, "failed")
            n_failed <- n_failed + 1L
            lg$error("Usina %s falhou: %s", iu, result$error)
        } else {
            update_plant_status(provenance, iu, "completed")
            if (resume) {
                write_plant_result(result, iu, output_dir)
                write_checkpoint(provenance, output_dir)
            }
        }
        lg$info("Usina %s processada (%d/%d)", iu, i, n_total)
    }
    n_failed
}

#' Combina Resultados Retomados com Resultados Novos
#'
#' Para cada usina em `all_ids`: se a usina estava pendente, usa o
#' resultado recen-calculado de `new_results`; caso contrario, carrega
#' o resultado intermediario salvo em disco via [read_plant_result()].
#' Resultados `plant_error` sao convertidos para `NULL`.
#'
#' @param all_ids character vector com todos os IDs de usinas do pipeline
#' @param pending_ids character vector com os IDs processados nesta execucao
#' @param new_results lista de resultados de [run_plants()], alinhada com
#'     `pending_ids`
#' @param output_dir character, diretorio onde resultados intermediarios
#'     foram salvos
#'
#' @return lista de resultados alinhada com `all_ids`; entradas com erro
#'     ou ausentes sao `NULL`
#'
#' @keywords internal
collect_all_results <- function(all_ids, pending_ids, new_results, output_dir) {
    lapply(all_ids, function(iu) {
        if (iu %in% pending_ids) {
            idx <- match(iu, pending_ids)
            r <- new_results[[idx]]
            if (is_plant_error(r)) return(NULL)
            r
        } else {
            read_plant_result(iu, output_dir)
        }
    })
}

#' Executa Pipeline de Previsao
#'
#' Gera previsoes de geracao solar usando modelos pre-treinados.
#' Registra proveniencia, metricas e relatorio de saude.
#'
#' Quando `resume = TRUE`, carrega checkpoint existente em `args$output`,
#' salta usinas ja completadas cujo resultado intermediario existe em disco,
#' persiste o estado apos cada usina e consolida todos os resultados
#' (retomados + novos) na saida final.
#' Quando `parallel = TRUE`, usa `future.apply::future_lapply` para
#' processar usinas em paralelo.
#'
#' @param args Lista com `input`, `output`, `ids_usinas`, `data_referencia`,
#'     `horizonte_dias`, `modelos_NWP`, `modelos_previsao`,
#'     `parametros_periodo_geracao`
#' @param parallel logical, se `TRUE` processa usinas em paralelo via
#'     `future`
#' @param resume logical, se `TRUE` retoma execucao a partir do ultimo
#'     checkpoint valido
#'
#' @return data.table com previsoes (colunas: id_usina, id_modelo_prev,
#'     id_modelo_nwp, data_hora_rodada, data_hora_previsao, valor);
#'     tambem salvo em `output/previsao_geracao_fotovoltaica.parquet`
#'
#' @export
predict_main <- function(args, parallel = FALSE, resume = FALSE) {
    provenance <- create_provenance(args, "predict", parallel)
    metrics <- create_metrics(provenance$run_id, "predict")
    set_log_context(provenance$run_id, "predict")
    lg <- get_pkg_logger()
    completed_plants <- character(0L)

    if (resume) {
        state <- load_predict_resume_state(args, provenance)
        provenance <- state$provenance
        completed_plants <- state$completed
    }

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

    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP

    data_prev <- define_hor_prev(args$data_referencia, v_horizonte)

    dt_usinas <- get_usinas(conn, id_usina = args$ids_usinas)

    data_set <- get_dataset(args, conn)
    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    v_usinas_pending <- setdiff(args$ids_usinas, completed_plants)

    resultados_new <- list()
    n_failed <- 0L

    if (length(v_usinas_pending) > 0L) {
        extra_args <- list(
            dt_usinas = dt_usinas,
            dt_ger_obs = data_set_ger,
            dt_prev = data_set_met,
            v_modelos_nwp = v_modelos_nwp,
            v_horizonte = v_horizonte,
            parametros_modelo_previsao = args$modelos_previsao,
            parametros_periodo_geracao = args$parametros_periodo_geracao,
            artifact_dir = args$artifact,
            data_prev = data_prev
        )

        if (parallel) {
            old_plan <- setup_parallel_plan()
            on.exit(reset_parallel_plan(old_plan), add = TRUE)
        }

        resultados_new <- run_plants(
            v_usinas_pending, predict_plant_wrapper, extra_args,
            parallel, metrics, lg
        )

        n_failed <- tally_predict_results(
            resultados_new, v_usinas_pending,
            provenance, metrics, lg, resume, args$output
        )
    }

    resultados <- collect_all_results(
        args$ids_usinas, v_usinas_pending, resultados_new, args$output
    )

    success_idx <- which(!vapply(resultados, is.null, logical(1L)))
    if (length(success_idx) == 0L) {
        lg$error("Todas as usinas falharam na predicao")
        finalize_provenance(provenance, "failed")
        return(invisible(NULL))
    }

    dt_final <- rbindlist(
        lapply(success_idx, function(i) {
            id_usina <- args$ids_usinas[i]
            prev_usina <- resultados[[i]]
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

    final_status <- if (n_failed == length(args$ids_usinas)) "failed" else "completed"
    finalize_provenance(provenance, final_status)
    if (resume) cleanup_checkpoint(args$output)
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
