#' Carrega Estado de Retomada do Treinamento
#'
#' Le o checkpoint de treinamento e identifica usinas ja completadas para
#' permitir retomada do pipeline. Retorna `completed = character(0L)` se
#' nenhum checkpoint valido for encontrado.
#'
#' @param args lista de argumentos do pipeline (deve conter `output` e `ids_usinas`)
#' @param provenance environment de proveniencia criado por [create_provenance()]
#'
#' @return lista com `provenance` (atualizado) e `completed` (character vector
#'     de IDs de usinas ja processadas)
#'
#' @seealso [read_checkpoint()], [get_pending_plants()]
#'
#' @export
load_train_resume_state <- function(args, provenance) {
    checkpoint <- read_checkpoint(args$output, args)
    if (is.null(checkpoint)) {
        return(list(provenance = provenance, completed = character(0L)))
    }
    completed_plants <- setdiff(args$ids_usinas, get_pending_plants(checkpoint))
    for (iu in completed_plants) {
        update_plant_status(provenance, iu, "completed")
    }
    list(provenance = provenance, completed = completed_plants)
}

#' Despacha Processamento de Usinas (Sequencial ou Paralelo)
#'
#' Itera sobre `v_usinas` chamando `fn` via `do.call`, com isolamento de erro
#' por usina via `plant_error`. No modo sequencial, o tempo por usina e medido
#' individualmente. No modo paralelo, o tempo total do lote e distribuido
#' igualmente.
#'
#' @param v_usinas character vector de IDs de usinas a processar
#' @param fn funcao worker a chamar para cada usina; assinatura
#'     `fn(iu, ...)` onde `...` sao os campos de `extra_args`
#' @param extra_args lista nomeada de argumentos adicionais a passar para `fn`
#' @param parallel logical, se `TRUE` usa `future.apply::future_lapply`
#' @param metrics objeto de metricas criado por [create_metrics()]
#' @param lg objeto logger
#'
#' @return lista com os resultados por usina (na ordem de `v_usinas`);
#'     erros sao encapsulados como objetos `plant_error`
#'
#' @export
run_plants <- function(v_usinas, fn, extra_args, parallel, metrics, lg) {
    if (parallel) {
        per_plant_args <- split_args_by_plant(extra_args, v_usinas)
        batch_start <- proc.time()[["elapsed"]]
        .fn <- fn
        .plant_error <- plant_error
        results <- future.apply::future_lapply(
            per_plant_args,
            function(plant_args) {
                iu <- plant_args$.iu
                plant_args$.iu <- NULL
                tryCatch(
                    do.call(.fn, c(list(iu), plant_args)),
                    error = function(e) .plant_error(iu, e)
                )
            },
            future.seed = TRUE,
            future.globals = list(.fn = .fn, .plant_error = .plant_error)
        )
        batch_elapsed <- proc.time()[["elapsed"]] - batch_start
        est_per_plant <- round(batch_elapsed / length(v_usinas), 2L)
        for (iu in v_usinas) record_plant_timing(metrics, iu, est_per_plant)
    } else {
        results <- lapply(v_usinas, function(iu) {
            t0 <- proc.time()[["elapsed"]]
            result <- tryCatch(
                do.call(fn, c(list(iu), extra_args)),
                error = function(e) {
                    lg$warn("Falha na usina %s: %s", iu, conditionMessage(e))
                    plant_error(iu, e)
                }
            )
            record_plant_timing(metrics, iu, round(proc.time()[["elapsed"]] - t0, 2L))
            result
        })
    }
    results
}

#' Processa Resultados de Treinamento por Usina
#'
#' Itera sobre os resultados de `run_plants`, atualiza status de proveniencia,
#' registra qualidade do modelo, escreve artefato enriquecido e, quando
#' `resume = TRUE`, persiste checkpoint apos cada usina.
#'
#' @param models lista de resultados de `run_plants` (um por usina)
#' @param v_usinas character vector de IDs de usinas, na mesma ordem que `models`
#' @param provenance environment de proveniencia criado por [create_provenance()]
#' @param metrics objeto de metricas criado por [create_metrics()]
#' @param lg objeto logger
#' @param resume logical, se `TRUE` chama [write_checkpoint()] apos cada usina
#' @param args lista com `output` (dir de checkpoint) e `artifact` (dir de artefatos)
#'     e demais campos necessarios para [build_model_artifact()]
#'
#' @return inteiro com o numero de usinas que falharam
#'
#' @keywords internal
tally_train_results <- function(models, v_usinas, provenance, metrics, lg, resume, args) {
    n_failed <- 0L
    n_total <- length(v_usinas)
    for (i in seq_along(v_usinas)) {
        iu <- v_usinas[i]
        result <- models[[i]]
        if (is_plant_error(result)) {
            update_plant_status(provenance, iu, "failed")
            n_failed <- n_failed + 1L
            lg$error("Usina %s falhou: %s", iu, result$error)
        } else {
            update_plant_status(provenance, iu, "completed")
            enriched <- build_model_artifact(iu, result, args)
            record_model_quality(metrics, iu, enriched$metadata)
            file_name <- paste0(iu, "_modelos_ajustados")
            pfvIO:::write_model_artifact(enriched, file_name, args$artifact)
        }
        if (resume) write_checkpoint(provenance, args$output)
        lg$info("Usina %s processada (%d/%d)", iu, i, n_total)
    }
    n_failed
}

#' Executa Pipeline de Treinamento
#'
#' Treina modelos de previsao de geracao solar para todas as usinas.
#' Registra proveniencia, metricas e relatorio de saude.
#'
#' Quando `resume = TRUE`, carrega checkpoint existente em `args$output`,
#' salta usinas ja completadas e persiste o estado apos cada usina.
#' Quando `parallel = TRUE`, usa `future.apply::future_lapply` para
#' processar usinas em paralelo.
#'
#' @param args lista com os seguintes campos:
#' \describe{
#'   \item{`input`}{caminho para os dados de entrada}
#'   \item{`output`}{diretorio de saida para proveniencia e checkpoints}
#'   \item{`artifact`}{diretorio para artefatos de modelo}
#'   \item{`ids_usinas`}{character vector de IDs de usinas}
#'   \item{`data_referencia`}{character, data de corte no formato `"YYYY-MM-DD"`}
#'   \item{`horizonte_dias`}{integer, horizonte de previsao em dias}
#'   \item{`modelos_NWP`}{character vector, modelos NWP a usar}
#'   \item{`modelos_previsao`}{lista de configs por modelo}
#'   \item{`parametros_periodo_geracao`}{lista de parametros do periodo solar}
#' }
#' @param parallel logical, se `TRUE` processa usinas em paralelo via `future`
#' @param resume logical, se `TRUE` retoma execucao a partir do ultimo checkpoint
#'
#' @return invisivel; modelos salvos em `args$artifact/{id_usina}_modelos_ajustados.rds`
#'
#' @export
train_main <- function(args, parallel = FALSE, resume = FALSE) {
    provenance <- create_provenance(args, "train", parallel)
    metrics <- create_metrics(provenance$run_id, "train")
    set_log_context(provenance$run_id, "train")
    lg <- get_pkg_logger()
    completed_plants <- character(0L)

    if (resume) {
        state <- load_train_resume_state(args, provenance)
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

    v_usinas <- setdiff(args$ids_usinas, completed_plants)

    if (length(v_usinas) == 0L) {
        finalize_provenance(provenance, "completed")
        cleanup_checkpoint(args$output)
        return(invisible(NULL))
    }

    conn <- conectamock_pfv(args$input)

    data_fim_treino <- as.Date(args$data_referencia)
    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP
    v_modelos_previsao <- vapply(args$modelos_previsao, function(x) x$tipo, character(1L))

    dt_usinas <- get_usinas(conn, id_usina = args$ids_usinas)

    data_set <- get_dataset(args, conn)
    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    extra_args <- list(
        dt_usinas = dt_usinas,
        dt_ger_obs = data_set_ger,
        dt_prev = data_set_met,
        v_modelos_nwp = v_modelos_nwp,
        v_horizonte = v_horizonte,
        v_modelos_previsao = v_modelos_previsao,
        parametros_modelo_previsao = args$modelos_previsao,
        parametros_periodo_geracao = args$parametros_periodo_geracao,
        data_fim_treino = data_fim_treino
    )

    if (parallel) {
        old_plan <- setup_parallel_plan()
        on.exit(reset_parallel_plan(old_plan), add = TRUE)
    }

    models <- run_plants(v_usinas, treina_usina, extra_args, parallel, metrics, lg)

    n_failed <- tally_train_results(models, v_usinas, provenance, metrics, lg, resume, args)

    final_status <- if (n_failed == length(v_usinas)) "failed" else "completed"
    finalize_provenance(provenance, final_status)
    if (resume) cleanup_checkpoint(args$output)
}

#' Treina Modelos para Uma Usina
#'
#' Treina modelos para todas as combinacoes NWP × horizonte × meia-hora × tipo.
#'
#' @param iu character, identificador da usina
#' @param dt_usinas data.table, cadastro de usinas
#' @param dt_ger_obs data.table, geracao observada
#' @param dt_prev list de data.tables, previsoes meteorologicas
#' @param v_modelos_nwp character, modelos NWP
#' @param v_horizonte character, horizontes de previsao
#' @param v_modelos_previsao character, tipos de modelos
#' @param parametros_modelo_previsao list, parametros por modelo
#' @param parametros_periodo_geracao list, parametros do periodo solar
#' @param data_fim_treino Date, limite para dados de treinamento
#'
#' @return list de pares `combinacao_ajuste` + `modelo`
#'
#' @keywords internal
treina_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev, v_modelos_nwp,
    v_horizonte, v_modelos_previsao, parametros_modelo_previsao,
    parametros_periodo_geracao, data_fim_treino
) {
    dad_usi <- dt_usinas[id_usina == iu]
    ger_usi <- dt_ger_obs[id_usina == iu]

    dt_prev <- lapply(dt_prev, function(dt) dt[id_modelo_nwp %in% v_modelos_nwp])
    dt_prev <- lapply(dt_prev, associa_nwp_usina, dt_usinas = dt_usinas)
    dt_prev <- lapply(dt_prev, interpola_previsao_nwp)
    dt_prev <- lapply(dt_prev, preenche_ausencia_previsao)
    dt_prev <- lapply(dt_prev, adicionar_passo_previsao)
    dt_prev <- lapply(dt_prev, function(dt) dt[id_usina == iu])

    ger_usi[, hora_min := format(data_hora_observacao, "%H:%M")]
    prev_met_usi <- lapply(dt_prev, function(dt) {
        dt[, hora_min := format(data_hora_previsao, "%H:%M")]
    })

    periodo_ger <- identifica_periodo_ger(
        dad_usi,
        ger_usi,
        fator_tol_ger = parametros_periodo_geracao$fator_tolerancia_limite_inferior_geracao,
        fator_tol_horas = parametros_periodo_geracao$percentual_dias_geracao
    )

    list_comb <- gera_combinacoes_modelo(v_modelos_nwp, v_horizonte, periodo_ger, v_modelos_previsao)

    lapply(list_comb, train_modelo,
        ger_usi = ger_usi,
        prev_met_usi = prev_met_usi,
        param_modelo_previsao = parametros_modelo_previsao,
        data_fim_treino = data_fim_treino
    )
}

#' Treina Modelo Individual
#'
#' Treina um modelo para uma combinacao especifica.
#'
#' @param l list com `id_modelo_nwp`, `horiz_prev`, `hora_min`, `modelo_prev`
#' @param ger_usi data.table, geracao observada
#' @param prev_met_usi list, previsoes meteorologicas
#' @param param_modelo_previsao list, parametros do modelo
#' @param data_fim_treino Date, limite para treinamento
#'
#' @return list com `combinacao_ajuste` e `modelo` ajustado
#'
#' @keywords internal
train_modelo <- function(l, ger_usi, prev_met_usi, param_modelo_previsao, data_fim_treino) {
    modelo_despacho <- l$modelo_prev
    modelo_parametros <- param_modelo_previsao[[modelo_despacho]]
    nlmod <- parse_train(modelo_parametros,
        pars = l,
        ger_usi = ger_usi,
        prev_met_usi = prev_met_usi,
        data_fim_treino = data_fim_treino
    )
    class(nlmod) <- modelo_despacho
    list(
        combinacao_ajuste = l,
        modelo = nlmod
    )
}

#' Metodo Generico para Treinamento de Modelo
#'
#' Funcao generica S3 que despacha para metodo especifico de treinamento.
#'
#' @param modelo_parametros list, parametros do modelo
#' @param ... Argumentos adicionais passados aos metodos especificos
#'
#' @return Objeto do modelo treinado
#'
#' @export
parse_train <- function(modelo_parametros, ...) UseMethod("parse_train")

#' Metodo Default para parse_train
#'
#' @param modelo_parametros list, parametros do modelo
#' @param ... Argumentos adicionais (ignorados)
#'
#' @return gera erro com tipo de modelo desconhecido
#'
#' @export
parse_train.default <- function(modelo_parametros, ...) {
    stop("Unknown model type for parse_train")
}

#' Treinamento de Modelo Fisico-Estimado
#'
#' Treina modelo fisico-estimado baseado em regressao linear.
#' Compara RLS (simples) vs RLM (multipla).
#'
#' @param modelo_parametros Lista com `n_dias_treino` e `amos_min`
#' @param ... Argumentos: `pars`, `ger_usi`, `prev_met_usi`, `data_fim_treino`
#'
#' @return Lista com `modelo_escolhido` ("RLS" ou "RLM"), `modelo_final`, `variaveis_usadas`
#'
#' @details
#' Expande janela de treinamento (ate +200 dias) se coeficientes forem
#' negativos ou dados insuficientes.
#'
#' @export
parse_train.fisico_estimado <- function(modelo_parametros, ...) {
    args <- list(...)
    pars <- args$pars
    ger_usi <- args$ger_usi
    prev_met_usi <- args$prev_met_usi
    data_fim_treino <- args$data_fim_treino
    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)
    dt_treino <- elimina_dados_invalidos(dt_treino)

    aumento <- 0
    repeat {
        dt_treino_filt <- seleciona_janela(
            dt_treino,
            data_ref = data_fim_treino,
            janela_dias_treinamento = modelo_parametros$n_dias_treino + aumento
        )
        setnames(dt_treino_filt, "valor", "ger_obs")

        dt_y <- data.table(ger_obs = rep(0, nrow(dt_treino_filt)))
        dt_x <- data.table(irrad_prev = rep(0, nrow(dt_treino_filt)))
        nlmod0 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x)
        nlmod0$coefficients[is.na(nlmod0$coefficients)] <- 0

        if (dados_suficientes(dt_treino_filt, num_min_dados = 10)) {
            dt_y <- dt_treino_filt[, .(ger_obs)]
            dt_x <- dt_treino_filt[, .(irrad_prev)]
            nlmod1 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod1$coefficients[is.na(nlmod1$coefficients)] <- 0

            dt_y <- dt_treino_filt[, .(ger_obs)]
            cols <- setdiff(names(dt_treino_filt)[-1], names(dt_y))
            dt_x <- dt_treino_filt[, .SD, .SDcols = cols]
            nlmod2 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod2$coefficients[is.na(nlmod2$coefficients)] <- 0

            if ((nlmod1$coefficients[2] > 0 & nlmod2$coefficients[2] > 0) | aumento == 200) {
                break
            }
            aumento <- aumento + 10
        } else {
            if (aumento == 200) {
                nlmod1 <- nlmod0
                nlmod2 <- nlmod0
                break
            }
            aumento <- aumento + 10
        }
    }

    erros <- calcula_erros_fisico_estimado(dt = dt_treino_filt[, -1], nlmod1, nlmod2)
    seleciona_modelo_fisico_estimado(nlmod1, nlmod2, erros)
}

#' Aplica Regressao Linear
#'
#' Ajusta modelo de regressao linear com fallback em caso de erro.
#'
#' @param dt_y data.table com variavel resposta
#' @param dt_x data.table com variaveis explicativas
#' @param nlmod0 Modelo de fallback (retornado em caso de erro); opcional
#'
#' @return Objeto \code{lm} com o modelo ajustado
#'
#' @keywords internal
aplica_regressao_linear <- function(dt_y, dt_x, nlmod0 = NULL) {
    dados <- cbind(dt_y, dt_x)
    resposta <- names(dt_y)
    preditoras <- names(dt_x)

    formula_obj <- as.formula(paste(resposta, "~", paste(preditoras, collapse = "+")))

    tryCatch(
        lm(formula_obj, data = dados),
        error = function(e) nlmod0
    )
}

#' Treinamento de Modelo ARIMAX
#'
#' Treina modelo ARIMA com variaveis exogenas usando auto.arima.
#'
#' @param modelo_parametros Lista com `n_dias_treino` e `amos_min`
#' @param ... Argumentos: `pars`, `ger_usi`, `prev_met_usi`, `data_fim_treino`
#'
#' @return Lista com `modelo_escolhido` ("ARIMA" ou "ARIMAX"), `modelo_final`
#'
#' @details
#' Seleciona melhor modelo por AICc + erro medio absoluto.
#' Retorna modelo dummy se dados insuficientes.
#'
#' @export
parse_train.arimax <- function(modelo_parametros, ...) {
    args <- list(...)
    pars <- args$pars
    ger_usi <- args$ger_usi
    prev_met_usi <- args$prev_met_usi
    data_fim_treino <- args$data_fim_treino

    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    dt_treino[, (names(dt_treino)) := lapply(.SD, function(x) fifelse(x == 999, NA, x))]

    dt_treino_filt <- seleciona_janela(
        dt_treino,
        data_fim_treino,
        janela_dias_treinamento = modelo_parametros$n_dias_treino
    )
    setnames(dt_treino_filt, "valor", "ger_obs")

    nlmod0 <- ajusta_dummy(dt_treino_filt)

    if (dados_suficientes(dt_treino_filt, num_min_dados = modelo_parametros$amos_min)) {
        norm_resultado <- normaliza_variaveis(dt_treino_filt)
        dt_treino_norm <- norm_resultado$dados
        stats_norm <- norm_resultado$stats

        cols_norm <- grep("_norm$", names(dt_treino_norm), value = TRUE)
        dt_treino_norm <- dt_treino_norm[, ..cols_norm]

        nlmod1 <- ajusta_arima(dt_treino_norm, nlmod0)
        nlmod2 <- ajusta_arimax(dt_treino_norm, nlmod0)

        erros <- calcula_erros(dt_treino_norm, nlmod1, nlmod2)
        seleciona_modelo(nlmod1, nlmod2, erros)
    } else {
        nlmod0
    }
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Cria Dataset para Treinamento
#'
#' Carrega dados para treinamento dos modelos.
#'
#' @param args Lista com `ids_usinas` e `modelos_NWP`
#' @param conn Conexao com banco de dados
#'
#' @return Lista com `ger_obs` e `irrad_prev`
#'
#' @keywords internal
get_dataset <- function(args, conn) {
    list(
        ger_obs = get_geracao_observada(conn, id_usina = args$ids_usinas),
        irrad_prev = get_irradiancia_prevista(conn, id_usina = args$ids_usinas, id_modelo_nwp = args$modelos_NWP)
    )
}

#' Preenche Lacunas de Rodadas NWP
#'
#' Identifica datas ausentes de rodadas NWP e preenche com NA.
#'
#' @param dt data.table com previsoes meteorologicas
#'
#' @return data.table com datas de rodada completas
#'
#' @keywords internal
preenche_ausencia_previsao <- function(dt) {
    datas_execucao <- dt[, .(data_hora_rodada = unique(data_hora_rodada)), by = .(id_modelo_nwp, id_usina)]
    datas_completas <- dt[,
        .(data_hora_rodada = seq.POSIXt(from = min(data_hora_rodada), to = max(data_hora_rodada), by = "days")),
        by = .(id_modelo_nwp, id_usina)
    ]
    dif <- fsetdiff(datas_completas, datas_execucao)

    if (length(dif) == 0) {
        return(dt)
    }

    passos_inicio <- dt[, min(data_hora_previsao) - data_hora_rodada, by = .(id_modelo_nwp, data_hora_rodada)]
    setnames(passos_inicio, "V1", "passos_inicio")
    passos_fim <- dt[, max(data_hora_previsao) - min(data_hora_previsao), by = .(id_modelo_nwp, data_hora_rodada)]
    setnames(passos_fim, "V1", "passos_fim")
    n_passos_previsao <- merge(passos_inicio, passos_fim, by = c("id_modelo_nwp", "data_hora_rodada"))
    n_passos_previsao <- n_passos_previsao[, lapply(.SD, unique),
        .SDcols = c("passos_inicio", "passos_fim"), by = id_modelo_nwp
    ]

    dif <- merge(dif, n_passos_previsao, by = "id_modelo_nwp")

    l_datas_faltantes <- lapply(split(dif, seq_len(nrow(dif))), cria_dt_auxiliar, dt_completo = dt)
    dt_datas_faltantes <- rbindlist(l_datas_faltantes)

    dt_prev_completo <- rbindlist(list(dt, dt_datas_faltantes))
    setorder(dt_prev_completo, id_modelo_nwp, id_usina, data_hora_rodada, data_hora_previsao)

    return(dt_prev_completo)
}

#' Cria data.table Auxiliar para Datas Ausentes
#'
#' Auxiliar para \code{preenche_ausencia_previsao}.
#'
#' @param dif data.table com uma linha (info sobre rodada ausente)
#' @param dt_completo data.table de referencia (latitude/longitude)
#'
#' @return data.table com estrutura identica, valores = NA
#'
#' @keywords internal
cria_dt_auxiliar <- function(dif, dt_completo) {
    latitude <- unique(dt_completo[id_modelo_nwp == dif$id_modelo_nwp & id_usina == dif$id_usina, latitude])
    longitude <- unique(dt_completo[id_modelo_nwp == dif$id_modelo_nwp & id_usina == dif$id_usina, longitude])
    data_hora_previsao_ini <- dif$data_hora_rodada + dif$passos_inicio
    data_hora_previsao_fim <- data_hora_previsao_ini + dif$passos_fim
    seq_data_hora_previsao <- seq.POSIXt(
        from = data_hora_previsao_ini, to = data_hora_previsao_fim,
        by = "30 min"
    )
    dt_auxiliar <- data.table(
        id_modelo_nwp = dif$id_modelo_nwp, id_usina = dif$id_usina, latitude = latitude,
        longitude = longitude, data_hora_rodada = dif$data_hora_rodada, data_hora_previsao = seq_data_hora_previsao,
        valor = NA
    )

    return(dt_auxiliar)
}

#' Elimina Dados Invalidos
#'
#' Remove registros com valores 999 ou NA (conversion, then filtering).
#'
#' @param dt data.table com series temporais
#'
#' @return data.table apenas com registros completos
#'
#' @keywords internal
elimina_dados_invalidos <- function(dt) {
    col_var <- setdiff(names(dt), "data_hora")
    dt[, (col_var) := lapply(.SD, function(x) fifelse(x == 999, NA, x)), .SDcols = col_var]
    dt_valido <- dt[complete.cases(dt[, .SD, .SDcols = col_var])]
}

#' Seleciona Janela Temporal de Treinamento
#'
#' Filtra dados para incluir apenas as N ultimas datas antes da data de referencia.
#'
#' @param dt data.table com coluna \code{data_hora}
#' @param data_ref Data de referencia (limite superior exclusivo)
#' @param janela_dias_treinamento Numero de dias a incluir na janela
#'
#' @return data.table filtrado com dados da janela especificada
#'
#' @details
#' Seleciona as \code{janela_dias_treinamento} datas mais recentes
#' anteriores a \code{data_ref}. Util para definir conjunto de treinamento
#' em validacao temporal.
#'
#' @keywords internal
seleciona_janela <- function(dt, data_ref, janela_dias_treinamento) {
    dt <- copy(dt)
    dt[, data := as.Date(data_hora)]

    dt <- dt[data < data_ref[1]]

    # considerar apenas as ultimas `janela_dias` datas
    ultimas_datas <- head(sort(unique(dt$data), decreasing = TRUE), janela_dias_treinamento)
    dt <- dt[data %in% ultimas_datas]

    dt[, data := NULL]

    return(dt)
}

#' Verifica Disponibilidade Minima de Dados
#'
#' Avalia se ha registros validos suficientes para ajuste do modelo.
#' Considera como invalidos: valores NA e valores zero.
#'
#' @param dt data.table com variaveis de interesse
#' @param num_min_dados Numero minimo de registros validos requeridos
#'
#' @return Logico: \code{TRUE} se dados suficientes, \code{FALSE} caso contrario
#'
#' @examples
#' \dontrun{
#' dt <- data.table(
#'     data_hora = 1:5,
#'     ger = c(10, NA, 12, 0, 15),
#'     irr = c(200, 210, NA, 205, 215)
#' )
#' dados_suficientes(dt, num_min_dados = 2)  # TRUE
#' dados_suficientes(dt, num_min_dados = 5)  # FALSE
#' }
#'
#' @keywords internal
dados_suficientes <- function(dt, num_min_dados) {
    dt <- copy(dt)
    col_var <- setdiff(names(dt), "data_hora")

    dt[, (col_var) := lapply(.SD, function(x) fifelse(x == 0, NA, x)), .SDcols = col_var]

    n_validos <- nrow(na.omit(dt[, ..col_var]))
    if (n_validos < num_min_dados) {
        return(FALSE)
    }
    TRUE
}

#' Cria Modelo ARIMA Dummy
#'
#' Cria um modelo ARIMA "neutro" para uso como fallback quando dados sao
#' insuficientes ou ajuste falha. O modelo retorna previsoes zero.
#'
#' @param dt data.table com dados de treinamento (usado apenas para dimensionamento)
#'
#' @return Objeto Arima ajustado a serie constante zero
#'
#' @keywords internal
ajusta_dummy <- function(dt) {
    auto.arima(rep(0, nrow(dt)), allowdrift = FALSE, allowmean = FALSE)
}

#' Normaliza Variaveis (Z-Score)
#'
#' Aplica normalizacao z-score a todas as variaveis numericas,
#' exceto \code{data_hora}.
#'
#' @param dt data.table com variaveis a normalizar
#'
#' @return Lista contendo:
#'   \describe{
#'     \item{dados}{data.table com colunas normalizadas (sufixo \code{_norm})}
#'     \item{stats}{data.table com estatisticas: variavel, med, sd}
#'   }
#'
#' @details
#' A normalizacao z-score transforma cada variavel X em:
#' \deqn{X_{norm} = (X - \bar{X}) / \sigma_X}
#'
#' As estatisticas sao preservadas para posterior desnormalizacao
#' das previsoes.
#'
#' @seealso \code{\link{desnormaliza_variaveis}}
#'
#' @keywords internal
normaliza_variaveis <- function(dt) {
    col_excluida <- "data_hora"

    var_norm <- setdiff(names(dt), col_excluida)

    stats <- dt[, lapply(.SD, function(x) {
        c(med = mean(x, na.rm = TRUE), desv = sd(x, na.rm = TRUE))
    }), .SDcols = var_norm]

    stats <- transpose(stats, keep.names = "variavel")

    setnames(stats, c("variavel", "med", "sd"))

    dt[, paste0(var_norm, "_norm") := lapply(var_norm, function(v) {
        (get(v) - stats[variavel == v, med]) / stats[variavel == v, sd]
    })]

    list(dados = dt, stats = stats)
}

#' Ajusta modelo ARIMA simples
#'
#' @param dt data.table com variavel \code{ger_obs_norm}.
#' @param nlmod0 modelo dummy utilizado em caso de erro.
#'
#' @return modelo ajustado do tipo \code{Arima}.
ajusta_arima <- function(dt, nlmod0) {
    y <- dt$ger_obs_norm
    y_validos <- y[!is.na(y)]

    modelo <- tryCatch(
        auto.arima(y_validos, allowdrift = FALSE, allowmean = FALSE),
        error = function(e) nlmod0
    )
}

#' Ajusta modelo ARIMAX com variaveis exogenas
#'
#' @param dt data.table com variavel \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}.
#' @param nlmod0 modelo dummy utilizado em caso de erro.
#'
#' @return modelo ajustado do tipo \code{Arima}.
ajusta_arimax <- function(dt, nlmod0) {
    var_exog <- setdiff(names(dt), "ger_obs_norm")
    dt_valido <- dt[complete.cases(dt[, c("ger_obs_norm", ..var_exog)])]

    xreg <- dt_valido[, ..var_exog]

    modelo <- tryCatch(
        auto.arima(dt_valido$ger_obs_norm,
            xreg = as.matrix(xreg),
            allowdrift = FALSE, allowmean = FALSE
        ),
        error = function(e) nlmod0
    )
}

#' Calcula erros medios in-sample dos modelos
#'
#' @param dt data.table com variavel \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}..
#' @param nlmod1 Modelo ARIMA.
#' @param nlmod2 Modelo ARIMAX.
#'
#' @return Lista com erro medio de cada modelo.
calcula_erros <- function(dt, nlmod1, nlmod2) {
    prev1 <- as.numeric(fitted(nlmod1))
    prev2 <- as.numeric(fitted(nlmod2))

    # filtra valores de geracao nao-NA
    y <- dt$ger_obs_norm
    y_validos <- y[!is.na(y)]

    # filtra valores nao-NA coincidentes de geracao e variaveis exogenas
    var_exog <- setdiff(names(dt), "ger_obs_norm")
    dt_valido <- dt[complete.cases(dt[, c("ger_obs_norm", ..var_exog)])]

    list(
        erro1 = mean(abs(y_validos - prev1), na.rm = TRUE),
        erro2 = mean(abs(dt_valido$ger_obs_norm - prev2), na.rm = TRUE)
    )
}

#' Calcula erros medios in-sample dos modelos
#'
#' @param dt data.table com variavel \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}..
#' @param nlmod1 Modelo RLS
#' @param nlmod2 Modelo RLM
#'
#' @return Lista com erro medio de cada modelo.
calcula_erros_fisico_estimado <- function(dt, nlmod1, nlmod2) {
    prev1 <- as.numeric(fitted(nlmod1))
    prev2 <- as.numeric(fitted(nlmod2))

    dt_valido <- dt[complete.cases(dt)]

    l_erros <- list(
        erro1 = mean(abs(dt_valido$ger_obs - prev1), na.rm = TRUE),
        erro2 = mean(abs(dt_valido$ger_obs - prev2), na.rm = TRUE)
    )
    return(l_erros)
}

#' Seleciona o melhor modelo entre ARIMA e ARIMAX
#'
#' @param nlmod1 Modelo ARIMA.
#' @param nlmod2 Modelo ARIMAX.
#' @param erros Lista com erros medios dos modelos.
#'
#' @return Lista com modelo escolhido.
seleciona_modelo <- function(nlmod1, nlmod2, erros) {
    escolhe_arima <- (nlmod1$aicc <= nlmod2$aicc) && (erros$erro1 <= erros$erro2)
    list(
        modelo_escolhido = if (escolhe_arima) "ARIMA" else "ARIMAX",
        modelo_final = if (escolhe_arima) nlmod1 else nlmod2
    )
}

#' Seleciona o melhor modelo entre Regressao Linear Simples (RLS) e Regressao Linear Multipla (RLM)
#'
#' @param nlmod1 Modelo RLS
#' @param nlmod2 Modelo RLM
#' @param erros Lista com erros medios dos modelos.
#'
#' @return Lista com modelo escolhido.
seleciona_modelo_fisico_estimado <- function(nlmod1, nlmod2, erros) {
    escolhe_fe <- (erros$erro1 <= erros$erro2)
    list(
        modelo_escolhido = if (escolhe_fe) "RLS" else "RLM",
        modelo_final = if (escolhe_fe) nlmod1 else nlmod2,
        variaveis_usadas = if (escolhe_fe) names(nlmod1$model) else names(nlmod2$model) # ISABELA - ARRUMAR
    )
}
