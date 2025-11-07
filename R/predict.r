mod_aju <- readRDS(paste(args$output, "BAUFI1_arima_ajustado.rds", sep = "/"))

predict_main <- function(args) {
    conn <- conectamock_pfv(args$input)

    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP

    # define horizonte de previsao
    data_prev <- define_hor_prev(args$data_referencia, v_horizonte)

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn, dias = 180)

    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    data_set_met <- lapply(data_set_met, associa_nwp_usina, dt_usinas)

    data_set_met <- lapply(data_set_met, interpola_previsao_nwp)
    data_set_met <- lapply(data_set_met, adicionar_passo_previsao)

    prev <- lapply(v_usinas, predict_usina,
        dt_usinas = dt_usinas,
        dt_ger_obs = data_set_ger,
        dt_prev = data_set_met,
        fator_tolerancia_geracao = args$fator_tolerancia_limite_inferior_geracao,
        fator_tolerancia_horas = args$percentual_dias_geracao,
        v_modelos_nwp = v_modelos_nwp,
        v_horizonte = v_horizonte,
        data_prev = data_prev
    )
}

predict_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev,
    fator_tolerancia_geracao, fator_tolerancia_horas,
    v_modelos_nwp, v_horizonte, data_prev) {
    # Filtra os dados de geracao e meteorologicos referentes a usina atual
    dad_usi <- dt_usinas[id_usina == iu]
    ger_usi <- dt_ger_obs[id_usina == iu]
    prev_met_usi <- lapply(dt_prev, function(dt) dt[id_usina == iu])

    ger_usi[, hora_min := format(data_hora_observacao, "%H:%M")]
    prev_met_usi <- lapply(dt_prev, function(dt) {
        dt[, hora_min := format(data_hora_previsao, "%H:%M")]
    })

    # identificacao das semi-horas com geracao solar
    periodo_ger <- identifica_periodo_ger(dad_usi, ger_usi, fator_tolerancia_geracao, fator_tolerancia_horas)

    # gera lista com as combinacoes nwp x passo de previsao x meia-hora
    list_comb <- gera_combinacoes_modelo(v_modelos_nwp, v_horizonte, periodo_ger)

    # treina o arima
    janela_dias_modelo <- 365

    ger_prev <- lapply(seq_along(list_comb), function(i) {
        pars <- list_comb[[i]]

        idx <- which(sapply(mod_aju, function(x) {
            x$combinacao_ajuste$id_modelo_nwp == pars$id_modelo_nwp &
                x$combinacao_ajuste$hora_min == pars$hora_min &
                x$combinacao_ajuste$horiz_prev == pars$horiz_prev
        }))

        if (length(idx) == 0) {
            return(NULL)
        }

        prev <- predict_arima(mod_aju[[idx]]$modelo, pars, data_prev, ger_usi, prev_met_usi, janela_dias_modelo)
        list(
            combinacao_ajuste = pars,
            prev = prev
        )
    })
}

predict_arima <- function(mod_aju_comb, pars, data_prev, ger_usi, prev_met_usi, janela_dias) {
    dt_filt <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    # Separa os dados de geracao e meteorologicos em treino e previsao -
    # PODE TRANSFORMAR EM FUNCAO DEPOIS
    dt_treino_filt <- dt_filt[as.Date(data_hora) < data_prev[1]]
    pos_hor <- which(v_horizonte == pars$horiz_prev)
    dt_prev_filt <- dt_filt[as.Date(data_hora) == data_prev[pos_hor]]
    setnames(dt_prev_filt, "valor", "ger_obs")

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(dt_treino_filt, janela_dias_treinamento = 300)
    setnames(dt_treino_filt, "valor", "ger_obs")

    # avalia numero de conjuntos ger x irr x temp x umid
    if (dados_suficientes(dt_treino_filt, num_min_dados = 5) == TRUE) {
        # diferencia treinamento e previsao

        # normaliza as variaveis necessarias para o ajuste
        norm_resultado <- normaliza_variaveis(dt_treino_filt)
        dt_treino_norm <- norm_resultado$dados
        stats_norm <- norm_resultado$stats

        cols_norm <- grep("_norm$", names(dt_treino_norm), value = TRUE)
        dt_treino_norm <- dt_treino_norm[, ..cols_norm]

        # normaliza dados previstos com base nas estatisticas de treino
        dt_prev_norm <- copy(dt_prev_filt)
        for (i in seq_len(nrow(stats_norm))) {
            var <- stats_norm$variavel[i]
            if (var %in% names(dt_prev_norm)) {
                media <- stats_norm$med[i]
                desvio <- stats_norm$sd[i]
                dt_prev_norm[, paste0(var, "_norm") := (get(var) - media) / desvio]
            }
        }

        var_exog <- setdiff(names(dt_prev_norm), "ger_obs_norm")

        # recalibra modelo Arima/Arimax e realiza previsao
        mod_selec <- mod_aju_comb$modelo_escolhido
        if (mod_selec == "ARIMAX") {
            nlmod1 <- recalibra_arimax(dt_treino_norm, mod_aju_comb$modelo_final)

            xreg_fut <- as.matrix(dt_prev_norm[, ..var_exog])
            prev_norm <- forecast(nlmod1, xreg = xreg_fut, h = nrow(dt_prev_norm))$mean
        } else {
            nlmod1 <- recalibra_arima(dt_treino_norm, mod_aju_comb$modelo_final)

            prev_norm <- forecast(nlmod1, h = nrow(dt_prev_norm))$mean
        }

        # monta data.table temporario para desnormalizar
        dt_prev_out <- data.table(ger_obs_norm = prev_norm)
        # desnormaliza previsao
        dt_prev_out <- desnormaliza_variaveis(dt_prev_out, stats_norm)

        prev_final <- dt_prev_out$ger_obs
    }
}

#' Recalibra modelo ARIMA simples
#'
#' @param dt data.table com variavel \code{ger_obs_norm}.
#' @param nlmod0 modelo ajustado pela etapa de treinamento.
#'
#' @return modelo recalibrado do tipo \code{Arima}.
recalibra_arima <- function(dt, nlmod0) {
    y <- dt$ger_obs_norm
    y_validos <- y[!is.na(y)]

    nlmod1 <- Arima(y_validos, model = nlmod0)
}

#' Recalibra modelo ARIMAX com variaveis exogenas
#'
#' @param dt data.table com variavel \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}.
#' @param nlmod0 modelo ajustado pela etapa de treinamento.
#'
#' @return modelo recalibrado do tipo \code{Arimax}.
recalibra_arimax <- function(dt, nlmod0) {
    var_exog <- setdiff(names(dt), "ger_obs_norm")
    dt_valido <- dt[complete.cases(dt[, c("ger_obs_norm", ..var_exog)])]

    xreg <- dt_valido[, ..var_exog]

    Arima(dt_valido$ger_obs_norm,
        xreg = as.matrix(xreg),
        model = nlmod0
    )
}

#' Desnormaliza variaveis com base em estatisticas de normalizacao
#'
#' @param dt data.table com colunas normalizadas (ex: *_norm)
#' @param stats_norm data.table com colunas: variavel, med, sd
#'
#' @return data.table com colunas desnormalizadas (sem sufixo "_norm")
#' @examples
#' dt_real <- desnormaliza_variaveis(dt_norm, stats_norm)
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
