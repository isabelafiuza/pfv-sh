train_main <- function(args) {
    conn <- conectamock_pfv(args$input)

    data_fim_treino <- as.Date(args$data_referencia)

    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP
    v_modelos_previsao <- sapply(args$modelos_previsao, function(x) x$tipo)

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn)
    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    artefatos <- lapply(v_usinas, treina_usina,
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
}

treina_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev, v_modelos_nwp,
    v_horizonte, v_modelos_previsao, parametros_modelo_previsao,
    parametros_periodo_geracao, data_fim_treino) {
    # Filtra os dados referentes a usina atual
    dad_usi <- dt_usinas[id_usina == iu]
    ger_usi <- dt_ger_obs[id_usina == iu]

    # Associa os dados NWP a usina, adiciona o passo de previsao e filtra usina atual
    dt_prev <- lapply(dt_prev, associa_nwp_usina, dt_usinas = dt_usinas)
    dt_prev <- lapply(dt_prev, interpola_previsao_nwp)
    # dt_prev <- lapply(dt_prev, preenche_ausencia_previsao)
    dt_prev <- lapply(dt_prev, adicionar_passo_previsao)
    dt_prev <- lapply(dt_prev, function(dt) dt[id_usina == iu])

    ger_usi[, hora_min := format(data_hora_observacao, "%H:%M")]
    prev_met_usi <- lapply(dt_prev, function(dt) {
        dt[, hora_min := format(data_hora_previsao, "%H:%M")]
    })

    # identificacao das semi-horas com geracao solar
    periodo_ger <- identifica_periodo_ger(dad_usi, ger_usi, fator_tol_ger = parametros_periodo_geracao$fator_tolerancia_limite_inferior_geracao, fator_tol_horas = parametros_periodo_geracao$percentual_dias_geracao)

    # gera lista com as combinacoes nwp x passo de previsao x meia-hora x modelos de previsao
    list_comb <- gera_combinacoes_modelo(v_modelos_nwp, v_horizonte, periodo_ger, v_modelos_previsao[1])

    # treina modelo
    mod_aju <- lapply(list_comb, train_modelo,
        ger_usi = ger_usi,
        prev_met_usi = prev_met_usi,
        param_modelo_previsao = parametros_modelo_previsao,
        data_fim_treino = data_fim_treino
    )
    # TODO - alterar para salvar todos os modelos
    saveRDS(mod_aju, file = paste(args$output, paste0(iu, "_modelos_ajustados.rds"), sep = "/"))
}

train_modelo <- function(l, ger_usi, prev_met_usi, param_modelo_previsao, data_fim_treino) {
    modelo_despacho <- l$modelo_prev
    modelo_parametros <- param_modelo_previsao[[modelo_despacho]]
    nlmod <- parse_train(modelo_parametros,
        pars = l,
        ger_usi = ger_usi,
        prev_met_usi = prev_met_usi,
        data_fim_treino = data_fim_treino) 
    class(nlmod) <- modelo_despacho
    list(
        combinacao_ajuste = l,
        modelo = nlmod
    )
    
}

parse_train <- function(modelo_parametros, ...) UseMethod("parse_train")

#' Treinamento Usando O Metodo Fisico Estimado
#'
#' Realiza treinamento do modelo fisico estimado e salva modelo para uso futuro
#'
#' @param data_set lista contendo o subset dos dados utilizados para treinamento do modelo.
#' O subset ja deve conter os dados do periodo adequado para o treinamento. Cada item da lista
#' corresponde a uma variavel usada no treinamento, tanto variavel resposta quanto explicativas.
#'
#' @return modelos ajustados
#'
parse_train.fisico_estimado <- function(modelo_parametros, pars,
                                        ger_usi, prev_met_usi,
                                        data_fim_treino) {
    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(dt_treino, janela_dias_treinamento = modelo_parametros$n_dias_treino)
    setnames(dt_treino_filt, "valor", "ger_obs")

    dt_y <- dt_treino_filt$ger_obs
    dt_x <- dt_treino_filt[names(dt_treino_filt) != "ger_obs"]

    modelo <- aplica_regressao_linear(dt_y, dt_x)
}

#' Aplica Regressao Linear
#'
#' Funcao que aplica regressao linear a um conjunto de dados
#'
#' @param dt_y ´data.table´ contendo a variavel resposta. Exemplo: geracao observada
#' @param dt_x ´data.table´ contendo a(s) variavel(is) explicativas. Exemplo: irradiancia, umidade e temperatura
#' As observacoes contidas nos data.tables contendo as variaveis ja devem estar em posicoes compativeis
#'
#' @return modelos ajustados
#'
aplica_regressao_linear <- function(dt_y, dt_x) {
    dados <- cbind(dt_y, dt_x)
    resposta <- names(dt_y)
    preditoras <- names(dt_x)

    formula <- paste(resposta, "~", paste(preditoras, collapse = "+"))
    formula_objeto <- as.formula(formula)

    modelo <- lm(formula_objeto, data = dados)
    return(modelo)
}

parse_train.arimax <- function(modelo_parametros, pars,
                               ger_usi, prev_met_usi, data_fim_treino) {
    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    # FUNCAO QUE CHECA OS DADOS DEVE FAZER ISSO
    dt_treino[, (names(dt_treino)) := lapply(.SD, function(x) fifelse(x == 999, NA, x))]

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(dt_treino, data_fim_treino, janela_dias_treinamento = modelo_parametros$n_dias_treino)
    setnames(dt_treino_filt, "valor", "ger_obs")

    # ajusta modelo dummy
    nlmod0 <- ajusta_dummy(dt_treino_filt)

    # avalia numero de conjuntos ger x irr x temp x umid
    if (dados_suficientes(dt_treino_filt, num_min_dados = modelo_parametros$amos_min) == TRUE) {
        # normaliza as variaveis necessarias para o ajuste
        norm_resultado <- normaliza_variaveis(dt_treino_filt)
        dt_treino_norm <- norm_resultado$dados
        stats_norm <- norm_resultado$stats

        cols_norm <- grep("_norm$", names(dt_treino_norm), value = TRUE)
        dt_treino_norm <- dt_treino_norm[, ..cols_norm]

        # ajusta ARIMA
        nlmod1 <- ajusta_arima(dt_treino_norm, nlmod0)

        # ajusta ARIMAX
        nlmod2 <- ajusta_arimax(dt_treino_norm, nlmod0)

        # calcular erro medio in-sample
        erros <- calcula_erros(dt_treino_norm, nlmod1, nlmod2)

        # seleciona do modelo com base no desvio e aicc
        selecao <- seleciona_modelo(nlmod1, nlmod2, erros)
    } else {
        return(nlmod0)
    }
    # cria diferenciacao para alguns horarios dias para que o ajuste
    # seja apenas ger x irr
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Cria Dataset Dos Dados Para Treino dos Modelos
#'
#' Cria dataset dos dados usados no treinamento dos modelos de previsao de acordo configuracao especificada
#'
#' @param args Lista de argumentos necessarios para criacao dos datasets
#' @param conn Objeto de conexao com banco de dados
#'
#' @return lista contendo o dataset
#'
get_dataset <- function(args, conn) {
    ger_obs <- get_geracao_observada(conn, id_usina = args$ids_usinas)
    irrad_prev <- get_irradiancia_prevista(conn, id_usina = args$ids_usinas, id_modelo_nwp = args$modelos_NWP)

    out <- list(ger_obs, irrad_prev)
    names(out) <- c("ger_obs", "irrad_prev")

    return(out)
}

#' Preenche Lacunas de Previsao
#'
#' Identifica dias em que nao ha rodada do modelo meteorologico na base de dados e preenche as previsoes
#' referentes a essa execucao ausente com NA.
#' A funcao avalia a coluna 'data_hora_rodada' de cada modelo meteorologico e, sendo verificada a
#' ausencia de alguma data, inclui dados NA para compatibilizacao das series temporais.
#'
#' @param dt 'data.table' contendo as previsoes meteorologicas
#'
#' @return 'data.table' com a coluna
#'
preenche_ausencia_previsao <- function(dt) { # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO

    datas_execucao <- dt[, .(data_hora_rodada = unique(dt$data_hora_rodada)), by = .(id_modelo_nwp, id_usina)]
    datas_completas <- dt[, .(data_hora_rodada = seq.POSIXt(from = min(data_hora_rodada), to = max(data_hora_rodada), by = "days")), by = .(id_modelo_nwp, id_usina)]

    dt_data_inicio <- dt[, .(data_inicio = min(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_data_fim <- dt[, .(data_fim = max(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_nwp", "id_usina", "data_hora_rodada"))
    dt_datas_inicio_fim_completas <- merge(datas_completas, dt_datas_inicio_fim, by = c("id_modelo_nwp", "id_usina", "data_hora_rodada"), all.x = TRUE)

    # AVALIAR ESSA LOGICA
    dt_datas_inicio_fim_completas[, data_inicio := nafill(seq.POSIXt(from = data_hora_rodada, length = 2, by = "days")[-1]), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim_completas[, data_fim := nafill(), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]

    cols_propagar <- setdiff(names(dt_datas_inicio_fim), names(datas_completas))
    for (col in cols_propagar) {
        # if
        dt_datas_inicio_fim_completas[, (col) := nafill(nafill(get(col), type = "locf"), type = "nocb"),
            by = .(id_modelo_nwp, id_usina, data_hora_rodada)
        ]
    }
}

cria_dt_dummy <- function(datas_execucao_dummy, dt_original) {
    dt_dummy <- dt[id_modelo_nwp == datas_execucao_dummy$id_modelo_nwp &
        id_usina == datas_execucao_dummy$id_usina]

    return(dt_dummy)
}

cria_dt_completo <- function(datas_faltantes, dt_original) {
    dt_auxiliar <- dt_original[id_modelo_nwp == datas_faltantes$id_modelo_nwp &
        id_usina == datas_faltantes$id_usina]
}

compatibiliza_datas <- function(data_set) { # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO

    ger_obs <- data_set$ger_obs
    ger_obs_colorder <- names(ger_obs)

    irrad_prev <- data_set$irrad_prev
    irrad_prev_colorder <- names(irrad_prev)

    data_ini <- max(min(ger_obs$data_hora_observacao), min(irrad_prev$data_hora_rodada))
    data_fim <- min(max(ger_obs$data_hora_observacao), max(irrad_prev$data_hora_previsao))
    janela <- seq.POSIXt(from = data_ini, to = data_fim, by = "30 min")

    dt_janela <- data.table(data_hora_observacao = janela)
    ger_obs <- merge(ger_obs, dt_janela, by = "data_hora_observacao", all.y = TRUE)
    setcolorder(ger_obs, ger_obs_colorder)

    names(dt_janela) <- "data_hora_previsao"
    irrad_prev <- merge(irrad_prev, dt_janela, by = "data_hora_previsao", all.y = TRUE)
    setorder(irrad_prev, data_hora_rodada, data_hora_previsao)
    setcolorder(irrad_prev, irrad_prev_colorder)

    data_set$ger_obs <- ger_obs
    data_set$irrad_prev <- irrad_prev

    return(data_set)
}

renomeia_colunas <- function(dt, nome_atual, nome_novo) {
    names(dt)[names(dt) == nome_atual] <- nome_novo
    return(dt)
}

#' Seleciona janela dos dados para treinamento
#'
#' @param dt `data.table` com data_hora, geracao, variaveis meteorologicas
#' @param janela_dias_treinamento numero de dias utilizados
#' para treinamento
#'
#' @return subset do data.table filtrado

seleciona_janela <- function(dt, data_ref, janela_dias_treinamento) {
    dt <- copy(dt)
    dt[, data := as.Date(data_hora)]

    dt[data < data_ref]

    # considerar apenas as ultimas `janela_dias` datas
    ultimas_datas <- head(sort(unique(dt$data), decreasing = TRUE), janela_dias_treinamento)
    dt <- dt[data %in% ultimas_datas]

    dt[, data := NULL]

    return(dt)
}

#' Verifica se ha dados suficientes para o ajuste
#'
#' @param dt `data.table` com data_hora, geracao, irradiancia,
#' temperatura e umidade
#' @param num_min_dados numero minimo de registros validos simultaneos
#'
#' @return TRUE se houver dados validos, FALSE caso contrario
#'
#' @examples
#' dt <- data.table(
#'     data_hora = 1:5,
#'     ger = c(10, NA, 12, 0, 15),
#'     irr = c(200, 210, NA, 205, 215)
#' )
#' dados_suficientes(dt, num_min_dados = 2)
dados_suficientes <- function(dt, num_min_dados) {
    col_var <- setdiff(names(dt), "data_hora")

    dt[, (col_var) := lapply(.SD, function(x) fifelse(x == 0, NA, x)), .SDcols = col_var]

    n_validos <- nrow(na.omit(dt[, ..col_var]))
    if (n_validos < num_min_dados) {
        return(FALSE)
    }
    TRUE
}

#' Cria um modelo ARIMA dummy
#'
#' @param dt  `data.table` com data_hora, geracao, irradiancia,
#' temperatura e umidade do periodo de treinamento selecionado
#' @return modelo ARIMA neutro

ajusta_dummy <- function(dt) {
    auto.arima(rep(0, nrow(dt)), allowdrift = FALSE, allowmean = FALSE)
}

#' Normaliza variaveis de entrada
#'
#' @param dt `data.table` com data_hora, geracao, irradiancia,
#' temperatura e umidade do periodo de treinamento selecionado
#'
#' @return lista com:
#' \itemize {
#'  \item \code{dados} - data.table com variaveis normalizadas
#'  \item \code{stats} - medias e desvios padroes de cada variavel
#' }

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
#' @param dt data.table com variável \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}..
#' @param nlmod1 Modelo ARIMA.
#' @param nlmod2 Modelo ARIMAX.
#'
#' @return Lista com erro médio de cada modelo.
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
