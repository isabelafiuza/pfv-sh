train_main <- function(args) { # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO
    conn <- conectamock_pfv(args$input)
    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias
    v_modelos_nwp <- args$modelos_NWP

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn, dias = 180)

    data_set_ger <- data_set$ger_obs
    data_set_met <- data_set[names(data_set) != "ger_obs"]

    data_set_met <- lapply(data_set_met, associa_nwp_usina, dt_usinas)

    data_set_met <- lapply(data_set_met, interpola_previsao_nwp)
    data_set_met <- lapply(data_set_met, adicionar_passo_previsao)
    # data_set$irrad_prev <- compatibiliza_datas(data_set)

    artefatos <- lapply(v_usinas, treina_usina,
        dt_usinas = dt_usinas,
        dt_ger_obs = data_set_ger,
        dt_prev = data_set_met,
        fator_tolerancia_geracao = args$fator_tolerancia_limite_inferior_geracao,
        fator_tolerancia_horas = args$percentual_dias_geracao
    )
}

treina_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev,
    fator_tolerancia_geracao, fator_tolerancia_horas) {
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
    mod_aju <- lapply(list_comb, train_arima, ger_usi, prev_met_usi, janela_dias_modelo)
}

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
train_fisico_estimado <- function(data_set) {
    data_set <- lapply(data_set, function(x) x[valor == 0, valor := NA])
    data_set <- mapply(renomeia_colunas, data_set, "data_hora_observacao", "data_hora")
    data_set <- mapply(renomeia_colunas, data_set, "data_hora_previsao", "data_hora")
    data_set <- lapply(data_set, function(x) x[, hora := format(data_hora, "%H:%M")])
    vetor_horas <- unique(data_set$ger_obs$hora) # ISABELA - DEPOIS SUBSTITUIR PELO PERIODO DE GERACAO IDENTIFICADO PARA CADA USINA


    for (h in vetor_horas) {
        list_y <- data_set[names(data_set) == "ger_obs"] # ISABELA - VERIFICAR SE TEM FORMA MELHOR DE FAZER
        list_x <- data_set[names(data_set) != "ger_obs"]

        list_y <- lapply(list_y, function(x) x[hora == h])
        list_x <- lapply(list_x, function(x) x[hora == h])

        dt_y <- as.data.table(sapply(list_y, function(x) x$valor))
        dt_x <- as.data.table(sapply(list_x, function(x) x$valor))

        modelo <- aplica_regressao_linear(dt_y, dt_x)
    }
}

#' Aplica Regressao Linear
#'
#' Funcao que aplica regressao linear a um conjunto de dados e salva o
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

train_arima <- function(pars, ger_usi, prev_met_usi, janela_dias) {
    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(dt_treino, janela_dias_treinamento = 300)
    setnames(dt_treino_filt, "valor", "ger_obs")

    # avalia numero de conjuntos ger x irr x temp x umid
    if (dados_suficientes(dt_treino_filt, num_min_dados = 5) == TRUE) {
        # ajusta modelo dummy
        nlmod0 <- ajusta_dummy(dt_treino_filt)

        # normaliza as variaveis necessarias para o ajuste
        norm_resultado <- normaliza_variaveis(dt_treino_filt)
        dt_treino_norm <- norm_resultado$dados
        stats_norm <- norm_resultado$stats

        # ajusta ARIMA
        nlmod1 <- ajusta_arima(dt_treino_norm, nlmod0)
    }
    # proximas funcoes sao aplicadas somente se a condicao for satisfeita
    # numero minimo pode ser parametro de entrada






    # cria diferenciacao para alguns horarios dias para que o ajuste
    # seja apenas ger x irr

    # ajusta arimax

    # calcular erro medio in-sample

    # seleciona do modelo com base no desvio e aicc

    # retorna modelos
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
get_dataset <- function(args, conn, dias) {
    janela <- paste0(args$data_referencia - dias, "/", args$data_referencia)

    ger_obs <- get_geracao_observada(conn, id_usina = args$ids_usinas)
    irrad_prev <- get_irradiancia_prevista(conn, id_usina = args$ids_usinas, id_modelo_nwp = args$modelos_NWP)

    out <- list(ger_obs, irrad_prev)
    names(out) <- c("ger_obs", "irrad_prev")

    return(out)
}

preenche_lacunas_previsao <- function(data_set) { # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO

    datas_rodadas <- unique(data_set$data_hora_rodada)
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

seleciona_janela <- function(dt, janela_dias_treinamento) {
    dt <- copy(dt)
    dt[, data := as.Date(data_hora)]

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
#' @param dt data.table com variavel \code{ger_norm}.
#' @param nlmod0 modelo dummy utilizado em caso de erro.
#'
#' @return modelo ajustado do tipo \code{Arima}.
ajusta_arima <- function(dt, nlmod0) {
    tryCatch(
        auto.arima(dt$ger_N, allowdrift = FALSE, allowmean = FALSE),
        error = function(e) nlmod0
    )
}
