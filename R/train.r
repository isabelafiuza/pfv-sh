#' Executa Pipeline de Treinamento
#'
#' Ponto de entrada principal para o treinamento de modelos de previsao de geracao
#' solar fotovoltaica. Processa todas as usinas especificadas na configuracao.
#'
#' @param args Lista de argumentos de configuracao contendo:
#'   \describe{
#'     \item{input}{Caminho para diretorio de dados de entrada}
#'     \item{output}{Caminho para diretorio de saida dos artefatos}
#'     \item{ids_usinas}{Vetor de IDs das usinas a processar}
#'     \item{data_referencia}{Data de referencia para treinamento}
#'     \item{horizonte_dias}{Vetor de horizontes (e.g., c("D+0", "D+1"))}
#'     \item{modelos_NWP}{Vetor de modelos NWP a utilizar}
#'     \item{modelos_previsao}{Lista de configuracoes dos modelos de previsao}
#'     \item{parametros_periodo_geracao}{Parametros para identificacao de periodo solar}
#'   }
#'
#' @return Lista de artefatos de modelos treinados (invisivel). Os modelos tambem
#'   sao salvos em arquivos RDS no diretorio de saida.
#'
#' @details
#' O pipeline de treinamento executa os seguintes passos para cada usina:
#' \enumerate{
#'   \item Carrega dados de geracao observada e previsoes meteorologicas
#'   \item Associa previsoes NWP as coordenadas da usina
#'   \item Interpola dados para resolucao semi-horaria
#'   \item Identifica periodos com geracao solar
#'   \item Gera combinacoes (NWP × horizonte × meia-hora × modelo)
#'   \item Treina modelo para cada combinacao
#'   \item Salva artefatos em \code{{output}/{id_usina}_modelos_ajustados.rds}
#' }
#'
#' @seealso
#' \code{\link{treina_usina}} para treinamento de uma usina especifica
#' \code{\link{parse_train}} para dispatch de treinamento por tipo de modelo
#'
#' @export
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
        local_escrita = args$out,
        data_fim_treino = data_fim_treino
    )
}

#' Treina Modelos para Uma Usina
#'
#' Executa o pipeline completo de treinamento para uma usina especifica,
#' gerando modelos para todas as combinacoes de NWP, horizonte e meia-hora.
#'
#' @param iu Identificador da usina (character)
#' @param dt_usinas data.table com cadastro de usinas
#' @param dt_ger_obs data.table com geracao observada
#' @param dt_prev Lista de data.tables com previsoes meteorologicas
#' @param v_modelos_nwp Vetor de modelos NWP a utilizar
#' @param v_horizonte Vetor de horizontes de previsao
#' @param v_modelos_previsao Vetor de tipos de modelos de previsao
#' @param parametros_modelo_previsao Lista com parametros de cada modelo
#' @param parametros_periodo_geracao Lista com parametros de identificacao do periodo solar
#' @param data_fim_treino Data limite para dados de treinamento
#'
#' @return Salva artefatos RDS no diretorio de saida (efeito colateral)
#'
#' @details
#' Esta funcao realiza:
#' \enumerate{
#'   \item Filtragem de dados por usina
#'   \item Pre-processamento de dados NWP (associacao, interpolacao, preenchimento)
#'   \item Identificacao de meias-horas com geracao solar
#'   \item Geracao de todas as combinacoes para treinamento
#'   \item Treinamento paralelo de modelos via \code{lapply}
#' }
#'
#' @keywords internal
treina_usina <- function(
    iu, dt_usinas, dt_ger_obs, dt_prev, v_modelos_nwp,
    v_horizonte, v_modelos_previsao, parametros_modelo_previsao,
    parametros_periodo_geracao, local_escrita, data_fim_treino
) {
    # Filtra os dados referentes a usina atual
    dad_usi <- dt_usinas[id_usina == iu]
    ger_usi <- dt_ger_obs[id_usina == iu]

    # Associa os dados NWP a usina, adiciona o passo de previsao e filtra usina atual
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

    # identificacao das semi-horas com geracao solar
    periodo_ger <- identifica_periodo_ger(
        dad_usi,
        ger_usi,
        fator_tol_ger = parametros_periodo_geracao$fator_tolerancia_limite_inferior_geracao,
        fator_tol_horas = parametros_periodo_geracao$percentual_dias_geracao
    )

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
    saveRDS(mod_aju, file = paste(local_escrita, paste0(iu, "_modelos_ajustados.rds"), sep = "/"))
}

#' Treina Modelo Individual
#'
#' Treina um modelo de previsao para uma combinacao especifica de
#' NWP, horizonte, meia-hora e tipo de modelo.
#'
#' @param l Lista contendo a combinacao de parametros:
#'   \describe{
#'     \item{id_modelo_nwp}{Identificador do modelo NWP}
#'     \item{horiz_prev}{Horizonte de previsao (e.g., "D+0")}
#'     \item{hora_min}{Meia-hora no formato "HH:MM"}
#'     \item{modelo_prev}{Tipo de modelo de previsao}
#'   }
#' @param ger_usi data.table com geracao observada da usina
#' @param prev_met_usi Lista de data.tables com previsoes meteorologicas
#' @param param_modelo_previsao Lista com parametros dos modelos
#' @param data_fim_treino Data limite para dados de treinamento
#'
#' @return Lista contendo:
#'   \describe{
#'     \item{combinacao_ajuste}{Parametros da combinacao}
#'     \item{modelo}{Objeto do modelo ajustado}
#'   }
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
#' Funcao generica S3 que despacha para o metodo especifico de treinamento
#' baseado no tipo de modelo especificado.
#'
#' @param modelo_parametros Lista com parametros do modelo, incluindo classe S3
#' @param ... Argumentos adicionais passados aos metodos especificos
#'
#' @return Objeto do modelo treinado (tipo depende do metodo especifico)
#'
#' @seealso
#' \code{\link{parse_train.arimax}} para treinamento ARIMAX
#' \code{\link{parse_train.fisico_estimado}} para treinamento Fisico-Estimado
#'
#' @export
parse_train <- function(modelo_parametros, ...) UseMethod("parse_train")

#' Metodo Default para parse_train
#'
#' Metodo default que gera erro para tipos de modelo nao suportados.
#'
#' @param modelo_parametros Lista com parametros do modelo
#' @param ... Argumentos adicionais (ignorados)
#'
#' @return Gera erro indicando tipo de modelo desconhecido
#'
#' @export
parse_train.default <- function(modelo_parametros, ...) {
    stop("Unknown model type for parse_train")
}

#' Treinamento de Modelo Fisico-Estimado
#'
#' Treina um modelo fisico-estimado baseado em regressao linear para previsao
#' de geracao solar. Compara regressao linear simples (RLS) vs multipla (RLM).
#'
#' @param modelo_parametros Lista com parametros do modelo:
#'   \describe{
#'     \item{n_dias_treino}{Numero de dias para janela de treinamento}
#'     \item{amos_min}{Numero minimo de amostras validas requeridas}
#'   }
#' @param ... Argumentos adicionais:
#'   \describe{
#'     \item{pars}{Lista com combinacao de parametros (NWP, horizonte, meia-hora)}
#'     \item{ger_usi}{data.table com geracao observada}
#'     \item{prev_met_usi}{Lista de data.tables com previsoes meteorologicas}
#'     \item{data_fim_treino}{Data limite para dados de treinamento}
#'   }
#'
#' @return Lista contendo:
#'   \describe{
#'     \item{modelo_escolhido}{"RLS" ou "RLM"}
#'     \item{modelo_final}{Objeto lm ajustado}
#'     \item{variaveis_usadas}{Nomes das variaveis utilizadas no modelo}
#'   }
#'
#' @details
#' O metodo executa:
#' \enumerate{
#'   \item Filtra dados pela combinacao NWP × horizonte × meia-hora
#'   \item Elimina dados invalidos (NA, 999)
#'   \item Ajusta modelo RLS: geracao ~ irradiancia
#'   \item Ajusta modelo RLM: geracao ~ irradiancia + outras variaveis
#'   \item Seleciona melhor modelo por erro medio absoluto
#' }
#'
#' Se coeficientes forem negativos ou dados insuficientes, a janela de

#' treinamento e automaticamente expandida (ate +200 dias).
#'
#' @seealso \code{\link{aplica_regressao_linear}}
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

    aumento <- 0 # variavel usada para auxiliar no aumento de amostra, caso necessario
    repeat {
        # seleciona janela dos dados para treinamento
        dt_treino_filt <- seleciona_janela(
            dt_treino,
            data_ref = data_fim_treino,
            janela_dias_treinamento = modelo_parametros$n_dias_treino + aumento
        )
        setnames(dt_treino_filt, "valor", "ger_obs")

        # ajusta dummy
        dt_y <- data.table(ger_obs = rep(0, nrow(dt_treino_filt)))
        dt_x <- data.table(irrad_prev = rep(0, nrow(dt_treino_filt)))
        nlmod0 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x)
        nlmod0$coefficients[is.na(nlmod0$coefficients)] <- 0

        # avalia numero de conjuntos ger x irr x temp x umid
        if (dados_suficientes(dt_treino_filt, num_min_dados = 10) == TRUE) {
            # ajusta RLS
            dt_y <- dt_treino_filt[, .(ger_obs)]
            dt_x <- dt_treino_filt[, .(irrad_prev)]
            nlmod1 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod1$coefficients[is.na(nlmod1$coefficients)] <- 0

            # ajusta RLM
            dt_y <- dt_treino_filt[, .(ger_obs)]
            cols <- setdiff(names(dt_treino_filt)[-1], names(dt_y))
            dt_x <- dt_treino_filt[, .SD, .SDcols = cols]
            nlmod2 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod2$coefficients[is.na(nlmod2$coefficients)] <- 0

            # condicao de parada
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

    # calcula erro medio in-sample
    erros <- calcula_erros_fisico_estimado(dt = dt_treino_filt[, -1], nlmod1, nlmod2)

    # seleciona modelo
    selecao <- seleciona_modelo_fisico_estimado(nlmod1, nlmod2, erros)

    return(selecao)
    # cria diferenciacao para alguns horarios dias para que o ajuste seja apenas ger x irr
}

#' Aplica Regressao Linear
#'
#' Ajusta um modelo de regressao linear a um conjunto de dados,
#' com fallback para modelo dummy em caso de erro.
#'
#' @param dt_y data.table contendo a variavel resposta (e.g., geracao observada)
#' @param dt_x data.table contendo a(s) variavel(is) explicativa(s)
#'   (e.g., irradiancia, umidade, temperatura). As observacoes devem estar
#'   alinhadas com \code{dt_y}.
#' @param nlmod0 Modelo de fallback retornado em caso de erro no ajuste.
#'   Se \code{NULL}, erro e propagado.
#'
#' @return Objeto \code{lm} com o modelo ajustado
#'
#' @examples
#' \dontrun{
#' dt_y <- data.table(ger_obs = c(10, 20, 30))
#' dt_x <- data.table(irrad = c(100, 200, 300))
#' modelo <- aplica_regressao_linear(dt_y, dt_x)
#' }
#'
#' @keywords internal
aplica_regressao_linear <- function(dt_y, dt_x, nlmod0) {
    dados <- cbind(dt_y, dt_x)
    resposta <- names(dt_y)
    preditoras <- names(dt_x)

    formula <- paste(resposta, "~", paste(preditoras, collapse = "+"))
    formula_objeto <- as.formula(formula)

    modelo <- tryCatch(
        lm(formula_objeto, data = dados),
        error = function(e) nlmod0
    )
    return(modelo)
}

#' Treinamento de Modelo ARIMAX
#'
#' Treina um modelo ARIMA com variaveis exogenas (ARIMAX) para previsao
#' de geracao solar. Utiliza selecao automatica de ordem via \code{auto.arima}.
#'
#' @param modelo_parametros Lista com parametros do modelo:
#'   \describe{
#'     \item{n_dias_treino}{Numero de dias para janela de treinamento}
#'     \item{amos_min}{Numero minimo de amostras validas requeridas}
#'   }
#' @param ... Argumentos adicionais:
#'   \describe{
#'     \item{pars}{Lista com combinacao de parametros (NWP, horizonte, meia-hora)}
#'     \item{ger_usi}{data.table com geracao observada}
#'     \item{prev_met_usi}{Lista de data.tables com previsoes meteorologicas}
#'     \item{data_fim_treino}{Data limite para dados de treinamento}
#'   }
#'
#' @return Lista contendo:
#'   \describe{
#'     \item{modelo_escolhido}{"ARIMA" ou "ARIMAX"}
#'     \item{modelo_final}{Objeto Arima ajustado}
#'   }
#'   Se dados insuficientes, retorna modelo dummy (Arima).
#'
#' @details
#' O metodo executa:
#' \enumerate{
#'   \item Filtra dados pela combinacao NWP × horizonte × meia-hora
#'   \item Substitui valores 999 por NA
#'   \item Normaliza variaveis (z-score)
#'   \item Ajusta modelo ARIMA puro via \code{auto.arima}
#'   \item Ajusta modelo ARIMAX com irradiancia como variavel exogena
#'   \item Seleciona melhor modelo por AICc + erro medio absoluto
#' }
#'
#' @seealso
#' \code{\link[forecast]{auto.arima}}
#' \code{\link{ajusta_arima}}, \code{\link{ajusta_arimax}}
#'
#' @export
parse_train.arimax <- function(modelo_parametros, ...) {
    args <- list(...)
    pars <- args$pars
    ger_usi <- args$ger_usi
    prev_met_usi <- args$prev_met_usi
    data_fim_treino <- args$data_fim_treino

    dt_treino <- filtra_dado_por_combinacao(pars, prev_met_usi, ger_usi)

    # FUNCAO QUE CHECA OS DADOS DEVE FAZER ISSO
    dt_treino[, (names(dt_treino)) := lapply(.SD, function(x) fifelse(x == 999, NA, x))]

    # seleciona janela dos dados para treinamento
    dt_treino_filt <- seleciona_janela(
        dt_treino,
        data_fim_treino,
        janela_dias_treinamento = modelo_parametros$n_dias_treino
    )
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

#' Cria Dataset para Treinamento
#'
#' Carrega e organiza os dados necessarios para treinamento dos modelos,
#' incluindo geracao observada e previsoes meteorologicas.
#'
#' @param args Lista de argumentos contendo:
#'   \describe{
#'     \item{ids_usinas}{Vetor de IDs das usinas}
#'     \item{modelos_NWP}{Vetor de modelos NWP a carregar}
#'   }
#' @param conn Objeto de conexao com banco de dados (via pfvIO)
#'
#' @return Lista nomeada contendo:
#'   \describe{
#'     \item{ger_obs}{data.table com geracao observada}
#'     \item{irrad_prev}{data.table com irradiancia prevista}
#'   }
#'
#' @keywords internal
get_dataset <- function(args, conn) {
    ger_obs <- get_geracao_observada(conn, id_usina = args$ids_usinas)
    irrad_prev <- get_irradiancia_prevista(conn, id_usina = args$ids_usinas, id_modelo_nwp = args$modelos_NWP)

    out <- list(ger_obs, irrad_prev)
    names(out) <- c("ger_obs", "irrad_prev")

    return(out)
}

#' Preenche Lacunas de Rodadas NWP
#'
#' Identifica datas em que nao ha rodada do modelo NWP e preenche
#' as previsoes ausentes com NA para manter series temporais completas.
#'
#' @param dt data.table contendo previsoes meteorologicas com colunas:
#'   \describe{
#'     \item{id_modelo_nwp}{Identificador do modelo NWP}
#'     \item{id_usina}{Identificador da usina}
#'     \item{data_hora_rodada}{Data/hora da rodada do modelo}
#'     \item{data_hora_previsao}{Data/hora da previsao}
#'     \item{valor}{Valor previsto}
#'   }
#'
#' @return data.table com datas de rodada completas (lacunas preenchidas com NA)
#'
#' @details
#' A funcao:
#' \enumerate{
#'   \item Identifica sequencia completa de datas entre min e max data_hora_rodada
#'   \item Detecta datas ausentes por modelo NWP e usina
#'   \item Gera registros com valor NA para datas faltantes
#'   \item Mantem estrutura temporal semi-horaria
#' }
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
#' Funcao auxiliar de \code{preenche_ausencia_previsao} que gera um data.table
#' com previsoes NA para uma data de rodada ausente.
#'
#' @param dif data.table com uma linha contendo:
#'   \describe{
#'     \item{id_modelo_nwp}{Modelo NWP}
#'     \item{id_usina}{Usina}
#'     \item{data_hora_rodada}{Data da rodada ausente}
#'     \item{passos_inicio}{Offset inicial das previsoes}
#'     \item{passos_fim}{Offset final das previsoes}
#'   }
#' @param dt_completo data.table de referencia para obter latitude/longitude
#'
#' @return data.table com estrutura identica as previsoes, valores = NA
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
#' Remove registros com valores invalidos (999 ou NA) das series temporais.
#' Valores 999 sao primeiro convertidos para NA, depois registros incompletos
#' sao removidos.
#'
#' @param dt data.table com series temporais (deve conter coluna \code{data_hora})
#'
#' @return data.table filtrado apenas com registros completos
#'
#' @details
#' O valor 999 e tratado como codigo de dado faltante em alguns sistemas
#' meteorologicos. A funcao converte esses valores para NA antes de filtrar.
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
