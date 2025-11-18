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
    dt_treino <- elimina_dados_invalidos(dt_treino)

    aumento <- 0 # variavel usada para auxiliar no aumento de amostra, caso necessario
    repeat{
        # seleciona janela dos dados para treinamento
        dt_treino_filt <- seleciona_janela(dt_treino, data_ref = data_fim_treino, janela_dias_treinamento = modelo_parametros$n_dias_treino + aumento)
        setnames(dt_treino_filt, "valor", "ger_obs")

        # ajusta dummy
        dt_y <- data.table(ger_obs = rep(0, nrow(dt_treino_filt)))
        dt_x <- data.table(irrad_prev = rep(0, nrow(dt_treino_filt)))
        nlmod0 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x)
        nlmod0$coefficients[is.na(nlmod0$coefficients)] <- 0

        # avalia numero de conjuntos ger x irr x temp x umid
        if (dados_suficientes(dt_treino_filt, num_min_dados = 10) == TRUE){
            # ajusta RLS
            dt_y <- dt_treino_filt[, .(ger_obs)]
            dt_x <- dt_treino_filt[, .(irrad_prev)]
            nlmod1 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod1$coefficients[is.na(nlmod1$coefficients)] <- 0

            # ajusta RLM
            dt_y <- dt_treino_filt[, .(ger_obs)]
            cols <- setdiff(names(dt_treino_filt)[-1],names(dt_y))
            dt_x <- dt_treino_filt[, .SD, .SDcols = cols]
            nlmod2 <- aplica_regressao_linear(dt_y = dt_y, dt_x = dt_x, nlmod0 = nlmod0)
            nlmod2$coefficients[is.na(nlmod2$coefficients)] <- 0

            # condicao de parada
            if ((nlmod1$coefficients[2] > 0 & nlmod2$coefficients[2] > 0) | aumento == 200) {
                break
            }
            aumento <- aumento + 10
        }else{
            if(aumento == 200){                
                nlmod1 <- nlmod0
                nlmod2 <- nlmod0
                break
            }
            aumento <- aumento + 10
        }
    }

    # calcula erro medio in-sample
    erros <- calcula_erros_fisico_estimado(dt = dt_treino_filt[,-1], nlmod1, nlmod2)

    # seleciona modelo
    selecao <- seleciona_modelo_fisico_estimado(nlmod1, nlmod2, erros)

    return(selecao)
    # cria diferenciacao para alguns horarios dias para que o ajuste seja apenas ger x irr
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
#' @return 'data.table' contendo as previsoes com datas de execucao do modelo meteorologico completas
#'
preenche_ausencia_previsao <- function(dt) {
    datas_execucao <- dt[, .(data_hora_rodada = unique(data_hora_rodada)), by = .(id_modelo_nwp,id_usina)]
    datas_completas <- dt[, .(data_hora_rodada = seq.POSIXt(from = min(data_hora_rodada), to = max(data_hora_rodada), by = "days")), by = .(id_modelo_nwp, id_usina)]
    dif <- fsetdiff(datas_completas, datas_execucao)

    if(length(dif) == 0) return(dt)

    passos_inicio <- dt[,  min(data_hora_previsao) - data_hora_rodada, by = .(id_modelo_nwp, data_hora_rodada)]
    setnames(passos_inicio, "V1", "passos_inicio")  
    passos_fim <- dt[,  max(data_hora_previsao) - min(data_hora_previsao), by = .(id_modelo_nwp, data_hora_rodada)]                
    setnames(passos_fim, "V1", "passos_fim")  
    n_passos_previsao <- merge(passos_inicio, passos_fim, by = c("id_modelo_nwp", "data_hora_rodada"))
    n_passos_previsao <- n_passos_previsao[, lapply(.SD, unique),
                                                .SDcols =c("passos_inicio", "passos_fim"), by = id_modelo_nwp]  

    dif <- merge(dif, n_passos_previsao, by = "id_modelo_nwp")
    
    l_datas_faltantes <- lapply(split(dif, seq_len(nrow(dif))), cria_dt_auxiliar, dt_completo = dt)
    dt_datas_faltantes <- rbindlist(l_datas_faltantes)

    dt_prev_completo <- rbindlist(list(dt,dt_datas_faltantes))
    setorder(dt_prev_completo, id_modelo_nwp, id_usina, data_hora_rodada, data_hora_previsao)

    return(dt_prev_completo)    
}

#' Cria Data.table Auxiliar
#' 
#' Auxiliar da funcao ´preenche_ausencia_previsao´. Cria um ´data.table´ auxiliar
#' com as datas ausentes nos dados originais de previsao meteorologica. As previsoes meteorologicas
#' para essas datas sao preenchidas com ´NA´.
#'
#' @param dif  ´data.table´ contendo a data ausente nos dados de previsao numerica para a qual se deseja
#' criar o data.table de previsoes NA. Contem tambem as demais informacoes necessaria para criar o
#' @return ´data.table´ nor formato adequado para inclusao nos dados de previsao meteorologica a ser usado
#' 
cria_dt_auxiliar <- function(dif, dt_completo) {
    latitude <- unique(dt_completo[id_modelo_nwp == dif$id_modelo_nwp & id_usina == dif$id_usina, latitude])
    longitude <- unique(dt_completo[id_modelo_nwp == dif$id_modelo_nwp & id_usina == dif$id_usina, longitude])
    data_hora_previsao_ini <- dif$data_hora_rodada + dif$passos_inicio
    data_hora_previsao_fim <- data_hora_previsao_ini + dif$passos_fim
    seq_data_hora_previsao <- seq.POSIXt(from = data_hora_previsao_ini, to = data_hora_previsao_fim,
                                        by = "30 min")
    dt_auxiliar <- data.table(id_modelo_nwp = dif$id_modelo_nwp, id_usina = dif$id_usina, latitude = latitude,
                            longitude = longitude, data_hora_rodada = dif$data_hora_rodada, data_hora_previsao = seq_data_hora_previsao,
                            valor = NA)

    return(dt_auxiliar)
}

#' Elimina Dados Invalidos
#'
#' Identifica dados invalidos nas series temporais e elimina da amostra.
#' A funcao avalia os valores das series temporais das variaveis contidas no data.table,
#' identifica posicoes com valores 999 ou NA e elimina de todas as variaveis concomitantemente.
#'
#' @param dt 'data.table' contendo os dados com as series temporais
#'
#' @return 'data.table' contendo os dados com as series temporais apos eliminacao de dados invalidos
#'
elimina_dados_invalidos <- function(dt){
    col_var <- setdiff(names(dt), "data_hora")
    dt[, (col_var) := lapply(.SD, function(x) fifelse(x == 999, NA, x)), .SDcols = col_var]
    dt_valido <- dt[complete.cases(dt[, .SD, .SDcols = col_var])]

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

    dt <- dt[data < data_ref]

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
    dt <- copy(dt)
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

#' Calcula erros medios in-sample dos modelos
#'
#' @param dt data.table com variável \code{ger_obs_norm}, \code{irrad_prev_norm},
#' \code{temp_prev_norm} e \code{umid_prev_norm}..
#' @param nlmod1 Modelo RLS
#' @param nlmod2 Modelo RLM
#'
#' @return Lista com erro médio de cada modelo.
calcula_erros_fisico_estimado <- function(dt, nlmod1, nlmod2) {
    prev1 <- as.numeric(fitted(nlmod1))
    prev2 <- as.numeric(fitted(nlmod2))

    dt_valido <- dt[complete.cases(dt)]

    l_erros <- list(erro1 = mean(abs(dt_valido$ger_obs - prev1), na.rm = TRUE),
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
        variaveis_usadas = if(escolhe_fe) names(nlmod1$model) else names(nlmod2$model) # ISABELA - ARRUMAR
    )
}

