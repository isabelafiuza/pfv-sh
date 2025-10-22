train_main <- function(args){ # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO
    conn <- conectamock_pfv(args$input)
    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn, dias = 180)

    data_set$irrad_prev <- associa_nwp_usina(dt_usinas = dt_usinas, dt_prev = data_set[names(data_set) != "ger_obs"])
    data_set$irrad_prev <- interpola_previsao_nwp(data_set = data_set$irrad_prev)
    data_set$irrad_prev <- adicionar_passo_previsao(dt_prev = data_set$irrad_prev)
    data_set$irrad_prev <- compatibiliza_datas(data_set)

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
train_fisico_estimado <- function(data_set){

    data_set <- lapply(data_set, function(x) x[valor == 0, valor := NA])
    data_set <- mapply(renomeia_colunas, data_set, "data_hora_observacao", "data_hora")
    data_set <- mapply(renomeia_colunas, data_set, "data_hora_previsao", "data_hora")
    data_set <- lapply(data_set, function(x) x[, hora := format(data_hora, "%H:%M")])
    vetor_horas <- unique(data_set$ger_obs$hora) # ISABELA - DEPOIS SUBSTITUIR PELO PERIODO DE GERACAO IDENTIFICADO PARA CADA USINA
   

    for (h in vetor_horas){

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
aplica_regressao_linear <- function(dt_y, dt_x){

    dados <- cbind(dt_y, dt_x)    
    resposta <- names(dt_y)
    preditoras <- names(dt_x)

    formula <- paste(resposta, "~", paste(preditoras, collapse = "+" ))
    formula_objeto <- as.formula(formula)
    
    modelo <- lm(formula_objeto, data = dados)
    return(modelo)
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
get_dataset <- function(args, conn, dias){
    
    janela <- paste0(args$data_referencia - dias, "/", args$data_referencia)

    ger_obs <- get_geracao_observada(conn, id_usina = args$ids_usinas)
    irrad_prev <- get_irradiancia_prevista(conn, id_usina = args$ids_usinas, id_modelo_nwp = args$modelos_NWP)

    out <- list(ger_obs, irrad_prev)
    names(out) <- c("ger_obs", "irrad_prev")

    return(out)
}

preenche_lacunas_previsao <- function(data_set){ # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO

    datas_rodadas <- unique(data_set$data_hora_rodada)

}

compatibiliza_datas <- function(data_set){ # ISABELA - EM DESENVOLVIMENTO - NAO ESTA FUNCIONANDO

    ger_obs <- data_set$ger_obs  
    ger_obs_colorder <- names(ger_obs)

    irrad_prev <- data_set$irrad_prev
    irrad_prev_colorder <- names(irrad_prev)

    data_ini <- max(min(ger_obs$data_hora_observacao),min(irrad_prev$data_hora_rodada))
    data_fim <- min(max(ger_obs$data_hora_observacao),max(irrad_prev$data_hora_previsao))
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

renomeia_colunas <- function(dt, nome_atual, nome_novo){
    names(dt)[names(dt) == nome_atual] <- nome_novo
    return(dt)
}
