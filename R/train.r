train_main <- function(args){
    conn <- conectamock_pfv(args$input)
    v_usinas <- args$ids_usinas
    v_horizonte <- args$horizonte_dias

    dt_usinas <- get_usinas(conn, id_usina = v_usinas)

    data_set <- get_dataset(args, conn, dias = 180)



    data_set$irrad_prev <- associa_nwp_usina(dt_usinas = dt_usinas, dt_prev = data_set$irrad_prev)
    data_set$irrad_prev <- interpola_previsao_nwp(data_set = data_set$irrad_prev)
    data_set$irrad_prev <- adicionar_passo_previsao(dt_prev = data_set$irrad_prev)
    data_set$irrad_prev <- compatibiliza_datas(data_set)

    #modelo <- mapply(train_fisico_estimado, v_usinas, v_horizonte, data_set)
}

train_fisico_estimado <- function(usina, horizonte, data_set){

    dt_geracao_observada <- data_set$ger_obs[id_usina == usina]
    dt_geracao_observada <- dt_geracao_observada[, semi_hora := format(data_hora_observacao, "%H:%M")]
    dt_geracao_observada[valor == 999, valor := NA]

    dt_irradiancia_prevista <- data_set$irrad_prev[id_usina == usina & passo_prev == horizonte]
    dt_irradiancia_prevista <- dt_irradiancia_prevista[, semi_hora := format(data_hora_previsao, "%H:%M")]
    dt_irradiancia_prevista[valor == 999, valor := NA]
    
    semi_hora <- dt_geracao_observada$semi_hora
    semi_hora <- semi_hora[!duplicated(semi_hora)]
    
    for(sh in semi_hora){
        dt_geracao_observada_sh <- dt_geracao_observada[semi_hora == sh]
        dt_irradiancia_prevista_sh <- dt_irradiancia_prevista[semi_hora == sh]

        x <- dt_irradiancia_prevista_sh$valor
        y <- dt_geracao_observada_sh$valor

        modelo <- lm(y ~ x)  
    }    
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

preenche_lacunas_previsao <- function(data_set){

    datas_rodadas <- unique(data_set$data_hora_rodada)

}

compatibiliza_datas <- function(data_set){

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