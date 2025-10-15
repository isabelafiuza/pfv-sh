







#' Compatibiliza Janela De Dias Do Dataset Usado Para Treinamento
#'
#' Compatibiliza dataset dos dados usados no treinamento dos modelos de previsao 
#' usando a janela de dias especificada
#' 
#' @param data_set Data.table contendo o dataset a ser usado no treinamendo do modelo
#' @param dias Numero de dias a contar a partir da data de referencia para compatibilizacao do dataset
#' 
#' @return lista contendo o dataset com dados compatibilizados
#' 
compatibiliza_dias_data_set <- function(args, data_set, dias){

    inicio_janela <- as.POSIXct(args$data_referencia - dias)
    fim_janela <- as.POSIXct(args$data_referencia)
    janela <- seq.POSIXt(from = inicio_janela, to = fim_janela, by = "30 min")[-1]
    janela <- data.table(data_hora = head(janela, -1))
    
    ger_obs <- data_set$ger_obs
    colnames <- names(ger_obs)
    names(ger_obs)[names(ger_obs) == "data_hora_observacao"] <- "data_hora"
    ger_obs <- merge(ger_obs, janela, by = "data_hora", all.y = TRUE)
    names(ger_obs)[names(ger_obs) == "data_hora"] <- "data_hora_observacao"
    setcolorder(ger_obs, colnames)

    corte <- data_set$corte
    colnames <- names(corte)
    names(corte)[names(corte) == "data_hora_observacao"] <- "data_hora"
    corte <- merge(corte, janela, by = "data_hora", all.y = TRUE)
    names(corte)[names(corte) == "data_hora"] <- "data_hora_observacao"
    setcolorder(corte, colnames)

    irrad_prev <- data_set$irrad_prev
    colnames <- names(irrad_prev)
    names(irrad_prev)[names(irrad_prev) == "data_hora_previsao"] <- "data_hora"
    irrad_prev <- merge(irrad_prev, janela, by = "data_hora", all.y = TRUE)
    names(irrad_prev)[names(irrad_prev) == "data_hora"] <- "data_hora_previsao"
    setcolorder(irrad_prev, colnames)

    out <- list(ger_obs, corte, irrad_prev)
    names(out) <- c("ger_obs", "corte", "irrad_prev")
    return(out)
}