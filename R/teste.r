#' Interpola Dados De Previsao NWP
#' 
#' Realiza interpolação dos dados previstos dos modelos NWP em intervalos semi-horarios
#' 
#' @param data_set ´data.table´ com os dados de previsao NWP a serem interpolados
#' 
#' @return ´data.table´ com dados previstos interpolados
#' 
interpola_dataset <- function(data_set){

    data_hora_interpolacao <- cria_sequencia_datas(data_set$irrad_prev, discretizacao = 30)
    data_set_interpolado <- merge(data_hora_interpolacao, data_set, by = c("data_hora_previsao","id_modelo_nwp","id_usina","passo_prev"))


}

#' Cria Sequencia De Datas
#' 
#' Identifica as datas de inicio e fim das series temporais
#' e cria dt com a sequencia completa de datas
cria_sequencia_datas <- function(dt, discretizacao){

    dt_data_inicio <- dt[, .(data_inicio = min(data_hora_previsao)), by = .(id_modelo_nwp, id_usina)]
    dt_data_fim <- dt[, .(data_fim = max(data_hora_previsao)), by = .(id_modelo_nwp, id_usina)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_nwp","id_usina"))
    dt_sequencia_datas <- dt_datas_inicio_fim[ , .(data_hora_previsao = seq(data_inicio, data_fim, by = paste0(discretizacao," min"))), by = .(id_modelo_nwp, id_usina)]
    
    return(dt_sequencia_datas)
}