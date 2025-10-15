#' Associa Usina A Quadricula da NWP
#'
#' Obtem as previsoes dos modelos NWP para as usinas simuladas. Para isso, realiza a associacao das coordenadas das
#' usinas com quadricula correspondente do modelo NWP.
#' E escolhida a quadricula que apresenta menor distancia euclidiana entre seu centroide e a coordenada da usina.
#' 
#' @param dt_usinas ´data.table´ contendo os dados cadastrais das usinas
#' @param dt_prev ´data.table´ contendo os dados das variaveis previstas do modelo NWP
#' 
#' @return ´data.table´ das usinas simuladas e as respectivas variaveis previstas do modelo NWP
#' 
#' OBSERVACAO ISABELA: Funcao aproveitada do MH, troquei o nome do argumento dt_irrad_prev para dt_prev para deixar generico

# Função usando distância euclidiana
associa_nwp_usina <- function(dt_usinas, dt_prev) {
    # Coordenadas únicas da previsão
    coord_prev <- unique(dt_prev[, .(latitude, longitude)])

    # Lista para armazenar os resultados
    lista_filtrados <- list()

    # Loop sobre cada usina
    for (i in seq_len(nrow(dt_usinas))) {
        usina <- dt_usinas[i]

        # Calcula a distância euclidiana entre a usina e todas as coordenadas da previsão
        coord_prev[, distancia := sqrt((latitude - usina$latitude)^2 + (longitude - usina$longitude)^2)]

        # Pega a coordenada mais próxima
        coord_mais_proxima <- coord_prev[which.min(distancia)]

        # Filtra os dados da previsão para essa coordenada
        dt_filt <- dt_prev[latitude == coord_mais_proxima$latitude &
                longitude == coord_mais_proxima$longitude]

        # Adiciona o id_usina
        dt_filt[, id_usina := usina$id_usina]

        # Adiciona à lista
        lista_filtrados[[i]] <- dt_filt
    }

    # Junta tudo
    dt_prev_filt <- rbindlist(lista_filtrados)

    # Reorganiza para id_usina ser a 2ª coluna
    setcolorder(dt_prev_filt, c(
        "id_modelo_nwp", "id_usina",
        setdiff(names(dt_prev_filt), c("id_modelo_nwp", "id_usina"))
    ))

    return(dt_prev_filt)
}

#' Adiciona Passo De Previsao
#'
#' Obtem as previsoes dos modelos NWP para as usinas simuladas. Para isso, realiza a associacao das coordenadas das
#' usinas com quadricula correspondente do modelo NWP.
#' E escolhida a quadricula que apresenta menor distancia euclidiana entre seu centroide e a coordenada da usina.
#' 
#' @param dt_usinas ´data.table´ contendo os dados cadastrais das usinas
#' @param dt_prev ´data.table´ contendo os dados das variaveis previstas do modelo NWP
#' 
#' @return ´data.table´ das usinas simuladas e as respectivas variaveis previstas do modelo NWP
#' 
#' OBSERVACAO ISABELA: Funcao aproveitada do MH, troquei o nome do argumento dt_irrad_prev para dt_prev para deixar generico
#' 
adicionar_passo_previsao <- function(dt_prev) {
    # Garante que as colunas são do tipo POSIXct
    dt_prev[, data_hora_rodada := as.POSIXct(data_hora_rodada)]
    dt_prev[, data_hora_previsao := as.POSIXct(data_hora_previsao)]

    # Calcula a diferença de dias entre as datas (ignorando horário)
    dt_prev[, passo_prev := paste0(
        "D+",
        as.integer(as.Date(data_hora_previsao) - as.Date(data_hora_rodada))
    )]

    return(dt_prev)
}

#' Interpola Dados De Previsao NWP
#' 
#' Realiza interpolação dos dados previstos dos modelos NWP em intervalos semi-horarios
#' 
#' @param data_set ´data.table´ com os dados de previsao NWP a serem interpolados
#' 
#' @return ´data.table´ com dados previstos interpolados
#' 
interpola_previsao_nwp <- function(data_set){

    colorder <- names(data_set)
    data_hora_interpolacao <- cria_sequencia_datas(dt = data_set, discretizacao = 30)    
    data_set_discretizacao <- merge(data_hora_interpolacao, data_set, by = c("data_hora_rodada","data_hora_previsao","id_modelo_nwp","id_usina"), all.x = TRUE)
    data_set_interpolado <- data_set_discretizacao[, valor := intepola_serie_temporal(data_set_discretizacao$valor)]
    setcolorder(data_set_interpolado, colorder)

    return(data_set_interpolado)
}

#' Cria Sequencia De Datas
#' 
#' Identifica as datas de inicio e fim das series temporais
#' e cria dt com a sequencia completa de datas
cria_sequencia_datas <- function(dt, discretizacao){

    dt_data_inicio <- dt[, .(data_inicio = min(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_data_fim <- dt[, .(data_fim = max(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_nwp","id_usina", "data_hora_rodada"))
    dt_sequencia_datas <- dt_datas_inicio_fim[ , .(data_hora_previsao = seq(data_inicio, data_fim, by = paste0(discretizacao," min"))), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    
    return(dt_sequencia_datas)
}

#' Interpola Serie Temporal
#' 
#' Identifica as lacunas da serie temporal e preenche realizando interpolacao linear
intepola_serie_temporal <- function(dt){
    dt_interpolado <- na.approx(dt)
    return(dt_interpolado)
} 
