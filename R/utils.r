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
associa_nwp_usina <- function(dt_prev, dt_usinas) {
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
interpola_previsao_nwp <- function(data_set_met) {
    colorder <- names(data_set_met)
    data_hora_interpolacao <- cria_sequencia_datas(dt = data_set_met, discretizacao = 30)
    data_set_discretizacao <- merge(data_hora_interpolacao, data_set_met, by = c("data_hora_rodada", "data_hora_previsao", "id_modelo_nwp", "id_usina"), all.x = TRUE)
    data_set_interpolado <- data_set_discretizacao[, valor := interpola_serie_temporal(valor),
        by = .(id_modelo_nwp, id_usina, data_hora_rodada)
    ]
    cols_propagar <- setdiff(names(data_set_interpolado), c(names(data_hora_interpolacao), "valor"))

    for (col in cols_propagar) {
        data_set_interpolado[, (col) := nafill(get(col), type = "locf"),
            by = .(id_modelo_nwp, id_usina, data_hora_rodada)
        ]
    }

    setcolorder(data_set_interpolado, colorder)

    return(data_set_interpolado)
}

#' Cria Sequencia De Datas
#'
#' Identifica as datas de inicio e fim das series temporais
#' e cria dt com a sequencia completa de datas
cria_sequencia_datas <- function(dt, discretizacao) {
    dt_data_inicio <- dt[, .(data_inicio = min(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_data_fim <- dt[, .(data_fim = max(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_nwp", "id_usina", "data_hora_rodada"))
    dt_sequencia_datas <- dt_datas_inicio_fim[, .(data_hora_previsao = seq(data_inicio, data_fim, by = paste0(discretizacao, " min"))), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]

    return(dt_sequencia_datas)
}

#' Interpola Serie Temporal
#'
#' Identifica as lacunas da serie temporal e preenche realizando interpolacao linear
interpola_serie_temporal <- function(dt) {
    dt_interpolado <- na.approx(dt)
    return(dt_interpolado)
}

#' Identifica periodo do dia com geracao solar
#'
#' @description
#' A funcao identifica horarios ("HH:MM") em que ha geracao
#' ao longo de um ano.
#' Util para definir periodo em que havera treinamento de modelos
#' e previsao de geracao solar
#'
#' @param dad_usina data.table contendo pelo menos:
#' \itemize {
#' \item id_usina
#' \item potencia instalada
#' }
#' @param ger_usina data.table contendo pelo menos:
#' \itemize {
#' \item data_hora_observacao
#' \item valor
#' }
#' @param fator_tol_ger
#' fator minimo da capacidade instalada da usina para considerar
#' que houve geracao
#' @param janela_dias
#' numero de dias passados a considerar na analise
#' @param fator_tol_horas
#' fator minimo dos dias analisados com geracao
#'
#' @return
#' vetor de characteres com as meias-horas ("HH:MM")
#'
#' @details
#' A funcao segue as etapas:
#' \itemize {
#'   \item extrai a meia-hora, hora_min, de cada observacao
#'   \item seleciona os dias para analise
#'   \item para cada meia-hora, conta o numero de dias com geracao superior ao limiar estabelecido
#'   \item retorna as meias-horas com geracao em pelo menos `fator_tol_dias*janela_dias` dias.
#' }

identifica_periodo_ger <- function(dad_usina, ger_usi, fator_tol_ger, fator_tol_horas) {
    janela_dias <- 300
    dt <- copy(ger_usi)

    dt[, hora_min := format(data_hora_observacao, "%H:%M")]
    dt[, data := as.Date(data_hora_observacao)]

    # considerar apenas as ultimas `janela_dias` datas
    ultimas_datas <- head(sort(unique(dt$data), decreasing = TRUE), janela_dias)
    dt <- dt[data %in% ultimas_datas]

    # contar em quantos dias houve geracao acima do limiar para cada hora:minuto
    resumo <- dt[
        valor > fator_tol_ger * dad_usi$capacidade_instalada_MW,
        .(dias_com_geracao = uniqueN(data)),
        by = hora_min
    ]

    dias_minimos <- fator_tol_horas * length(ultimas_datas)
    horas_validas <- resumo[dias_com_geracao >= dias_minimos, hora_min]

    horas_validas <- sort(horas_validas)

    return(horas_validas)
}

#' Gera combinacoes entre nwp, passo de previsao e meia-hora
#'
#' @description
#' Cria uma lista com todas as combinacoes possiveis entre modelos meteorologicos,
#' passos de previsao e meias-horas
#'
#' @param v_modelos_nwp vetor com os nomes dos modelos meteorologicos, ex: c("GFS")
#' @param v_horizonte vetor de caracteres com os passos de previsao, ex: c("D+0", "D+1")
#' @param periodo_ger vetor de horarios no formato HH:MM, ex: c("12:00", "12:30")
#'
#' @return lista com as colunas:
#' \itemize {
#' \item id_modelo_nwp - modelo meteorologico
#' \item horiz_prev - passo de previsao
#' \item periodo_ger - meias-horas com geracao solar
#' }
#'
#' @examples
#' v_nwp <- c("GFS", "ECMWF")
#' v_horiz <- c("D+0", "D+1")
#' v_hor_ger <- c("12:00", "12:30")
#' gera_combinacoes_modelo(v_nwp, v_horiz, v_hor_ger)
gera_combinacoes_modelo <- function(v_modelos_nwp, v_horizonte, periodo_ger) {
    comb <- CJ(
        id_modelo_nwp = unlist(v_modelos_nwp),
        horiz_prev = v_horizonte,
        hora_min = periodo_ger
    )

    lista_comb <- split(comb, seq_len(nrow(comb)))
    lista_comb <- lapply(lista_comb, as.list)

    return(lista_comb)
}

#' Filtra dados meteorologicos para cada combinacao nwp x passo de previsao x meia-hora
#'
#' @description
#' Para cada trio modelo meteorologico x passo de previsao x meias-horas,
#' e filtrado o conjunto de dados de interesse do data.table original
#'
#' @param irr_usi `data.table` pelo menos com as colunas:
#' \itemize id_modelo_nwp - modelo meteorologico
#' \item horiz_prev - passo de previsao
#' \item hora_min - meias-horas com geracao solar (formato HH:MM)
#' \item valor - irradiancia observada
#'
#' @param list_comb lista de listas, em que cada elemento contem:
#' \itemize {
#'   \item id_modelo_nwp
#'   \item horiz_prev
#'   \item hora_min
#' }
#'
#' @return `data.table` com os dados meteorologicos filtrados

filtra_dado_por_combinacao <- function(irr_usi, list_comb) {
    irr_usi_filt <- copy(irr_usi)

    lapply(list_comb, function(pars) {
        irr_usi_filt[
            id_modelo_nwp == pars$id_modelo_nwp &
                passo_prev == pars$horiz_prev &
                hora_min == pars$hora_min
        ]
    })
}
