#' Gera vetor de datas-alvo de previsao
#'
#' Esta funcao cria um vetor de datas correspondentes aos horizontes de previsao,
#' com base em uma data de referencia (default: o dia de execucao da previsao)
#' e em um vetor de horizontes (D+0 a D+9).
#'
#' @param data_referencia `Date` ou `character` coerente com formato de data.
#'   Representa a data-base da execucao da previsao (ex: "2025-11-07").
#' @param v_horizonte Vetor indicando os horizontes de previsao.
#'
#' @return Vetor de classe `Date` contendo as datas alvo de previsao.
#' @examples
#' gera_datas_alvo("2025-11-07", c("D+0", "D+1"))
#' # Retorna: 2025-11-07, 2025-11-08
#'
#' @export
define_hor_prev <- function(data_referencia, v_horizonte) {
  
  data_ref <- as.Date(data_referencia)

  datas_alvo <- seq.Date(
    from = data_ref,
    by = "day",
    length.out = length(v_horizonte)
  )

  return(datas_alvo)
}

#' Associa Usina A Quadricula da NWP
#'
#' Obtem as previsoes dos modelos NWP para as usinas simuladas. Para isso, realiza a associacao das coordenadas das
#' usinas com quadricula correspondente do modelo NWP.
#' E escolhida a quadricula que apresenta menor distancia euclidiana entre seu centroide e a coordenada da usina.
#' 
#' @param dt_prev ´data.table´ contendo os dados das variaveis previstas do modelo NWP
#' @param dt_usinas ´data.table´ contendo os dados cadastrais das usinas
#'
#' @return ´data.table´ das usinas simuladas e as respectivas variaveis previstas do modelo NWP
#'
#' OBSERVACAO ISABELA: Funcao aproveitada do MH, troquei o nome do argumento dt_irrad_prev para dt_prev para deixar generico

# Função usando distância euclidiana
associa_nwp_usina <- function(dt_prev, dt_usinas) {
    # Coordenadas únicas da previsão
    coord_prev <- unique(dt_prev[, .(latitude, longitude), by = "id_modelo_nwp"])

    # Lista para armazenar os resultados
    lista_filtrados <- list()

    # Loop sobre cada usina
    for (i in seq_len(nrow(dt_usinas))) {
        usina <- dt_usinas[i]

        # Calcula a distância euclidiana entre a usina e todas as coordenadas da previsão
        coord_prev[, distancia := sqrt((latitude - usina$latitude)^2 + (longitude - usina$longitude)^2)]

        # Pega a coordenada mais próxima
        coord_mais_proxima <- coord_prev[coord_prev[, .I[which.min(distancia)], by = "id_modelo_nwp"]$V1]

        # Filtra os dados da previsão para essa coordenada
        dt_filt <- dt_prev[dt_prev[, .I[which(latitude == coord_mais_proxima$latitude & longitude == coord_mais_proxima$longitude)], by = "id_modelo_nwp"]$V1]

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
#' @param dt_prev ´data.table´ com os dados de previsao NWP a serem interpolados
#'
#' @return ´data.table´ com dados previstos interpolados
#'
interpola_previsao_nwp <- function(dt_prev) {
    colorder <- names(dt_prev)
    data_hora_interpolacao <- cria_sequencia_datas(dt = dt_prev, discretizacao = "30 min")
    data_set_discretizacao <- merge(data_hora_interpolacao, dt_prev, by = c("data_hora_rodada", "data_hora_previsao", "id_modelo_nwp", "id_usina"), all.x = TRUE)
    data_set_discretizacao[, valor := interpola_serie_temporal(valor),
        by = .(id_modelo_nwp, id_usina, data_hora_rodada)
    ]
    data_set_interpolado <- copy(data_set_discretizacao)
    cols_propagar <- setdiff(names(data_set_interpolado), c(names(data_hora_interpolacao), "valor"))

    for (col in cols_propagar) {
        data_set_interpolado[, (col) := nafill(nafill(get(col), type = "locf"), type = "nocb"),
            by = .(id_modelo_nwp, id_usina, data_hora_rodada)
        ]
    }

    setcolorder(data_set_interpolado, colorder)
    data_set_interpolado <- data_set_interpolado[order(id_modelo_nwp, data_hora_rodada, data_hora_previsao)]

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
    dt_sequencia_datas <- dt_datas_inicio_fim[, .(data_hora_previsao = seq(data_inicio, data_fim, by = discretizacao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]

    return(dt_sequencia_datas)
}

#' Interpola Serie Temporal
#'
#' Identifica as lacunas da serie temporal e preenche realizando interpolacao linear
interpola_serie_temporal <- function(dt) {
    dt_interpolado <- na.approx(dt, na.rm = FALSE)
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
gera_combinacoes_modelo <- function(v_modelos_nwp, v_horizonte, periodo_ger, v_modelos_previsao) {
    comb <- CJ(
        id_modelo_nwp = unlist(v_modelos_nwp),
        horiz_prev = v_horizonte,
        hora_min = periodo_ger,
        modelo_prev = v_modelos_previsao
    )

    lista_comb <- split(comb, seq_len(nrow(comb)))
    lista_comb <- lapply(lista_comb, as.list)

    return(lista_comb)
}

#' Filtra data.table com informacao meteorologica e com geracao
#' para ajuste do modelo de interesse (nwp x passo de previsao x meia-hora)
#' e une ambos os data.tables em um unico
#'
#' @description
#' Para cada trio modelo meteorologico x passo de previsao x meias-horas,
#' e filtrado o conjunto de dados de interesse do data.table original com
#' as previsoes e com a geracao. Realiza ainda a juncao em um unico data.table
#' com os dados para treinamento
#'
#' @param prev_met_usi `list` contendo data.tables pelo menos com as colunas:
#' \itemize {
#'  \item id_modelo_nwp - modelo meteorologico
#'  \item data_hora_previsao - data referencia da previsao
#'  \item horiz_prev - passo de previsao
#'  \item hora_min - data_hora em formato HH:MM
#'  \item valor - variavel meteorologica prevista
#' }
#'
#' @param ger_usi `data.table` pelo menos com as colunas:
#' \itemize {
#'  \item data_hora_observacao - data referencia da observacao
#'  \item hora_min - data_hora em formato HH:MM
#'  \item valor - geracao observada
#' }
#'
#' @param elem_comb `list` em que cada elemento contem:
#' \itemize {
#'   \item id_modelo_nwp
#'   \item horiz_prev
#'   \item hora_min
#' }
#'
#' @return `data.table` com os dados filtrados para treinamento

filtra_dado_por_combinacao <- function(elem_comb, prev_met_usi, ger_usi) {
    ger_usi_filt <- copy(ger_usi)
    prev_met_usi_list <- copy(prev_met_usi)

    # filtra os dados para o trio nwp x passo de previsao x meia-hora desejado
    ger_usi_filt <- ger_usi_filt[hora_min == elem_comb$hora_min]

    prev_met_usi_list <- lapply(prev_met_usi_list, function(dt) {
        dt[
            id_modelo_nwp == elem_comb$id_modelo_nwp &
                passo_prev == elem_comb$horiz_prev &
                hora_min == elem_comb$hora_min
        ]
    })

    # unifica as variaveis meteorologicas em um unico data.table
    prev_met_usi_filt <- rbindlist(prev_met_usi_list, idcol = "variavel_meteoro")

    id_cols <- setdiff(names(prev_met_usi_filt), c("valor", "variavel_meteoro"))

    prev_usi_wide <- dcast(
        prev_met_usi_filt,
        formula = paste(paste(id_cols, collapse = " + "), "~ variavel_meteoro"),
        value.var = "valor"
    )

    # une geracao com a variavel meteorologica
    cols_apagar <- c("id_modelo_nwp", "id_usina", "latitude", "longitude", "data_hora_rodada", "passo_prev", "hora_min")
    prev_usi_wide[, (cols_apagar) := NULL]

    cols_apagar <- c("id_fonte_observacao", "id_usina", "status", "hora_min")
    ger_usi_filt[, (cols_apagar) := NULL]

    setnames(prev_usi_wide, "data_hora_previsao", "data_hora")
    setnames(ger_usi_filt, "data_hora_observacao", "data_hora")

    dt_merged <- merge(
        prev_usi_wide,
        ger_usi_filt,
        by = "data_hora",
        all.x = TRUE,
        all.y = TRUE
    )

    return(dt_merged)
}

#' Monta data.table com previsoes elaboradas para uma usina
#'
#' @description
#' Organiza previsao elaborada para uma usina em um data.table
#' contendo informacoes do modelo de previsao, modelo nwp, data_hora_rodada
#' e data_hora_previsao
#'
#' @param prev_usina_elem `list` contendo listas pelo menos com os elementos:
#' \itemize {
#'  \item combinacao_ajuste - conjunto de parametros que definem a previsao,
#'  incluindo id_modelo_nwp, horiz_prev, hora_min, modelo_prev
#'  \item prev - valor previsto
#' }
#'
#' @param id_usina `character` com identificador da usina
#'
#' @param data_referencia `date` que identificada data da rodada
#'
#' @return `data.table` com os dados previstos para a usina

monta_dt_prev <- function(prev_usina_elem, id_usina, data_referencia) {

    data_hora_rodada <- as.POSIXct(data_referencia, tz = "UTC")

    # dados da combinação
    id_modelo_prev  <- prev_usina_elem$combinacao_ajuste$modelo_prev
    id_modelo_nwp   <- prev_usina_elem$combinacao_ajuste$id_modelo_nwp
    horiz_prev      <- prev_usina_elem$combinacao_ajuste$horiz_prev   
    hora_min        <- prev_usina_elem$combinacao_ajuste$hora_min     

    # define data_hora_previsao
    dias <- as.integer(sub("D\\+", "", horiz_prev))

    hora_prev <- hm(hora_min)
    data_hora_previsao <- data_hora_rodada + days(dias) + hora_prev

    # extrai valor da previsao
    valor <- as.numeric(prev_usina_elem$prev)

    data.table(
        id_modelo_prev = id_modelo_prev,
        id_usina = id_usina,
        id_modelo_nwp = id_modelo_nwp,
        data_hora_rodada = data_hora_rodada,
        data_hora_previsao = data_hora_previsao,
        valor = valor
    )
}


