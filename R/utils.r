#' Gera Vetor de Datas-Alvo de Previsao
#'
#' Cria um vetor de datas correspondentes aos horizontes de previsao,
#' a partir de uma data de referencia.
#'
#' @param data_referencia Data base da execucao, como \code{Date} ou
#'   \code{character} no formato "YYYY-MM-DD"
#' @param v_horizonte Vetor de horizontes de previsao (e.g., c("D+0", "D+1")).
#'   O comprimento deste vetor determina o numero de datas retornadas.
#'
#' @return Vetor de classe \code{Date} com as datas-alvo de previsao
#'
#' @examples
#' define_hor_prev("2025-11-07", c("D+0", "D+1"))
#' # Retorna: 2025-11-07, 2025-11-08
#'
#' define_hor_prev("2025-01-01", c("D+0", "D+1", "D+2", "D+3"))
#' # Retorna: 2025-01-01, 2025-01-02, 2025-01-03, 2025-01-04
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

#' Associa Usinas as Quadriculas NWP
#'
#' Mapeia cada usina para a quadricula mais proxima do modelo NWP,
#' usando distancia euclidiana entre coordenadas.
#'
#' @param dt_prev data.table com previsoes NWP contendo:
#'   \describe{
#'     \item{id_modelo_nwp}{Identificador do modelo NWP}
#'     \item{latitude}{Latitude do ponto de grade}
#'     \item{longitude}{Longitude do ponto de grade}
#'     \item{data_hora_rodada}{Data/hora da rodada}
#'     \item{data_hora_previsao}{Data/hora da previsao}
#'     \item{valor}{Valor previsto}
#'   }
#' @param dt_usinas data.table com cadastro de usinas contendo:
#'   \describe{
#'     \item{id_usina}{Identificador da usina}
#'     \item{latitude}{Latitude da usina}
#'     \item{longitude}{Longitude da usina}
#'   }
#'
#' @return data.table com previsoes associadas as usinas, incluindo
#'   coluna \code{id_usina}
#'
#' @details
#' Para cada usina, a funcao:
#' \enumerate{
#'   \item Calcula distancia euclidiana para todos os pontos de grade
#'   \item Seleciona o ponto mais proximo por modelo NWP
#'   \item Atribui previsoes desse ponto a usina
#' }
#'
#' A coluna \code{id_usina} e posicionada como segunda coluna no resultado.
#'
#' @keywords internal
associa_nwp_usina <- function(dt_prev, dt_usinas) {
    # Coordenadas unicas da previsao
    coord_prev <- unique(dt_prev[, .(latitude, longitude), by = "id_modelo_nwp"])

    # Lista para armazenar os resultados
    lista_filtrados <- list()

    # Loop sobre cada usina
    for (i in seq_len(nrow(dt_usinas))) {
        usina <- dt_usinas[i]

        # Calcula a distancia euclidiana entre a usina e todas as coordenadas da previsao
        coord_prev[, distancia := sqrt((latitude - usina$latitude)^2 + (longitude - usina$longitude)^2)]

        # Pega a coordenada mais proxima
        coord_mais_proxima <- coord_prev[coord_prev[, .I[which.min(distancia)], by = "id_modelo_nwp"]$V1]

        # Filtra os dados da previsao para essa coordenada
        dt_filt <- dt_prev[
            dt_prev[,
                .I[which(latitude == coord_mais_proxima$latitude & longitude == coord_mais_proxima$longitude)],
                by = "id_modelo_nwp"
            ]$V1
        ]

        # Adiciona o id_usina
        dt_filt[, id_usina := usina$id_usina]

        # Adiciona a lista
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

#' Adiciona Passo de Previsao
#'
#' Calcula e adiciona coluna \code{passo_prev} indicando o horizonte
#' de cada previsao em formato "D+N" (dias a frente da rodada).
#'
#' @param dt_prev data.table com previsoes NWP contendo:
#'   \describe{
#'     \item{data_hora_rodada}{Data/hora da rodada do modelo}
#'     \item{data_hora_previsao}{Data/hora da previsao}
#'   }
#'
#' @return data.table com coluna adicional \code{passo_prev}
#'
#' @details
#' O passo e calculado como a diferenca em dias (ignorando horario)
#' entre \code{data_hora_previsao} e \code{data_hora_rodada}.
#'
#' @examples
#' \dontrun{
#' dt <- data.table(
#'     data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
#'     data_hora_previsao = as.POSIXct("2025-01-02 12:00:00", tz = "UTC")
#' )
#' adicionar_passo_previsao(dt)
#' # passo_prev = "D+1"
#' }
#'
#' @keywords internal
adicionar_passo_previsao <- function(dt_prev) {
    # Garante que as colunas sao do tipo POSIXct
    dt_prev[, data_hora_rodada := as.POSIXct(data_hora_rodada)]
    dt_prev[, data_hora_previsao := as.POSIXct(data_hora_previsao)]

    # Calcula a diferenca de dias entre as datas (ignorando horario)
    dt_prev[, passo_prev := paste0(
        "D+",
        as.integer(as.Date(data_hora_previsao) - as.Date(data_hora_rodada))
    )]

    return(dt_prev)
}

#' Interpola Previsoes NWP para Resolucao Semi-horaria
#'
#' Converte dados NWP de resolucao horaria (ou outra) para resolucao
#' semi-horaria (30 minutos) usando interpolacao linear.
#'
#' @param dt_prev data.table com previsoes NWP
#'
#' @return data.table com dados interpolados em intervalos de 30 minutos
#'
#' @details
#' A funcao:
#' \enumerate{
#'   \item Cria sequencia completa de timestamps em intervalos de 30 min
#'   \item Faz merge com dados originais
#'   \item Aplica interpolacao linear nos valores faltantes
#'   \item Propaga colunas auxiliares via LOCF (last observation carried forward)
#' }
#'
#' @seealso \code{\link{cria_sequencia_datas}}, \code{\link{interpola_serie_temporal}}
#'
#' @keywords internal
interpola_previsao_nwp <- function(dt_prev) {
    colorder <- names(dt_prev)
    data_hora_interpolacao <- cria_sequencia_datas(dt = dt_prev, discretizacao = "30 min")
    data_set_discretizacao <- merge(
        data_hora_interpolacao,
        dt_prev,
        by = c("data_hora_rodada", "data_hora_previsao", "id_modelo_nwp", "id_usina"),
        all.x = TRUE
    )
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

#' Cria Sequencia de Datas para Interpolacao
#'
#' Gera data.table com sequencia completa de timestamps entre
#' inicio e fim de cada serie temporal.
#'
#' @param dt data.table com previsoes contendo colunas de agrupamento
#'   e \code{data_hora_previsao}
#' @param discretizacao Intervalo de tempo para a sequencia (e.g., "30 min", "1 hour")
#'
#' @return data.table com colunas de agrupamento e \code{data_hora_previsao}
#'   preenchida com sequencia completa
#'
#' @keywords internal
cria_sequencia_datas <- function(dt, discretizacao) {
    dt_data_inicio <- dt[, .(data_inicio = min(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_data_fim <- dt[, .(data_fim = max(data_hora_previsao)), by = .(id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_nwp", "id_usina", "data_hora_rodada"))
    dt_sequencia_datas <- dt_datas_inicio_fim[,
        .(data_hora_previsao = seq(data_inicio, data_fim, by = discretizacao)),
        by = .(id_modelo_nwp, id_usina, data_hora_rodada)
    ]

    return(dt_sequencia_datas)
}

#' Interpola Serie Temporal
#'
#' Aplica interpolacao linear para preencher valores NA em uma serie temporal.
#'
#' @param dt Vetor numerico a ser interpolado
#'
#' @return Vetor numerico com valores interpolados (NA nas extremidades sao mantidos)
#'
#' @details
#' Usa \code{zoo::na.approx} com \code{na.rm = FALSE}, mantendo NAs
#' no inicio e fim da serie que nao podem ser interpolados.
#'
#' @seealso \code{\link[zoo]{na.approx}}
#'
#' @keywords internal
interpola_serie_temporal <- function(dt) {
    dt_interpolado <- na.approx(dt, na.rm = FALSE)
    return(dt_interpolado)
}

#' Identifica Periodo de Geracao Solar
#'
#' Determina as meias-horas do dia em que ha geracao solar significativa,
#' baseado em analise historica.
#'
#' @param dad_usina data.table com dados cadastrais da usina:
#'   \describe{
#'     \item{id_usina}{Identificador da usina}
#'     \item{capacidade_instalada_MW}{Capacidade instalada em MW}
#'   }
#' @param ger_usi data.table com geracao observada:
#'   \describe{
#'     \item{data_hora_observacao}{Timestamp da observacao}
#'     \item{valor}{Geracao em MW}
#'   }
#' @param fator_tol_ger Fator minimo da capacidade para considerar geracao
#'   significativa (e.g., 0.01 = 1% da capacidade)
#' @param fator_tol_horas Fracao minima de dias com geracao para validar
#'   um horario (e.g., 0.9 = 90% dos dias)
#'
#' @return Vetor de strings no formato "HH:MM" com meias-horas validas
#'
#' @details
#' A funcao analisa os ultimos 300 dias de geracao e:
#' \enumerate{
#'   \item Extrai meia-hora de cada observacao
#'   \item Conta dias com geracao > fator_tol_ger × capacidade
#'   \item Retorna horarios com geracao em >= fator_tol_horas × dias
#' }
#'
#' Util para excluir horarios noturnos do treinamento/previsao.
#'
#' @keywords internal
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
        valor > fator_tol_ger * dad_usina$capacidade_instalada_MW,
        .(dias_com_geracao = uniqueN(data)),
        by = hora_min
    ]

    dias_minimos <- fator_tol_horas * length(ultimas_datas)
    horas_validas <- resumo[dias_com_geracao >= dias_minimos, hora_min]

    horas_validas <- sort(horas_validas)

    return(horas_validas)
}

#' Gera Combinacoes de Parametros para Modelagem
#'
#' Cria lista com todas as combinacoes de modelo NWP, horizonte,
#' meia-hora e tipo de modelo de previsao.
#'
#' @param v_modelos_nwp Vetor de modelos NWP (e.g., c("GFS", "ECMWF"))
#' @param v_horizonte Vetor de horizontes (e.g., c("D+0", "D+1"))
#' @param periodo_ger Vetor de meias-horas com geracao (e.g., c("12:00", "12:30"))
#' @param v_modelos_previsao Tipo de modelo de previsao
#'
#' @return Lista de listas, cada uma contendo:
#'   \describe{
#'     \item{id_modelo_nwp}{Modelo NWP}
#'     \item{horiz_prev}{Horizonte de previsao}
#'     \item{hora_min}{Meia-hora}
#'     \item{modelo_prev}{Tipo de modelo de previsao}
#'   }
#'
#' @details
#' Usa \code{data.table::CJ} para gerar produto cartesiano eficiente.
#' O numero de combinacoes = |NWP| × |horizontes| × |meias-horas| × |modelos|.
#'
#' @examples
#' \dontrun{
#' gera_combinacoes_modelo(
#'     v_modelos_nwp = c("GFS"),
#'     v_horizonte = c("D+0", "D+1"),
#'     periodo_ger = c("12:00", "12:30"),
#'     v_modelos_previsao = "arimax"
#' )
#' # Retorna lista com 4 combinacoes
#' }
#'
#' @keywords internal
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

#' Filtra Dados por Combinacao de Parametros
#'
#' Filtra e une dados de geracao e previsoes meteorologicas para uma
#' combinacao especifica de NWP, horizonte e meia-hora.
#'
#' @param elem_comb Lista com combinacao de parametros:
#'   \describe{
#'     \item{id_modelo_nwp}{Modelo NWP}
#'     \item{horiz_prev}{Horizonte de previsao (e.g., "D+0")}
#'     \item{hora_min}{Meia-hora no formato "HH:MM"}
#'   }
#' @param prev_met_usi Lista de data.tables com previsoes meteorologicas,
#'   cada uma contendo: id_modelo_nwp, passo_prev, hora_min, data_hora_previsao, valor
#' @param ger_usi data.table com geracao observada contendo:
#'   data_hora_observacao, hora_min, valor
#'
#' @return data.table com colunas:
#'   \describe{
#'     \item{data_hora}{Timestamp unificado}
#'     \item{irrad_prev}{Irradiancia prevista (e outras variaveis meteorologicas)}
#'     \item{valor}{Geracao observada}
#'   }
#'
#' @details
#' A funcao:
#' \enumerate{
#'   \item Filtra dados pela combinacao especificada
#'   \item Pivota variaveis meteorologicas para formato wide
#'   \item Une com geracao observada por timestamp
#' }
#'
#' @keywords internal
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

#' Monta data.table de Previsoes
#'
#' Organiza previsao elaborada em data.table com estrutura padronizada,
#' incluindo metadados de modelo e timestamps.
#'
#' @param prev_usina_elem Lista contendo:
#'   \describe{
#'     \item{combinacao_ajuste}{Lista com id_modelo_nwp, horiz_prev, hora_min, modelo_prev}
#'     \item{prev}{Valor previsto}
#'   }
#' @param id_usina Identificador da usina (character)
#' @param data_referencia Data da rodada (Date)
#'
#' @return data.table com colunas:
#'   \describe{
#'     \item{id_modelo_prev}{Tipo do modelo de previsao}
#'     \item{id_usina}{Identificador da usina}
#'     \item{id_modelo_nwp}{Modelo NWP utilizado}
#'     \item{data_hora_rodada}{Data/hora da rodada (POSIXct UTC)}
#'     \item{data_hora_previsao}{Data/hora da previsao (POSIXct UTC)}
#'     \item{valor}{Geracao prevista (numeric)}
#'   }
#'
#' @details
#' Calcula \code{data_hora_previsao} a partir de:
#' \code{data_hora_rodada + dias(horizonte) + hora_min}
#'
#' @keywords internal
monta_dt_prev <- function(prev_usina_elem, id_usina, data_referencia) {
    data_hora_rodada <- as.POSIXct(data_referencia, tz = "UTC")

    # dados da combinacao
    id_modelo_prev <- prev_usina_elem$combinacao_ajuste$modelo_prev
    id_modelo_nwp <- prev_usina_elem$combinacao_ajuste$id_modelo_nwp
    horiz_prev <- prev_usina_elem$combinacao_ajuste$horiz_prev
    hora_min <- prev_usina_elem$combinacao_ajuste$hora_min

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

#' Completa Datas Da Previsao Final
#'
#' Gera data.table com sequencia completa de timestamps da previsao final 
#'
#' @param dt data.table com colunas:
#'   \describe{
#'     \item{id_modelo_prev}{}
#'     \item{id_usina}{}
#'     \item{id_modelo_nwp}{}
#'     \item{data_hora_rodada}{}
#'   }
#' @param discretizacao Intervalo de tempo para a sequencia (e.g., "30 min", "1 hour")
#'
#' @return data.table com colunas de \code{data_hora_previsao}
#'   preenchida com sequencia completa
#'
#' @keywords internal
completa_datas <- function(dt, discretizacao) {
    dt_data_inicio <- dt[, .(data_inicio = as.Date(min(data_hora_previsao))), by = .(id_modelo_prev, id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_data_fim <- dt[, .(data_fim = as.Date(max(data_hora_previsao))), by = .(id_modelo_prev, id_modelo_nwp, id_usina, data_hora_rodada)]
    dt_datas_inicio_fim <- merge(dt_data_inicio, dt_data_fim, by = c("id_modelo_prev", "id_modelo_nwp", "id_usina", "data_hora_rodada"))
    
    dt_sequencia_datas <- dt_datas_inicio_fim[,
        .(data_hora_previsao = seq(as.POSIXct(data_inicio), as.POSIXct(data_fim), by = discretizacao)),
        by = .(id_modelo_prev, id_modelo_nwp, id_usina, data_hora_rodada)
    ]

    dt <- merge(dt, dt_sequencia_datas,
                 by = c("id_modelo_prev", "id_modelo_nwp", "id_usina", "data_hora_rodada", "data_hora_previsao"),
                 all.y = TRUE)

    return(dt)
}
