combina_media <- function(dt) {

    dt <- copy(dt)
    colorder <- names(dt)
    prev_combinada <- dt[
        ,
        .(id_modelo_prev = "combinado", valor = mean(valor, na.rm = TRUE)),
        by = .(id_usina, id_modelo_nwp, data_hora_rodada, data_hora_previsao)
    ]
    prev_combinada[is.nan(valor), valor := NA]
    prev_combinada[, dia_previsao := as.Date(data_hora_previsao)]
    prev_combinada[
        ,
        valor_max_dia := max(valor, na.rm = TRUE),
        by = .(id_usina, id_modelo_nwp, data_hora_rodada, dia_previsao)
    ]
    prev_combinada[, valor_suavizado := NA_real_]
    prev_combinada[
        !is.na(valor),
        valor_suavizado := suaviza_previsao(
            y = valor, x = data_hora_previsao,
            percent = 0.1, ymax = valor_max_dia, span = 0.5
        ),
        by = .(id_usina, id_modelo_nwp, data_hora_rodada, dia_previsao)
    ]

    prev_combinada[, c("dia_previsao", "valor_max_dia", "valor") := NULL]
    setnames(prev_combinada, "valor_suavizado", "valor")
    setcolorder(prev_combinada, colorder)

    dt_final <- rbind(dt, prev_combinada)

    return(dt_final)
}

suaviza_previsao <- function(y, x, percent, ymax, span) {
    id_val <- y > percent * ymax
    y_suavizado <- y
    mod <- loess(y[id_val] ~ as.numeric(x[id_val]), span = span)
    y_suavizado[id_val] <- predict(mod)

    return(y_suavizado)
}
