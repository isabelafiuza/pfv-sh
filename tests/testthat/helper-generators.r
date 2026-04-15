gen_config <- function(mode = "train", ...) {
    defaults <- list(
        mode = mode,
        input = "./data",
        output = "./out",
        artifact = ".",
        data_referencia = "2025-07-02",
        horizonte_dias = c("D+0", "D+1"),
        modelos_NWP = c("GFS"),
        modelos_previsao = list(
            arimax = list(tipo = "arimax", n_dias_treino = 360L, amos_min = 5L)
        ),
        parametros_periodo_geracao = list(
            fator_tolerancia_limite_superior_geracao = 1.1,
            fator_tolerancia_limite_inferior_geracao = 0.01,
            percentual_dias_geracao = 0.9
        ),
        ids_usinas = c("USI1")
    )
    overrides <- list(...)
    defaults[names(overrides)] <- overrides
    defaults
}

gen_default_datas <- function(n_days = 7L, start = "2025-07-01") {
    start_posix <- as.POSIXct(start, tz = "UTC")
    end_posix <- start_posix + (n_days * 86400) - 1800
    seq(start_posix, end_posix, by = "30 mins")
}

gen_usinas <- function(n = 2L, ids = NULL) {
    stopifnot(is.null(ids) || is.character(ids))
    if (is.null(ids)) ids <- paste0("USI", seq_len(n))
    n <- length(ids)
    data.table::data.table(
        id_usina = ids,
        latitude = seq(-12, -25, length.out = n),
        longitude = seq(-38, -50, length.out = n),
        capacidade_instalada_MW = rep(28.0, n),
        data_inicio_operacao_comercial = rep(
            as.POSIXct("2017-08-05", tz = "UTC"), n
        )
    )
}

gen_solar_curve <- function(hours, capacity = 28.0) {
    solar_val <- ifelse(
        hours >= 5.0 & hours <= 18.5,
        capacity * sin(pi * (hours - 5.0) / 13.5),
        0.0
    )
    pmax(solar_val, 0.0)
}

gen_geracao_observada <- function(ids = "USI1", datas = NULL,
    fontes = c("Consis"), pattern = "normal", capacity = 28.0, seed = 42L) {

    stopifnot(is.character(ids))
    valid_patterns <- c("normal", "frozen", "all_na")
    stopifnot(pattern %in% valid_patterns)
    if (is.null(datas)) datas <- gen_default_datas()

    grid <- data.table::CJ(
        id_fonte_observacao = fontes,
        id_usina = ids,
        data_hora_observacao = datas,
        sorted = FALSE
    )

    hours <- data.table::hour(grid$data_hora_observacao) +
        data.table::minute(grid$data_hora_observacao) / 60

    grid[, valor := gen_solar_curve(hours, capacity)]
    grid[, status := 1L]

    gen_geracao_apply_pattern(grid, pattern, seed)
}

gen_geracao_apply_pattern <- function(dt, pattern, seed, ...) {
    mc <- match.call()
    fun <- as.name(paste0("gen_geracao_apply_pattern_", pattern))
    mc[[1]] <- fun
    eval(mc, parent.frame())
}

gen_geracao_apply_pattern_normal <- function(dt, pattern, seed, ...) {
    set.seed(seed)
    dt[valor > 0, valor := valor * (1 + stats::rnorm(.N, 0, 0.05))]
    dt[valor < 0, valor := 0]
    dt[]
}

gen_geracao_apply_pattern_frozen <- function(dt, pattern, seed, ...) {
    is_day <- data.table::hour(dt$data_hora_observacao) >= 5 &
        data.table::hour(dt$data_hora_observacao) <= 18
    dt[is_day, valor := 10.0]
    dt[!is_day, valor := 0.0]
    dt[]
}

gen_geracao_apply_pattern_all_na <- function(dt, pattern, seed, ...) {
    dt[, valor := NA_real_]
    dt[]
}

gen_irradiancia_prevista <- function(ids = "USI1", datas = NULL,
    modelo_nwp = "GFS") {

    stopifnot(is.character(ids))
    if (is.null(datas)) datas <- gen_default_datas()

    usinas_ref <- gen_usinas(n = length(ids), ids = ids)

    grids <- lapply(seq_along(ids), function(i) {
        usi <- usinas_ref[i]
        data.table::data.table(
            id_modelo_nwp = modelo_nwp,
            latitude = usi$latitude,
            longitude = usi$longitude,
            data_hora_rodada = datas,
            data_hora_previsao = datas,
            valor = gen_irrad_curve(datas)
        )
    })

    data.table::rbindlist(grids)
}

gen_irrad_curve <- function(datas) {
    hours <- data.table::hour(datas) +
        data.table::minute(datas) / 60
    ifelse(
        hours >= 5.0 & hours <= 18.5,
        1000 * sin(pi * (hours - 5.0) / 13.5),
        0.0
    )
}

gen_model_artifact <- function(id_usina = "USI1", n = 2L) {
    models <- replicate(n, gen_model_entry(), simplify = FALSE)
    config <- gen_config(ids_usinas = id_usina)
    build_model_artifact(id_usina, models, config)
}

gen_model_entry <- function(modelo_prev = "arimax") {
    list(
        combinacao_ajuste = list(
            id_modelo_nwp = "GFS",
            horiz_prev = 1L,
            hora_min = "12:00",
            modelo_prev = modelo_prev
        ),
        modelo = list(
            modelo_escolhido = modelo_prev,
            modelo_final = NULL,
            variaveis_usadas = character(0L)
        )
    )
}
