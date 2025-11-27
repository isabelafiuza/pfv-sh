# TESTS FOR define_hor_prev ----------------------------------------------------

test_that("define_hor_prev returns correct dates for standard horizons", {
    result <- define_hor_prev("2025-11-07", c("D+0", "D+1"))
    expect_s3_class(result, "Date")
    expect_length(result, 2)
    expect_equal(result[1], as.Date("2025-11-07"))
    expect_equal(result[2], as.Date("2025-11-08"))
})

test_that("define_hor_prev handles single horizon", {
    result <- define_hor_prev("2025-11-07", c("D+0"))
    expect_length(result, 1)
    expect_equal(result[1], as.Date("2025-11-07"))
})

test_that("define_hor_prev handles multiple horizons", {
    result <- define_hor_prev("2025-01-01", c("D+0", "D+1", "D+2", "D+3"))
    expect_length(result, 4)
    expect_equal(result[1], as.Date("2025-01-01"))
    expect_equal(result[4], as.Date("2025-01-04"))
})

test_that("define_hor_prev accepts Date input", {
    result <- define_hor_prev(as.Date("2025-11-07"), c("D+0", "D+1"))
    expect_equal(result[1], as.Date("2025-11-07"))
    expect_equal(result[2], as.Date("2025-11-08"))
})

test_that("define_hor_prev returns empty vector for empty horizons", {
    result <- define_hor_prev("2025-11-07", character(0))
    expect_length(result, 0)
})

# TESTS FOR adicionar_passo_previsao -------------------------------------------

test_that("adicionar_passo_previsao adds passo_prev column", {
    dt <- data.table(
        data_hora_rodada = as.POSIXct(c("2025-01-01", "2025-01-01"), tz = "UTC"),
        data_hora_previsao = as.POSIXct(c("2025-01-01 12:00:00", "2025-01-02 12:00:00"), tz = "UTC")
    )
    result <- adicionar_passo_previsao(dt)
    expect_true("passo_prev" %in% names(result))
})

test_that("adicionar_passo_previsao computes correct D+0", {
    dt <- data.table(
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct("2025-01-01 12:00:00", tz = "UTC")
    )
    result <- adicionar_passo_previsao(dt)
    expect_equal(result$passo_prev, "D+0")
})

test_that("adicionar_passo_previsao computes correct D+1", {
    dt <- data.table(
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct("2025-01-02 12:00:00", tz = "UTC")
    )
    result <- adicionar_passo_previsao(dt)
    expect_equal(result$passo_prev, "D+1")
})

test_that("adicionar_passo_previsao handles multiple days", {
    dt <- data.table(
        data_hora_rodada = as.POSIXct(rep("2025-01-01", 3), tz = "UTC"),
        data_hora_previsao = as.POSIXct(c(
            "2025-01-01 12:00:00",
            "2025-01-02 12:00:00",
            "2025-01-05 12:00:00"
        ), tz = "UTC")
    )
    result <- adicionar_passo_previsao(dt)
    expect_equal(result$passo_prev, c("D+0", "D+1", "D+4"))
})

# TESTS FOR cria_sequencia_datas -----------------------------------------------

test_that("cria_sequencia_datas creates complete sequence", {
    dt <- data.table(
        id_modelo_nwp = "GFS",
        id_usina = "USI1",
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct(c("2025-01-01 00:00:00", "2025-01-01 01:00:00"), tz = "UTC")
    )
    result <- cria_sequencia_datas(dt, "30 min")
    expect_s3_class(result, "data.table")
    expect_true("data_hora_previsao" %in% names(result))
    expect_equal(nrow(result), 3) # 00:00, 00:30, 01:00
})

test_that("cria_sequencia_datas preserves grouping columns", {
    dt <- data.table(
        id_modelo_nwp = "GFS",
        id_usina = "USI1",
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct(c("2025-01-01 00:00:00", "2025-01-01 02:00:00"), tz = "UTC")
    )
    result <- cria_sequencia_datas(dt, "1 hour")
    expect_true(all(c("id_modelo_nwp", "id_usina", "data_hora_rodada") %in% names(result)))
})

test_that("cria_sequencia_datas handles multiple groups", {
    dt <- data.table(
        id_modelo_nwp = c("GFS", "GFS", "ECMWF", "ECMWF"),
        id_usina = c("USI1", "USI1", "USI1", "USI1"),
        data_hora_rodada = as.POSIXct(c("2025-01-01", "2025-01-01", "2025-01-01", "2025-01-01"), tz = "UTC"),
        data_hora_previsao = as.POSIXct(c(
            "2025-01-01 00:00:00", "2025-01-01 01:00:00",
            "2025-01-01 00:00:00", "2025-01-01 01:00:00"
        ), tz = "UTC")
    )
    result <- cria_sequencia_datas(dt, "30 min")
    # Each group should have 3 rows (00:00, 00:30, 01:00)
    expect_equal(nrow(result), 6)
})

# TESTS FOR interpola_serie_temporal -------------------------------------------

test_that("interpola_serie_temporal fills gaps with linear interpolation", {
    x <- c(1, NA, 3)
    result <- interpola_serie_temporal(x)
    expect_equal(result[2], 2)
})

test_that("interpola_serie_temporal handles multiple consecutive NAs", {
    x <- c(1, NA, NA, NA, 5)
    result <- interpola_serie_temporal(x)
    expect_equal(result, c(1, 2, 3, 4, 5))
})

test_that("interpola_serie_temporal preserves leading NAs", {
    x <- c(NA, NA, 3, 4, 5)
    result <- interpola_serie_temporal(x)
    expect_true(is.na(result[1]))
    expect_true(is.na(result[2]))
})

test_that("interpola_serie_temporal preserves trailing NAs", {
    x <- c(1, 2, 3, NA, NA)
    result <- interpola_serie_temporal(x)
    expect_true(is.na(result[4]))
    expect_true(is.na(result[5]))
})

test_that("interpola_serie_temporal handles no NAs", {
    x <- c(1, 2, 3, 4, 5)
    result <- interpola_serie_temporal(x)
    expect_equal(result, x)
})

test_that("interpola_serie_temporal handles all NAs", {
    x <- c(NA, NA, NA)
    result <- interpola_serie_temporal(x)
    expect_true(all(is.na(result)))
})

# TESTS FOR gera_combinacoes_modelo --------------------------------------------

test_that("gera_combinacoes_modelo generates all combinations", {
    v_nwp <- c("GFS", "ECMWF")
    v_horiz <- c("D+0", "D+1")
    v_hor_ger <- c("12:00", "12:30")
    v_modelos <- "arimax"

    result <- gera_combinacoes_modelo(v_nwp, v_horiz, v_hor_ger, v_modelos)

    # 2 NWP * 2 horizons * 2 hours * 1 model = 8 combinations
    expect_type(result, "list")
    expect_length(result, 8)
})

test_that("gera_combinacoes_modelo includes all required fields", {
    v_nwp <- c("GFS")
    v_horiz <- c("D+0")
    v_hor_ger <- c("12:00")
    v_modelos <- "arimax"

    result <- gera_combinacoes_modelo(v_nwp, v_horiz, v_hor_ger, v_modelos)

    first_elem <- result[[1]]
    expect_true("id_modelo_nwp" %in% names(first_elem))
    expect_true("horiz_prev" %in% names(first_elem))
    expect_true("hora_min" %in% names(first_elem))
    expect_true("modelo_prev" %in% names(first_elem))
})

test_that("gera_combinacoes_modelo returns list of lists", {
    v_nwp <- c("GFS")
    v_horiz <- c("D+0")
    v_hor_ger <- c("12:00")
    v_modelos <- "arimax"

    result <- gera_combinacoes_modelo(v_nwp, v_horiz, v_hor_ger, v_modelos)

    expect_type(result[[1]], "list")
})

test_that("gera_combinacoes_modelo handles multiple models", {
    v_nwp <- c("GFS")
    v_horiz <- c("D+0")
    v_hor_ger <- c("12:00")
    v_modelos <- c("arimax", "fisico_estimado")

    result <- gera_combinacoes_modelo(v_nwp, v_horiz, v_hor_ger, v_modelos)

    expect_length(result, 2)
})

# TESTS FOR monta_dt_prev ------------------------------------------------------

test_that("monta_dt_prev creates data.table with correct structure", {
    prev_elem <- list(
        combinacao_ajuste = list(
            modelo_prev = "arimax",
            id_modelo_nwp = "GFS",
            horiz_prev = "D+0",
            hora_min = "12:00"
        ),
        prev = 15.5
    )

    result <- monta_dt_prev(prev_elem, "USI1", as.Date("2025-01-01"))

    expect_s3_class(result, "data.table")
    expect_true(all(c(
        "id_modelo_prev", "id_usina", "id_modelo_nwp",
        "data_hora_rodada", "data_hora_previsao", "valor"
    ) %in% names(result)))
})

test_that("monta_dt_prev computes correct data_hora_previsao for D+0", {
    prev_elem <- list(
        combinacao_ajuste = list(
            modelo_prev = "arimax",
            id_modelo_nwp = "GFS",
            horiz_prev = "D+0",
            hora_min = "12:00"
        ),
        prev = 15.5
    )

    result <- monta_dt_prev(prev_elem, "USI1", as.Date("2025-01-01"))

    expected_datetime <- as.POSIXct("2025-01-01 12:00:00", tz = "UTC")
    expect_equal(result$data_hora_previsao, expected_datetime)
})

test_that("monta_dt_prev computes correct data_hora_previsao for D+1", {
    prev_elem <- list(
        combinacao_ajuste = list(
            modelo_prev = "arimax",
            id_modelo_nwp = "GFS",
            horiz_prev = "D+1",
            hora_min = "14:30"
        ),
        prev = 20.0
    )

    result <- monta_dt_prev(prev_elem, "USI1", as.Date("2025-01-01"))

    expected_datetime <- as.POSIXct("2025-01-02 14:30:00", tz = "UTC")
    expect_equal(result$data_hora_previsao, expected_datetime)
})

test_that("monta_dt_prev preserves id_usina", {
    prev_elem <- list(
        combinacao_ajuste = list(
            modelo_prev = "arimax",
            id_modelo_nwp = "GFS",
            horiz_prev = "D+0",
            hora_min = "12:00"
        ),
        prev = 15.5
    )

    result <- monta_dt_prev(prev_elem, "BAUFI1", as.Date("2025-01-01"))

    expect_equal(result$id_usina, "BAUFI1")
})

test_that("monta_dt_prev converts valor to numeric", {
    prev_elem <- list(
        combinacao_ajuste = list(
            modelo_prev = "arimax",
            id_modelo_nwp = "GFS",
            horiz_prev = "D+0",
            hora_min = "12:00"
        ),
        prev = "15.5" # String input
    )

    result <- monta_dt_prev(prev_elem, "USI1", as.Date("2025-01-01"))

    expect_type(result$valor, "double")
    expect_equal(result$valor, 15.5)
})

# TESTS FOR associa_nwp_usina --------------------------------------------------

test_that("associa_nwp_usina associates closest grid point to plant", {
    dt_prev <- data.table(
        id_modelo_nwp = "GFS",
        latitude = c(-12.75, -12.5),
        longitude = c(-44.75, -44.5),
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct("2025-01-01 12:00:00", tz = "UTC"),
        valor = c(100, 200)
    )

    dt_usinas <- data.table(
        id_usina = "USI1",
        latitude = -12.6, # Closer to -12.5
        longitude = -44.6 # Closer to -44.5
    )

    result <- associa_nwp_usina(dt_prev, dt_usinas)

    expect_s3_class(result, "data.table")
    expect_true("id_usina" %in% names(result))
    expect_equal(result$id_usina, "USI1")
    # Should select the closest point (-12.5, -44.5) with valor 200
    expect_equal(result$valor, 200)
})

test_that("associa_nwp_usina handles multiple plants", {
    dt_prev <- data.table(
        id_modelo_nwp = "GFS",
        latitude = c(-12.75, -12.5, -12.0),
        longitude = c(-44.75, -44.5, -44.0),
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct("2025-01-01 12:00:00", tz = "UTC"),
        valor = c(100, 200, 300)
    )

    dt_usinas <- data.table(
        id_usina = c("USI1", "USI2"),
        latitude = c(-12.7, -12.1),
        longitude = c(-44.7, -44.1)
    )

    result <- associa_nwp_usina(dt_prev, dt_usinas)

    expect_equal(length(unique(result$id_usina)), 2)
})

test_that("associa_nwp_usina preserves id_usina column order", {
    dt_prev <- data.table(
        id_modelo_nwp = "GFS",
        latitude = -12.75,
        longitude = -44.75,
        data_hora_rodada = as.POSIXct("2025-01-01", tz = "UTC"),
        data_hora_previsao = as.POSIXct("2025-01-01 12:00:00", tz = "UTC"),
        valor = 100
    )

    dt_usinas <- data.table(
        id_usina = "USI1",
        latitude = -12.75,
        longitude = -44.75
    )

    result <- associa_nwp_usina(dt_prev, dt_usinas)

    # id_modelo_nwp should be first, id_usina should be second
    expect_equal(names(result)[1], "id_modelo_nwp")
    expect_equal(names(result)[2], "id_usina")
})

# TESTS FOR identifica_periodo_ger ---------------------------------------------

test_that("identifica_periodo_ger returns character vector of times", {
    # Create mock generation data with solar pattern
    dates <- seq(
        as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2025-01-01 23:30:00", tz = "UTC"),
        by = "30 min"
    )

    # Create 10 days of data
    all_dates <- as.POSIXct(
        unlist(
            lapply(0:9, function(d) as.numeric(dates) + d * 86400)
        ),
        origin = "1970-01-01", tz = "UTC"
    )

    # Solar generation pattern: 0 at night, high during day
    hour <- as.numeric(format(all_dates, "%H"))
    generation <- ifelse(hour >= 6 & hour <= 17, 10, 0)

    ger_usi <- data.table(
        data_hora_observacao = all_dates,
        valor = generation
    )

    dad_usina <- data.table(
        id_usina = "USI1",
        capacidade_instalada_MW = 28
    )

    result <- identifica_periodo_ger(
        dad_usina = dad_usina,
        ger_usi = ger_usi,
        fator_tol_ger = 0.01,
        fator_tol_horas = 0.5
    )

    expect_type(result, "character")
})

test_that("identifica_periodo_ger returns sorted times", {
    dates <- seq(
        as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2025-01-01 23:30:00", tz = "UTC"),
        by = "30 min"
    )

    all_dates <- as.POSIXct(
        unlist(
            lapply(0:9, function(d) as.numeric(dates) + d * 86400)
        ),
        origin = "1970-01-01", tz = "UTC"
    )

    hour <- as.numeric(format(all_dates, "%H"))
    generation <- ifelse(hour >= 6 & hour <= 17, 10, 0)

    ger_usi <- data.table(
        data_hora_observacao = all_dates,
        valor = generation
    )

    dad_usina <- data.table(
        id_usina = "USI1",
        capacidade_instalada_MW = 28
    )

    result <- identifica_periodo_ger(
        dad_usina = dad_usina,
        ger_usi = ger_usi,
        fator_tol_ger = 0.01,
        fator_tol_horas = 0.5
    )

    # Check that result is sorted
    expect_equal(result, sort(result))
})

test_that("identifica_periodo_ger excludes night hours", {
    dates <- seq(
        as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
        as.POSIXct("2025-01-01 23:30:00", tz = "UTC"),
        by = "30 min"
    )

    all_dates <- as.POSIXct(
        unlist(
            lapply(0:9, function(d) as.numeric(dates) + d * 86400)
        ),
        origin = "1970-01-01", tz = "UTC"
    )

    hour <- as.numeric(format(all_dates, "%H"))
    # Only generate during 10:00-14:00
    generation <- ifelse(hour >= 10 & hour <= 14, 10, 0)

    ger_usi <- data.table(
        data_hora_observacao = all_dates,
        valor = generation
    )

    dad_usina <- data.table(
        id_usina = "USI1",
        capacidade_instalada_MW = 28
    )

    result <- identifica_periodo_ger(
        dad_usina = dad_usina,
        ger_usi = ger_usi,
        fator_tol_ger = 0.01,
        fator_tol_horas = 0.5
    )

    # Should not include early morning or late evening hours
    expect_false("00:00" %in% result)
    expect_false("05:00" %in% result)
    expect_false("20:00" %in% result)
})
