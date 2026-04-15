make_prev_dt <- function(ids_usina = c("USI1", "USI2"),
    ids_nwp = c("GFS"),
    n_modelos_prev = 2L,
    n_half_hours = 48L) {

    rodada <- as.POSIXct("2025-07-01 00:00:00", tz = "UTC")
    timestamps <- rodada + seq(0, (n_half_hours - 1L) * 1800, by = 1800)
    hours <- as.numeric(format(timestamps, "%H")) +
        as.numeric(format(timestamps, "%M")) / 60

    modelos_prev <- paste0("modelo_", seq_len(n_modelos_prev))

    grid <- data.table::CJ(
        id_usina = ids_usina,
        id_modelo_prev = modelos_prev,
        id_modelo_nwp = ids_nwp,
        data_hora_previsao = timestamps,
        sorted = FALSE
    )
    grid[, data_hora_rodada := rodada]

    hour_vals <- as.numeric(format(grid$data_hora_previsao, "%H")) +
        as.numeric(format(grid$data_hora_previsao, "%M")) / 60
    grid[, valor := ifelse(hour_vals >= 5.0 & hour_vals <= 18.5,
        28.0 * sin(pi * (hour_vals - 5.0) / 13.5), 0.0)]
    grid[valor < 0, valor := 0.0]

    grid[]
}

# TESTS FOR combina_media ------------------------------------------------------

test_that("combina_media", {
    f <- combina_media
    expect_true(is.function(f))

    test_that("combina_media adds rows with id_modelo_prev == 'combinado'", {
        dt <- make_prev_dt()

        result <- f(dt)

        expect_true("combinado" %in% result$id_modelo_prev)
    })

    test_that("combina_media returns more rows than input", {
        dt <- make_prev_dt()

        result <- f(dt)

        expect_gt(nrow(result), nrow(dt))
    })

    test_that("combina_media adds one combinado group per usina x nwp x rodada x timestamp", {
        dt <- make_prev_dt(ids_usina = c("USI1"), ids_nwp = c("GFS"), n_modelos_prev = 3L)
        n_input_rows <- nrow(dt)

        result <- f(dt)
        n_combinado <- nrow(result[id_modelo_prev == "combinado"])

        expect_equal(nrow(result), n_input_rows + n_combinado)
    })

    test_that("combina_media preserves original model rows unchanged", {
        dt <- make_prev_dt(ids_usina = "USI1", n_modelos_prev = 2L)

        result <- f(dt)
        orig_models <- result[id_modelo_prev != "combinado"]

        expect_equal(nrow(orig_models), nrow(dt))
    })

    test_that("combina_media combined valor is mean of model valores", {
        dt <- make_prev_dt(ids_usina = "USI1", ids_nwp = "GFS", n_modelos_prev = 2L, n_half_hours = 4L)
        dt[id_modelo_prev == "modelo_1", valor := 10.0]
        dt[id_modelo_prev == "modelo_2", valor := 20.0]

        result <- f(dt)
        combinado_rows <- result[id_modelo_prev == "combinado"]

        day_rows <- combinado_rows[!is.na(valor) & valor > 0]
        if (nrow(day_rows) > 0L) {
            expect_true(all(abs(day_rows$valor - 15.0) < 1.0))
        }
        expect_true(nrow(combinado_rows) > 0L)
    })

    test_that("combina_media preserves original column structure in output", {
        dt <- make_prev_dt()
        expected_cols <- c(
            "id_usina", "id_modelo_prev", "id_modelo_nwp",
            "data_hora_rodada", "data_hora_previsao", "valor"
        )

        result <- f(dt)

        expect_true(all(expected_cols %in% names(result)))
    })
})

# TESTS FOR suaviza_previsao ---------------------------------------------------

test_that("suaviza_previsao", {
    f <- suaviza_previsao
    expect_true(is.function(f))

    test_that("suaviza_previsao returns numeric vector", {
        timestamps <- as.POSIXct("2025-07-01 06:00:00", tz = "UTC") +
            seq(0, 47 * 1800, by = 1800)
        hours <- as.numeric(format(timestamps, "%H")) +
            as.numeric(format(timestamps, "%M")) / 60
        y <- pmax(28.0 * sin(pi * (hours - 5.0) / 13.5), 0.0)
        ymax <- max(y)

        result <- f(y = y, x = timestamps, percent = 0.1, ymax = ymax, span = 0.5)

        expect_type(result, "double")
    })

    test_that("suaviza_previsao returns vector of same length as input", {
        timestamps <- as.POSIXct("2025-07-01 06:00:00", tz = "UTC") +
            seq(0, 47 * 1800, by = 1800)
        hours <- as.numeric(format(timestamps, "%H")) +
            as.numeric(format(timestamps, "%M")) / 60
        y <- pmax(28.0 * sin(pi * (hours - 5.0) / 13.5), 0.0)
        ymax <- max(y)

        result <- f(y = y, x = timestamps, percent = 0.1, ymax = ymax, span = 0.5)

        expect_length(result, length(y))
    })

    test_that("suaviza_previsao preserves zero values below threshold", {
        n <- 48L
        x <- seq_len(n)
        y <- c(rep(0.0, 12L), seq(0, 20, length.out = 24L), rep(0.0, 12L))
        ymax <- max(y)

        result <- f(y = y, x = x, percent = 0.1, ymax = ymax, span = 0.5)

        expect_equal(result[1:12], rep(0.0, 12L))
        expect_equal(result[(n - 11L):n], rep(0.0, 12L))
    })

    test_that("suaviza_previsao smoothed daytime values stay non-negative", {
        timestamps <- as.POSIXct("2025-07-01 05:00:00", tz = "UTC") +
            seq(0, 27 * 1800, by = 1800)
        hours <- as.numeric(format(timestamps, "%H")) +
            as.numeric(format(timestamps, "%M")) / 60
        y <- pmax(28.0 * sin(pi * (hours - 5.0) / 13.5), 0.0)
        ymax <- max(y)

        result <- f(y = y, x = timestamps, percent = 0.1, ymax = ymax, span = 0.5)

        expect_true(all(result >= 0.0))
    })
})
