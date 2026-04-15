make_previsao_dt <- function(n = 4L) {
    data.table::data.table(
        id_usina = rep("USI1", n),
        id_modelo_prev = rep("arimax", n),
        id_modelo_nwp = rep("GFS", n),
        data_hora_rodada = rep(as.POSIXct("2025-07-01 00:00:00", tz = "UTC"), n),
        data_hora_previsao = as.POSIXct("2025-07-01 00:00:00", tz = "UTC") +
            seq(0L, (n - 1L) * 1800L, by = 1800L),
        valor = seq(0.0, by = 1.5, length.out = n)
    )
}

# TESTS FOR write_previsao_geracao_fotovoltaica --------------------------------

test_that("write_previsao_geracao_fotovoltaica", {
    f <- write_previsao_geracao_fotovoltaica
    expect_true(is.function(f))

    test_that("write_previsao_geracao_fotovoltaica delegates to write_dataset with correct filename", {
        out_dir <- withr::local_tempdir()
        dt <- make_previsao_dt()
        captured_args <- list()

        mockery::stub(
            f,
            "write_dataset",
            function(data, filename, dir, ...) {
                captured_args <<- list(data = data, filename = filename, dir = dir)
                invisible(NULL)
            }
        )

        f(dt, out_dir)

        expect_equal(captured_args$filename, "previsao_geracao_fotovoltaica.parquet")
    })

    test_that("write_previsao_geracao_fotovoltaica passes output_dir to write_dataset", {
        out_dir <- withr::local_tempdir()
        dt <- make_previsao_dt()
        captured_dir <- NULL

        mockery::stub(
            f,
            "write_dataset",
            function(data, filename, dir, ...) {
                captured_dir <<- dir
                invisible(NULL)
            }
        )

        f(dt, out_dir)

        expect_equal(captured_dir, out_dir)
    })

    test_that("write_previsao_geracao_fotovoltaica passes data unchanged to write_dataset", {
        out_dir <- withr::local_tempdir()
        dt <- make_previsao_dt()
        captured_data <- NULL

        mockery::stub(
            f,
            "write_dataset",
            function(data, filename, dir, ...) {
                captured_data <<- data
                invisible(NULL)
            }
        )

        f(dt, out_dir)

        expect_identical(captured_data, dt)
    })

    test_that("write_previsao_geracao_fotovoltaica returns invisibly", {
        out_dir <- withr::local_tempdir()
        dt <- make_previsao_dt()

        mockery::stub(f, "write_dataset", function(...) invisible(NULL))

        result <- f(dt, out_dir)

        expect_null(result)
    })
})
