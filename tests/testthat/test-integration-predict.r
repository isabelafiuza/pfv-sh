run_predict_integration <- function(.env = parent.frame()) {
    temp_artifact <- withr::local_tempdir(.local_envir = .env)
    temp_output <- withr::local_tempdir(.local_envir = .env)
    conn <- pfvIO::conectamock_pfv(test_path("data"))

    config_train <- gen_config(mode = "train", ids_usinas = c("BAUFI1"))
    config_train$input <- normalizePath(test_path("data"))
    config_train$artifact <- temp_artifact
    config_train$output <- temp_artifact
    config_train <- parse_config(config_train, conn)

    train_main(config_train)

    config_predict <- gen_config(mode = "predict", ids_usinas = c("BAUFI1"))
    config_predict$input <- normalizePath(test_path("data"))
    config_predict$artifact <- temp_artifact
    config_predict$output <- temp_output
    config_predict <- parse_config(config_predict, conn)

    list(
        config_predict = config_predict,
        temp_artifact = temp_artifact,
        temp_output = temp_output
    )
}

test_that("predict_main completes without error after training", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_predict_integration()

    expect_no_error(predict_main(s$config_predict))
})

test_that("predict_main produces output parquet file", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_predict_integration()

    predict_main(s$config_predict)

    parquet_path <- file.path(s$temp_output, "previsao_geracao_fotovoltaica.parquet")
    expect_true(file.exists(parquet_path))

    dt <- data.table::setDT(arrow::read_parquet(parquet_path))
    expect_true(nrow(dt) > 0L)
    expected_cols <- c(
        "id_usina", "id_modelo_prev", "id_modelo_nwp",
        "data_hora_rodada", "data_hora_previsao", "valor"
    )
    expect_true(all(expected_cols %in% names(dt)))
})

test_that("predict_main writes provenance JSON with correct structure", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_predict_integration()

    predict_main(s$config_predict)

    prov_files <- list.files(
        s$temp_output,
        pattern = "^provenance-predict-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(prov_files), 1L)

    parsed <- jsonlite::fromJSON(prov_files[1], simplifyVector = FALSE)
    expect_equal(parsed$mode, "predict")
    expect_true(!is.null(parsed$run_id))
    expect_true(!is.null(parsed$status))
    expect_equal(parsed$n_plants, length(s$config_predict$ids_usinas))
})

test_that("predict_main writes metrics JSON with plant entries", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_predict_integration()

    predict_main(s$config_predict)

    metrics_files <- list.files(
        s$temp_output,
        pattern = "^metrics-predict-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(metrics_files), 1L)

    parsed <- jsonlite::fromJSON(metrics_files[1])
    expect_true(!is.null(parsed$plants))
    expect_true(length(parsed$plants) > 0L)
})

test_that("predict_main writes health JSON with overall_health field", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_predict_integration()

    predict_main(s$config_predict)

    health_files <- list.files(
        s$temp_output,
        pattern = "^health-predict-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(health_files), 1L)

    parsed <- jsonlite::fromJSON(health_files[1], simplifyVector = FALSE)
    expect_true(!is.null(parsed$overall_health))
    expect_true(parsed$overall_health %in% c("healthy", "degraded", "failed"))
})
