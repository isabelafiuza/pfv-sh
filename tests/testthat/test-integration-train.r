run_train_integration <- function(.env = parent.frame()) {
    temp_artifact <- withr::local_tempdir(.local_envir = .env)
    temp_output <- withr::local_tempdir(.local_envir = .env)
    conn <- pfvIO::conectamock_pfv(test_path("data"))
    config <- gen_config(mode = "train", ids_usinas = c("BAUFI1"))
    config$input <- normalizePath(test_path("data"))
    config$artifact <- temp_artifact
    config$output <- temp_output
    config <- parse_config(config, conn)
    list(config = config, temp_artifact = temp_artifact, temp_output = temp_output)
}

test_that("train_main completes without error", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_train_integration()

    expect_no_error(train_main(s$config))
})

test_that("train_main produces model artifact files for each usina", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_train_integration()

    train_main(s$config)

    expected_files <- file.path(
        s$temp_artifact,
        paste0(s$config$ids_usinas, "_modelos_ajustados.rds")
    )
    expect_true(all(file.exists(expected_files)))
})

test_that("train_main writes provenance JSON with correct structure", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_train_integration()

    train_main(s$config)

    prov_files <- list.files(
        s$temp_output,
        pattern = "^provenance-train-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(prov_files), 1L)

    parsed <- jsonlite::fromJSON(prov_files[1], simplifyVector = FALSE)
    expect_equal(parsed$mode, "train")
    expect_true(!is.null(parsed$run_id))
    expect_true(!is.null(parsed$status))
    expect_equal(parsed$n_plants, length(s$config$ids_usinas))
})

test_that("train_main writes metrics JSON with plant entries", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_train_integration()

    train_main(s$config)

    metrics_files <- list.files(
        s$temp_output,
        pattern = "^metrics-train-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(metrics_files), 1L)

    parsed <- jsonlite::fromJSON(metrics_files[1])
    expect_true(!is.null(parsed$plants))
    expect_true(length(parsed$plants) > 0L)
})

test_that("train_main writes health JSON with overall_health field", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    s <- run_train_integration()

    train_main(s$config)

    health_files <- list.files(
        s$temp_output,
        pattern = "^health-train-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(health_files), 1L)

    parsed <- jsonlite::fromJSON(health_files[1], simplifyVector = FALSE)
    expect_true(!is.null(parsed$overall_health))
    expect_true(parsed$overall_health %in% c("healthy", "degraded", "failed"))
})
