make_train_config <- function(ids, artifact_dir, output_dir) {
    conn <- pfvIO::conectamock_pfv(test_path("data"))
    config <- gen_config(
        mode = "train",
        ids_usinas = ids
    )
    config$input <- normalizePath(test_path("data"))
    config$artifact <- artifact_dir
    config$output <- output_dir
    parse_config(config, conn)
}

make_predict_config <- function(ids, artifact_dir, output_dir) {
    conn <- pfvIO::conectamock_pfv(test_path("data"))
    config <- gen_config(
        mode = "predict",
        ids_usinas = ids
    )
    config$input <- normalizePath(test_path("data"))
    config$artifact <- artifact_dir
    config$output <- output_dir
    parse_config(config, conn)
}

artifact_path <- function(artifact_dir, id_usina) {
    file.path(artifact_dir, paste0(id_usina, "_modelos_ajustados.rds"))
}

# TESTS FOR train_main resume --------------------------------------------------

test_that("train_main resume skips completed plants", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output)
    plant_ids <- config$ids_usinas
    expect_true(length(plant_ids) >= 2L)

    prov <- create_provenance(config, "train", FALSE)
    update_plant_status(prov, plant_ids[1], "completed")
    write_checkpoint(prov, tmp_output)

    artifact_before <- gen_model_artifact(id_usina = plant_ids[1])
    saveRDS(artifact_before, artifact_path(tmp_artifact, plant_ids[1]))

    train_main(config, resume = TRUE)

    artifact_after <- readRDS(artifact_path(tmp_artifact, plant_ids[1]))
    expect_equal(artifact_after$id_usina, artifact_before$id_usina)
    expect_equal(length(artifact_after$models), length(artifact_before$models))

    expect_true(file.exists(artifact_path(tmp_artifact, plant_ids[2])))
    artifact_second <- readRDS(artifact_path(tmp_artifact, plant_ids[2]))
    expect_equal(artifact_second$id_usina, plant_ids[2])
})

test_that("train_main resume with config mismatch runs from scratch", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output)
    plant_ids <- config$ids_usinas

    config_other <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output)
    config_other$data_referencia <- as.Date("2020-01-01")

    prov_other <- create_provenance(config_other, "train", FALSE)
    update_plant_status(prov_other, plant_ids[1], "completed")
    write_checkpoint(prov_other, tmp_output)

    expect_no_error(train_main(config, resume = TRUE))

    for (iu in plant_ids) {
        expect_true(file.exists(artifact_path(tmp_artifact, iu)))
    }
})

test_that("train_main cleans up checkpoint after successful resume run", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config <- make_train_config(list("BAUFI1"), tmp_artifact, tmp_output)

    train_main(config, resume = TRUE)

    cp_files <- list.files(tmp_output, pattern = "^checkpoint-.*\\.json$")
    expect_equal(length(cp_files), 0L)

    prov_files <- list.files(tmp_output, pattern = "^provenance-train-.*\\.json$")
    expect_equal(length(prov_files), 1L)
})

test_that("train_main resume with no existing checkpoint runs from scratch", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output)

    expect_no_error(train_main(config, resume = TRUE))

    for (iu in config$ids_usinas) {
        expect_true(file.exists(artifact_path(tmp_artifact, iu)))
    }
})

# TESTS FOR predict_main resume ------------------------------------------------

test_that("predict_main resume combines completed and new plant results", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config_train <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_artifact)
    train_main(config_train)

    config_predict <- make_predict_config(
        list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output
    )
    plant_ids <- config_predict$ids_usinas
    expect_true(length(plant_ids) >= 2L)

    artifact_path_1 <- file.path(
        tmp_artifact, paste0(plant_ids[1], "_modelos_ajustados.rds")
    )
    artifact_1 <- readRDS(artifact_path_1)
    models_1 <- if (!is.null(artifact_1$models)) artifact_1$models else artifact_1

    saved_result <- lapply(models_1, function(m) {
        list(combinacao_ajuste = m$combinacao_ajuste, prev = NA_real_)
    })
    write_plant_result(saved_result, plant_ids[1], tmp_output)

    prov_resume <- create_provenance(config_predict, "predict", FALSE)
    update_plant_status(prov_resume, plant_ids[1], "completed")
    write_checkpoint(prov_resume, tmp_output)

    expect_no_error(predict_main(config_predict, resume = TRUE))

    parquet_resume <- file.path(tmp_output, "previsao_geracao_fotovoltaica.parquet")
    expect_true(file.exists(parquet_resume))

    dt_resume <- data.table::setDT(arrow::read_parquet(parquet_resume))
    ids_in_output <- unique(dt_resume$id_usina)
    expect_true(all(plant_ids %in% ids_in_output))
})

test_that("predict_main cleans up checkpoint and plant-result files after successful resume", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config_train <- make_train_config(list("BAUFI1"), tmp_artifact, tmp_artifact)
    train_main(config_train)

    config_predict <- make_predict_config(list("BAUFI1"), tmp_artifact, tmp_output)

    predict_main(config_predict, resume = TRUE)

    cp_files <- list.files(tmp_output, pattern = "^checkpoint-.*\\.json$")
    expect_equal(length(cp_files), 0L)

    pr_files <- list.files(tmp_output, pattern = "^plant-result-.*\\.rds$")
    expect_equal(length(pr_files), 0L)

    prov_files <- list.files(tmp_output, pattern = "^provenance-predict-.*\\.json$")
    expect_equal(length(prov_files), 1L)
})

test_that("predict_main resume with missing plant-result file reprocesses that plant", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    tmp_artifact <- withr::local_tempdir()
    tmp_output <- withr::local_tempdir()

    config_train <- make_train_config(list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_artifact)
    train_main(config_train)

    config_predict <- make_predict_config(
        list("BAUFI1", "BAUFI2"), tmp_artifact, tmp_output
    )
    plant_ids <- config_predict$ids_usinas

    prov <- create_provenance(config_predict, "predict", FALSE)
    update_plant_status(prov, plant_ids[1], "completed")
    write_checkpoint(prov, tmp_output)

    expect_no_error(predict_main(config_predict, resume = TRUE))

    parquet_path <- file.path(tmp_output, "previsao_geracao_fotovoltaica.parquet")
    dt_result <- data.table::setDT(arrow::read_parquet(parquet_path))
    expect_true(all(plant_ids %in% unique(dt_result$id_usina)))
})
