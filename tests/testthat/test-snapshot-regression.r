make_train_config_snap <- function(artifact_dir, output_dir) {
    conn <- pfvIO::conectamock_pfv(test_path("data"))
    config <- gen_config(mode = "train", ids_usinas = list("BAUFI1"))
    config$input <- normalizePath(test_path("data"))
    config$artifact <- artifact_dir
    config$output <- output_dir
    parse_config(config, conn)
}

make_predict_config_snap <- function(artifact_dir, output_dir) {
    conn <- pfvIO::conectamock_pfv(test_path("data"))
    config <- gen_config(mode = "predict", ids_usinas = list("BAUFI1"))
    config$input <- normalizePath(test_path("data"))
    config$artifact <- artifact_dir
    config$output <- output_dir
    parse_config(config, conn)
}

strip_nondeterministic_prov_fields <- function(prov_list) {
    prov_list$run_id <- NULL
    prov_list$start_time <- NULL
    prov_list$end_time <- NULL
    prov_list$duration_seconds <- NULL
    prov_list$r_version <- NULL
    prov_list$package_version <- NULL
    prov_list
}

# TESTS FOR provenance JSON snapshot -------------------------------------------

test_that("snapshot_provenance_train_structure", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    temp_artifact <- withr::local_tempdir()
    temp_output <- withr::local_tempdir()

    config <- make_train_config_snap(temp_artifact, temp_output)

    train_main(config)

    prov_files <- list.files(
        temp_output,
        pattern = "^provenance-train-.*\\.json$",
        full.names = TRUE
    )
    expect_equal(length(prov_files), 1L)

    prov_raw <- jsonlite::fromJSON(prov_files[1], simplifyVector = FALSE)
    prov_stripped <- strip_nondeterministic_prov_fields(prov_raw)

    snapshot_repr <- list(
        field_names = sort(names(prov_stripped)),
        mode = prov_stripped$mode,
        status = prov_stripped$status,
        n_plants = prov_stripped$n_plants,
        plant_ids = sort(unlist(prov_stripped$plant_ids)),
        plant_status_keys = sort(names(prov_stripped$plant_status)),
        plant_status_values = unname(
            vapply(
                sort(names(prov_stripped$plant_status)),
                function(k) prov_stripped$plant_status[[k]],
                character(1L)
            )
        ),
        parallel = prov_stripped$parallel
    )

    expect_snapshot_value(snapshot_repr, style = "json2")
})

test_that("snapshot_predict_output_structure", {
    skip_if_not(dir.exists(test_path("data")))
    skip_if_no_zstd()
    temp_artifact <- withr::local_tempdir()
    temp_output <- withr::local_tempdir()

    config_train <- make_train_config_snap(temp_artifact, temp_artifact)
    train_main(config_train)

    config_predict <- make_predict_config_snap(temp_artifact, temp_output)
    predict_main(config_predict)

    parquet_path <- file.path(temp_output, "previsao_geracao_fotovoltaica.parquet")
    expect_true(file.exists(parquet_path))

    dt <- data.table::setDT(arrow::read_parquet(parquet_path))

    for (iu in config_predict$ids_usinas) {
        dt_plant <- dt[id_usina == iu]

        snapshot_repr <- list(
            id_usina = iu,
            col_names = sort(names(dt_plant)),
            col_types = vapply(
                sort(names(dt_plant)),
                function(col) class(dt_plant[[col]])[1L],
                character(1L)
            ),
            n_rows = nrow(dt_plant),
            n_rows_per_modelo_prev = as.list(
                sort(table(dt_plant$id_modelo_prev))
            ),
            n_rows_per_modelo_nwp = as.list(
                sort(table(dt_plant$id_modelo_nwp))
            )
        )

        expect_snapshot_value(snapshot_repr, style = "json2")
    }
})
