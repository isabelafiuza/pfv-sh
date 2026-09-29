make_model_entry <- function(escolhido = "arimax") {
    list(
        combinacao_ajuste = list(
            id_modelo_nwp = "GFS",
            horiz_prev = 1L,
            hora_min = "12:00",
            modelo_prev = escolhido
        ),
        modelo = list(
            modelo_escolhido = escolhido,
            modelo_final = NULL,
            variaveis_usadas = character(0L)
        )
    )
}

make_models_list <- function(n = 2L, n_dummy = 0L) {
    valid <- replicate(n, make_model_entry("arimax"), simplify = FALSE)
    dummy <- replicate(n_dummy, make_model_entry("fallback"), simplify = FALSE)
    c(valid, dummy)
}

make_config <- function() {
    list(
        ids_usinas = "BAUFI1",
        modelos_previsao = c("arimax"),
        horizonte_dias = 10L
    )
}

make_new_artifact <- function(id = "BAUFI1", n = 2L, n_dummy = 0L) {
    build_model_artifact(id, make_models_list(n, n_dummy), make_config())
}

# TESTS FOR build_model_artifact -----------------------------------------------

test_that("build_model_artifact returns list with three envelope keys", {
    result <- make_new_artifact()

    expect_type(result, "list")
    expect_true(all(c("id_usina", "models", "metadata") %in% names(result)))
})

test_that("build_model_artifact preserves id_usina", {
    result <- make_new_artifact(id = "BAUFI1")

    expect_equal(result$id_usina, "BAUFI1")
})

test_that("build_model_artifact preserves models list unchanged", {
    models <- make_models_list(3L)
    result <- build_model_artifact("BAUFI1", models, make_config())

    expect_identical(result$models, models)
})

test_that("build_model_artifact sets correct metadata type", {
    result <- make_new_artifact()

    expect_equal(result$metadata$type, "pfvsh_model_ensemble")
})

test_that("build_model_artifact counts n_combinacoes correctly", {
    result <- make_new_artifact(n = 5L)

    expect_equal(result$metadata$n_combinacoes, 5L)
})

test_that("build_model_artifact counts n_modelos_validos excluding dummies", {
    result <- make_new_artifact(n = 4L, n_dummy = 2L)

    expect_equal(result$metadata$n_combinacoes, 6L)
    expect_equal(result$metadata$n_modelos_validos, 4L)
})

test_that("build_model_artifact includes timestamp in ISO 8601 format", {
    result <- make_new_artifact()

    expect_match(result$metadata$timestamp, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("build_model_artifact includes package_version as character", {
    result <- make_new_artifact()

    expect_type(result$metadata$package_version, "character")
    expect_length(result$metadata$package_version, 1L)
})

test_that("build_model_artifact includes config_hash as character", {
    result <- make_new_artifact()

    expect_type(result$metadata$config_hash, "character")
    expect_length(result$metadata$config_hash, 1L)
    expect_equal(nchar(result$metadata$config_hash), 64L)
})

test_that("build_model_artifact config_hash changes with different config", {
    models <- make_models_list(2L)
    cfg1 <- list(ids_usinas = "BAUFI1", horizonte_dias = 10L)
    cfg2 <- list(ids_usinas = "BAUFI1", horizonte_dias = 5L)

    r1 <- build_model_artifact("BAUFI1", models, cfg1)
    r2 <- build_model_artifact("BAUFI1", models, cfg2)

    expect_false(identical(r1$metadata$config_hash, r2$metadata$config_hash))
})

# TESTS FOR count_valid_models -------------------------------------------------

test_that("count_valid_models counts non-dummy models", {
    models <- make_models_list(n = 3L, n_dummy = 0L)

    expect_equal(count_valid_models(models), 3L)
})

test_that("count_valid_models excludes fallback entries", {
    models <- make_models_list(n = 2L, n_dummy = 3L)

    expect_equal(count_valid_models(models), 2L)
})

test_that("count_valid_models returns zero when all models are dummy", {
    models <- make_models_list(n = 0L, n_dummy = 4L)

    expect_equal(count_valid_models(models), 0L)
})

test_that("count_valid_models returns full count when no dummies", {
    models <- make_models_list(n = 96L, n_dummy = 0L)

    expect_equal(count_valid_models(models), 96L)
})

# TESTS FOR validate_artifact (new format) -------------------------------------

test_that("validate_artifact accepts valid new-format artifact", {
    artifact <- make_new_artifact()

    expect_invisible(validate_artifact(artifact))
    expect_true(validate_artifact(artifact))
})

test_that("validate_artifact stops when artifact is not a list", {
    expect_error(validate_artifact("not a list"), "lista")
    expect_error(validate_artifact(42L), "lista")
    expect_error(validate_artifact(NULL), "lista")
})

test_that("validate_artifact stops with collected errors for malformed new format", {
    malformed <- list(metadata = list(type = "pfvsh_model_ensemble"))

    expect_error(validate_artifact(malformed), "Campo obrigatorio ausente")
})

test_that("validate_artifact collects multiple errors before stopping", {
    malformed <- list(metadata = list(type = "pfvsh_model_ensemble"))

    err <- tryCatch(validate_artifact(malformed), error = function(e) conditionMessage(e))

    expect_match(err, "id_usina")
    expect_match(err, "models")
})

test_that("validate_artifact errors on missing models with partial envelope", {
    partial <- list(id_usina = "BAUFI1", metadata = list(type = "pfvsh_model_ensemble"))

    expect_error(validate_artifact(partial), "Campo obrigatorio ausente")
})

test_that("validate_artifact errors on missing id_usina with partial envelope", {
    partial <- list(models = make_models_list(2L), metadata = list(type = "x"))

    expect_error(validate_artifact(partial), "Campo obrigatorio ausente")
})

test_that("validate_artifact errors when id_usina is not character scalar", {
    artifact <- make_new_artifact()
    artifact$id_usina <- c("BAUFI1", "BAUFI2")

    expect_error(validate_artifact(artifact), "id_usina")
})

test_that("validate_artifact errors when models is empty list", {
    artifact <- make_new_artifact()
    artifact$models <- list()

    expect_error(validate_artifact(artifact), "models")
})

test_that("validate_artifact errors when metadata is missing", {
    artifact <- list(id_usina = "BAUFI1", models = make_models_list(2L))

    expect_error(validate_artifact(artifact), "Campo obrigatorio ausente")
    err <- tryCatch(validate_artifact(artifact), error = function(e) conditionMessage(e))
    expect_match(err, "metadata")
})

# TESTS FOR check_artifact_id_usina -------------------------------------------

test_that("check_artifact_id_usina appends error when id_usina missing", {
    artifact <- list(models = list())
    errors <- character(0L)

    result <- check_artifact_id_usina(errors, artifact)

    expect_length(result, 1L)
    expect_match(result, "id_usina")
})

test_that("check_artifact_id_usina appends error when id_usina is not scalar", {
    artifact <- list(id_usina = c("A", "B"), models = list())
    errors <- character(0L)

    result <- check_artifact_id_usina(errors, artifact)

    expect_length(result, 1L)
    expect_match(result, "id_usina")
})

test_that("check_artifact_id_usina returns errors unchanged when valid", {
    artifact <- list(id_usina = "BAUFI1", models = list())
    errors <- c("prior error")

    result <- check_artifact_id_usina(errors, artifact)

    expect_equal(result, c("prior error"))
})

# TESTS FOR check_artifact_models ---------------------------------------------

test_that("check_artifact_models appends error when models missing", {
    artifact <- list(id_usina = "BAUFI1")
    errors <- character(0L)

    result <- check_artifact_models(errors, artifact)

    expect_length(result, 1L)
    expect_match(result, "models")
})

test_that("check_artifact_models appends error when models is empty", {
    artifact <- list(id_usina = "BAUFI1", models = list())
    errors <- character(0L)

    result <- check_artifact_models(errors, artifact)

    expect_length(result, 1L)
    expect_match(result, "models")
})

test_that("check_artifact_models returns errors unchanged when valid", {
    artifact <- list(id_usina = "BAUFI1", models = make_models_list(1L))
    errors <- character(0L)

    result <- check_artifact_models(errors, artifact)

    expect_length(result, 0L)
})
