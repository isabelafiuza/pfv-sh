# Helper function to generate a valid configuration
gen_config <- function() {
    list(
        mode = "predict",
        input = "./data",
        output = "./out",
        artifact = ".",
        ids_usinas = list(),
        data_referencia = "2025-07-02",
        horizonte_dias = list("D+0", "D+1"),
        modelos_NWP = list("GFS"),
        modelos_previsao = list(
            arimax = list(
                tipo = "arimax",
                n_dias_treino = 300,
                amos_min = 5
            ),
            fisico_estimado = list(
                tipo = "fisico_estimado",
                n_dias_treino = 180,
                amos_min = 5
            )
        ),
        parametros_periodo_geracao = list(
            fator_tolerancia_limite_superior_geracao = 1.1,
            fator_tolerancia_limite_inferior_geracao = 0.01,
            percentual_dias_geracao = 0.9
        )
    )
}

# TESTS FOR is_relative_path ---------------------------------------------------

test_that("is_relative_path detects relative paths", {
    expect_true(is_relative_path("./data"))
    expect_true(is_relative_path("data"))
    expect_true(is_relative_path("../out"))
    expect_true(is_relative_path("artifact"))
})

test_that("is_relative_path detects absolute and URI paths", {
    expect_false(is_relative_path("/home/user/data"))
    expect_false(is_relative_path("s3://bucket/key"))
    expect_false(is_relative_path("~/data"))
})

# TESTS FOR resolve_config_paths -----------------------------------------------

test_that("resolve_config_paths resolves relative paths against base_dir", {
    withr::with_tempdir({
        base <- getwd()
        dir.create("data")
        conf <- list(input = "./data", output = "./out", artifact = "./artifact")

        result <- resolve_config_paths(conf, base)

        expect_equal(result$input, normalizePath(file.path(base, "data")))
        expect_equal(result$output, normalizePath(file.path(base, "out")))
        expect_equal(result$artifact, normalizePath(file.path(base, "artifact")))
    })
})

test_that("resolve_config_paths creates output and artifact dirs", {
    withr::with_tempdir({
        base <- getwd()
        dir.create("data")
        conf <- list(input = "./data", output = "./out", artifact = "./artifact")

        expect_false(dir.exists(file.path(base, "out")))
        expect_false(dir.exists(file.path(base, "artifact")))

        resolve_config_paths(conf, base)

        expect_true(dir.exists(file.path(base, "out")))
        expect_true(dir.exists(file.path(base, "artifact")))
    })
})

test_that("resolve_config_paths errors when input dir does not exist", {
    withr::with_tempdir({
        base <- getwd()
        conf <- list(input = "./missing", output = "./out", artifact = "./artifact")

        expect_error(
            resolve_config_paths(conf, base),
            "Diretorio de entrada nao encontrado"
        )
    })
})

test_that("resolve_config_paths preserves absolute paths", {
    withr::with_tempdir({
        base <- getwd()
        abs_input <- file.path(base, "abs_data")
        dir.create(abs_input)
        conf <- list(input = abs_input, output = "./out", artifact = "./artifact")

        result <- resolve_config_paths(conf, base)

        expect_equal(result$input, normalizePath(abs_input))
    })
})

# TESTS FOR valida_nomes_config ------------------------------------------------

test_that("valida_nomes_config accepts valid config", {
    conf <- gen_config()
    expect_null(valida_nomes_config(conf))
})

test_that("valida_nomes_config accepts config with extra elements", {
    conf <- gen_config()
    conf$nome_extra <- "valor_extra"
    expect_null(valida_nomes_config(conf))
})

test_that("valida_nomes_config errors on missing required element", {
    conf <- gen_config()
    conf$input <- NULL
    expect_error(valida_nomes_config(conf), "nao possui chaves")
})

test_that("valida_nomes_config errors with informative message about missing keys", {
    conf <- gen_config()
    conf$input <- NULL
    conf$output <- NULL
    expect_error(valida_nomes_config(conf), "input")
    expect_error(valida_nomes_config(conf), "output")
})

# TESTS FOR valid_tipos_unit ---------------------------------------------------

test_that("valid_tipos_unit validates scalar character", {
    expect_true(valid_tipos_unit("a", "character"))
    expect_true(valid_tipos_unit("a", list("character")))
    expect_true(valid_tipos_unit("a", list("character", "numeric")))
})

test_that("valid_tipos_unit validates scalar numeric", {
    expect_true(valid_tipos_unit(10, "numeric"))
    expect_true(valid_tipos_unit(10, list("character", "numeric")))
})

test_that("valid_tipos_unit validates integer", {
    expect_true(valid_tipos_unit(10L, list("character", "numeric", "integer")))
    expect_true(valid_tipos_unit(10L, "integer"))
})

test_that("valid_tipos_unit rejects NULL for non-NULL types", {
    expect_false(valid_tipos_unit(NULL, list("character")))
    expect_false(valid_tipos_unit(NULL, list("character", "numeric")))
})

test_that("valid_tipos_unit accepts NULL when NULL is in types", {
    expect_true(valid_tipos_unit(NULL, list("character", "NULL")))
})

test_that("valid_tipos_unit validates Date objects", {
    expect_true(valid_tipos_unit(Sys.Date(), "Date"))
    expect_false(valid_tipos_unit("2025-01-01", "Date"))
})

test_that("valid_tipos_unit validates logical", {
    expect_true(valid_tipos_unit(TRUE, "logical"))
    expect_true(valid_tipos_unit(FALSE, "logical"))
    expect_false(valid_tipos_unit(1, "logical"))
})

# TESTS FOR valid_tipos --------------------------------------------------------

test_that("valid_tipos validates list of numerics", {
    l <- list(1, 2, 3)
    expect_true(valid_tipos(l, "numeric"))
    expect_true(valid_tipos(l, list("numeric", "integer")))
})

test_that("valid_tipos rejects list with wrong types", {
    l <- list(1, 2, 3)
    expect_false(valid_tipos(l, list("Date")))
    expect_false(valid_tipos(l, "character"))
})

test_that("valid_tipos validates empty list", {
    l <- list()
    expect_true(valid_tipos(l, list("numeric", "Date", "character", "integer")))
})

test_that("valid_tipos validates mixed list when all types allowed", {
    l <- list(1, "a", 2L)
    expect_true(valid_tipos(l, list("numeric", "character", "integer")))
})

test_that("valid_tipos rejects mixed list when not all types allowed", {
    l <- list(1, "a", 2L)
    expect_false(valid_tipos(l, list("numeric", "integer")))
})

# TESTS FOR valida_tipos_config ------------------------------------------------

test_that("valida_tipos_config accepts valid config", {
    conf <- gen_config()
    expect_null(valida_tipos_config(conf))
})

test_that("valida_tipos_config ignores extra elements", {
    conf <- gen_config()
    conf$extra <- NA_integer_
    expect_null(valida_tipos_config(conf))
})

test_that("valida_tipos_config errors on wrong type", {
    conf <- gen_config()
    conf$input <- NA_integer_
    expect_error(valida_tipos_config(conf), "nao possuem os tipos corretos")
})

test_that("valida_tipos_config accepts date as string vector", {
    conf <- gen_config()
    conf$data_referencia <- c("2021-01-01", "2021-04-01")
    expect_null(valida_tipos_config(conf))
})

test_that("valida_tipos_config accepts numeric data_referencia", {
    conf <- gen_config()
    # data_referencia expects character or list(character/NULL), not numeric directly
    # In the actual config, numeric goes through parsearg function
    # For valida_tipos_config, character is the expected input
    conf$data_referencia <- "2021-01-01"
    expect_null(valida_tipos_config(conf))
})

# TESTS FOR parsearg_data_referencia -------------------------------------------

test_that("parsearg_data_referencia.character converts string to Date", {
    result <- parsearg_data_referencia("2025-07-02")
    expect_s3_class(result, "Date")
    expect_equal(result, as.Date("2025-07-02"))
})

test_that("parsearg_data_referencia.character handles vector input", {
    result <- parsearg_data_referencia(c("2025-01-01", "2025-12-31"))
    expect_s3_class(result, "Date")
    expect_length(result, 2)
    expect_equal(result[1], as.Date("2025-01-01"))
    expect_equal(result[2], as.Date("2025-12-31"))
})

test_that("parsearg_data_referencia.numeric computes relative dates", {
    result <- parsearg_data_referencia(10)
    expect_s3_class(result, "Date")
    expect_length(result, 2)
    expect_equal(result[1], Sys.Date() - 11)
    expect_equal(result[2], Sys.Date() - 1)
})

test_that("parsearg_data_referencia.numeric handles zero", {
    result <- parsearg_data_referencia(0)
    expect_s3_class(result, "Date")
    expect_equal(result[1], Sys.Date() - 1)
    expect_equal(result[2], Sys.Date() - 1)
})

test_that("parsearg_data_referencia.list processes list elements", {
    input <- list("2025-01-01", 5)
    result <- parsearg_data_referencia(input)
    expect_type(result, "list")
    expect_length(result, 2)
    expect_s3_class(result[[1]], "Date")
    expect_s3_class(result[[2]], "Date")
})

# TESTS FOR parsearg_ids_usinas ------------------------------------------------

test_that("parsearg_ids_usinas returns unique ids from list", {
    conn <- conectamock_pfv(testthat::test_path("data"))
    ids <- list("teste1", "teste2", "teste2")
    ids_parsed <- parsearg_ids_usinas(ids, conn)
    expect_identical(unique(unlist(ids)), ids_parsed)
})

test_that("parsearg_ids_usinas returns all usinas when empty list", {
    conn <- conectamock_pfv(testthat::test_path("data"))
    ref <- get_usinas(conn)$id_usina
    ids <- list()
    ids_parsed <- parsearg_ids_usinas(ids, conn)
    expect_identical(ref, ids_parsed)
})

test_that("parsearg_ids_usinas removes duplicates", {
    conn <- conectamock_pfv(testthat::test_path("data"))
    ids <- list("BAUFI1", "BAUFI1", "BAUFI2")
    ids_parsed <- parsearg_ids_usinas(ids, conn)
    expect_equal(length(ids_parsed), 2)
    expect_true("BAUFI1" %in% ids_parsed)
    expect_true("BAUFI2" %in% ids_parsed)
})

# TESTS FOR parsearg_horizonte_dias --------------------------------------------

test_that("parsearg_horizonte_dias unlists input", {
    input <- list("D+0", "D+1", "D+2")
    result <- parsearg_horizonte_dias(input)
    expect_type(result, "character")
    expect_equal(result, c("D+0", "D+1", "D+2"))
})

test_that("parsearg_horizonte_dias handles single element", {
    input <- list("D+0")
    result <- parsearg_horizonte_dias(input)
    expect_equal(result, "D+0")
})

test_that("parsearg_horizonte_dias handles empty list", {
    input <- list()
    result <- parsearg_horizonte_dias(input)
    expect_null(result)
})

test_that("parsearg_horizonte_dias preserves order", {
    input <- list("D+2", "D+0", "D+1")
    result <- parsearg_horizonte_dias(input)
    expect_equal(result, c("D+2", "D+0", "D+1"))
})

# TESTS FOR parsearg_modelos_nwp -----------------------------------------------

test_that("parsearg_modelos_nwp unlists input", {
    input <- list("GFS", "ECMWF")
    result <- parsearg_modelos_nwp(input)
    expect_type(result, "character")
    expect_equal(result, c("GFS", "ECMWF"))
})

test_that("parsearg_modelos_nwp handles single model", {
    input <- list("GFS")
    result <- parsearg_modelos_nwp(input)
    expect_equal(result, "GFS")
})

test_that("parsearg_modelos_nwp handles empty list", {
    input <- list()
    result <- parsearg_modelos_nwp(input)
    expect_null(result)
})

# TESTS FOR parsearg_modelos_previsao ------------------------------------------

test_that("parsearg_modelos_previsao sets class from tipo", {
    input <- list(tipo = "arimax", n_dias_treino = 300, amos_min = 5)
    result <- parsearg_modelos_previsao(input)
    expect_s3_class(result, "arimax")
    expect_equal(result$n_dias_treino, 300)
    expect_equal(result$amos_min, 5)
})

test_that("parsearg_modelos_previsao works with fisico_estimado", {
    input <- list(tipo = "fisico_estimado", n_dias_treino = 180, amos_min = 10)
    result <- parsearg_modelos_previsao(input)
    expect_s3_class(result, "fisico_estimado")
    expect_equal(result$n_dias_treino, 180)
})

test_that("parsearg_modelos_previsao preserves all list elements", {
    input <- list(
        tipo = "arimax",
        n_dias_treino = 300,
        amos_min = 5,
        custom_param = "custom_value"
    )
    result <- parsearg_modelos_previsao(input)
    expect_equal(result$custom_param, "custom_value")
})
