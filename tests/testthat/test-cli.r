test_that("read_env_flag", {
    f <- pfvsh:::read_env_flag
    expect_true(is.function(f))

    test_that("read_env_flag retorna padrao quando variavel nao esta definida", {
        withr::local_envvar(PFVSH_TEST_FLAG = NA)
        expect_false(f("PFVSH_TEST_FLAG", FALSE))
        expect_true(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag retorna TRUE para 'true'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "true")
        expect_true(f("PFVSH_TEST_FLAG", FALSE))
    })

    test_that("read_env_flag retorna TRUE para 'TRUE' (maiusculo)", {
        withr::local_envvar(PFVSH_TEST_FLAG = "TRUE")
        expect_true(f("PFVSH_TEST_FLAG", FALSE))
    })

    test_that("read_env_flag retorna TRUE para '1'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "1")
        expect_true(f("PFVSH_TEST_FLAG", FALSE))
    })

    test_that("read_env_flag retorna TRUE para 'yes'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "yes")
        expect_true(f("PFVSH_TEST_FLAG", FALSE))
    })

    test_that("read_env_flag retorna FALSE para 'false'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "false")
        expect_false(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag retorna FALSE para 'FALSE' (maiusculo)", {
        withr::local_envvar(PFVSH_TEST_FLAG = "FALSE")
        expect_false(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag retorna FALSE para '0'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "0")
        expect_false(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag retorna FALSE para 'no'", {
        withr::local_envvar(PFVSH_TEST_FLAG = "no")
        expect_false(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag retorna padrao para valor invalido", {
        withr::local_envvar(PFVSH_TEST_FLAG = "banana")
        expect_false(f("PFVSH_TEST_FLAG", FALSE))
        expect_true(f("PFVSH_TEST_FLAG", TRUE))
    })

    test_that("read_env_flag ignora espacos em branco ao redor do valor", {
        withr::local_envvar(PFVSH_TEST_FLAG = "  true  ")
        expect_true(f("PFVSH_TEST_FLAG", FALSE))
    })
})

test_that("read_env_integer", {
    f <- pfvsh:::read_env_integer
    expect_true(is.function(f))

    test_that("read_env_integer retorna NULL quando variavel nao esta definida", {
        withr::local_envvar(PFVSH_TEST_INT = NA)
        expect_null(f("PFVSH_TEST_INT"))
    })

    test_that("read_env_integer retorna padrao customizado quando nao definida", {
        withr::local_envvar(PFVSH_TEST_INT = NA)
        expect_equal(f("PFVSH_TEST_INT", default = 4L), 4L)
    })

    test_that("read_env_integer retorna inteiro correto para valor valido", {
        withr::local_envvar(PFVSH_TEST_INT = "4")
        expect_equal(f("PFVSH_TEST_INT"), 4L)
    })

    test_that("read_env_integer retorna inteiro correto para '1'", {
        withr::local_envvar(PFVSH_TEST_INT = "1")
        expect_equal(f("PFVSH_TEST_INT"), 1L)
    })

    test_that("read_env_integer retorna NULL para valor nao numerico", {
        withr::local_envvar(PFVSH_TEST_INT = "abc")
        expect_null(f("PFVSH_TEST_INT"))
    })

    test_that("read_env_integer retorna NULL para valor negativo", {
        withr::local_envvar(PFVSH_TEST_INT = "-1")
        expect_null(f("PFVSH_TEST_INT"))
    })

    test_that("read_env_integer retorna NULL para zero", {
        withr::local_envvar(PFVSH_TEST_INT = "0")
        expect_null(f("PFVSH_TEST_INT"))
    })

    test_that("read_env_integer retorna integer (nao double)", {
        withr::local_envvar(PFVSH_TEST_INT = "3")
        result <- f("PFVSH_TEST_INT")
        expect_true(is.integer(result))
    })
})

test_that("cli_main", {
    f <- cli_main
    expect_true(is.function(f))

    test_that("cli_main usa PFVSH_PARALLEL quando parallel nao e TRUE", {
        withr::local_envvar(PFVSH_PARALLEL = "true", PFVSH_RESUME = NA)
        parallel_recebido <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                parallel_recebido <<- parallel
            },
            .package = "pfvsh"
        )
        f(datadir = "./data")
        expect_true(parallel_recebido)
    })

    test_that("cli_main usa PFVSH_RESUME quando resume nao e TRUE", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = "true")
        resume_recebido <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                resume_recebido <<- resume
            },
            .package = "pfvsh"
        )
        f(datadir = "./data")
        expect_true(resume_recebido)
    })

    test_that("cli_main com parallel=TRUE sobrepoe PFVSH_PARALLEL=false", {
        withr::local_envvar(PFVSH_PARALLEL = "false")
        parallel_recebido <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                parallel_recebido <<- parallel
            },
            .package = "pfvsh"
        )
        f(datadir = "./data", parallel = TRUE)
        expect_true(parallel_recebido)
    })

    test_that("cli_main descarta workers quando parallel e FALSE", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA)
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) invisible(NULL),
            .package = "pfvsh"
        )
        expect_no_error(f(datadir = "./data", parallel = FALSE, workers = 4L))
    })

    test_that("cli_main define PFVSH_WORKERS quando parallel=TRUE e workers e fornecido", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA, PFVSH_WORKERS = NA)
        workers_env_capturado <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                workers_env_capturado <<- Sys.getenv("PFVSH_WORKERS", unset = "")
            },
            .package = "pfvsh"
        )
        f(datadir = "./data", parallel = TRUE, workers = 4L)
        expect_equal(workers_env_capturado, "4")
    })

    test_that("cli_main nao define PFVSH_WORKERS quando workers e NULL", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA, PFVSH_WORKERS = NA)
        workers_env_capturado <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                workers_env_capturado <<- Sys.getenv("PFVSH_WORKERS", unset = "UNSET")
            },
            .package = "pfvsh"
        )
        f(datadir = "./data", parallel = TRUE, workers = NULL)
        expect_equal(workers_env_capturado, "UNSET")
    })

    test_that("cli_main passa parallel e resume para predict_main", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA)
        parallel_recebido <- NULL
        resume_recebido <- NULL
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "predict"),
            predict_main = function(config, parallel = FALSE, resume = FALSE, ...) {
                parallel_recebido <<- parallel
                resume_recebido <<- resume
            },
            .package = "pfvsh"
        )
        f(datadir = "./data", parallel = TRUE, resume = TRUE)
        expect_true(parallel_recebido)
        expect_true(resume_recebido)
    })

    test_that("cli_main retorna 0L invisivel em sucesso", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA)
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "train"),
            train_main = function(...) invisible(NULL),
            .package = "pfvsh"
        )
        result <- withVisible(f(datadir = "./data"))
        expect_equal(result$value, 0L)
        expect_false(result$visible)
    })

    test_that("cli_main levanta erro com modo invalido", {
        withr::local_envvar(PFVSH_PARALLEL = NA, PFVSH_RESUME = NA)
        local_mocked_bindings(
            conectamock_pfv = function(...) list(),
            get_config = function(...) list(),
            parse_config = function(...) list(mode = "invalid_mode"),
            .package = "pfvsh"
        )
        expect_error(f(datadir = "./data"), "Modo invalido")
    })
})
