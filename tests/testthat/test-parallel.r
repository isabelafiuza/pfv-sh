test_that("setup_parallel_plan", {
    f <- setup_parallel_plan
    expect_true(is.function(f))

    test_that("setup_parallel_plan com defaults configura plano valido", {
        withr::defer(future::plan("sequential"))
        f()
        cfg <- get_parallel_config()
        expect_true(cfg$workers >= 1L)
    })

    test_that("setup_parallel_plan com sequential configura plano sequential", {
        withr::defer(future::plan("sequential"))
        f(strategy = "sequential")
        cfg <- get_parallel_config()
        expect_equal(cfg$strategy, "sequential")
    })

    test_that("setup_parallel_plan respeita argumento workers", {
        withr::defer(future::plan("sequential"))
        f(workers = 2L)
        cfg <- get_parallel_config()
        expect_equal(cfg$workers, 2L)
    })

    test_that("setup_parallel_plan retorna plano anterior", {
        withr::defer(future::plan("sequential"))
        future::plan("sequential")
        old <- f(workers = 2L)
        expect_true(!is.null(old))
    })

    test_that("setup_parallel_plan com workers negativos levanta erro", {
        expect_error(f(workers = -1L), "inteiro positivo")
    })

    test_that("setup_parallel_plan com workers zero levanta erro", {
        expect_error(f(workers = 0L), "inteiro positivo")
    })

    test_that("setup_parallel_plan com strategy invalida levanta erro", {
        expect_error(f(strategy = "bad"), "arg")
    })

    test_that("setup_parallel_plan workers default e pelo menos 1", {
        withr::defer(future::plan("sequential"))
        f()
        cfg <- get_parallel_config()
        expect_gte(cfg$workers, 1L)
    })

    test_that("setup_parallel_plan com workers fracionarios levanta erro", {
        expect_error(f(workers = 2.5), "inteiro positivo")
    })

    test_that("setup_parallel_plan com workers NA levanta erro", {
        expect_error(f(workers = NA_integer_), "inteiro positivo")
    })

    test_that("setup_parallel_plan usa PFVSH_WORKERS quando workers e NULL", {
        withr::local_envvar(PFVSH_WORKERS = "2")
        withr::defer(future::plan("sequential"))
        f(workers = NULL, strategy = "multisession")
        cfg <- get_parallel_config()
        expect_equal(cfg$workers, 2L)
    })

    test_that("setup_parallel_plan ignora PFVSH_WORKERS quando workers e fornecido", {
        withr::local_envvar(PFVSH_WORKERS = "8")
        withr::defer(future::plan("sequential"))
        f(workers = 2L, strategy = "multisession")
        cfg <- get_parallel_config()
        expect_equal(cfg$workers, 2L)
    })

    test_that("setup_parallel_plan usa auto-detect quando PFVSH_WORKERS e invalido", {
        withr::local_envvar(PFVSH_WORKERS = "banana")
        withr::defer(future::plan("sequential"))
        expect_no_error(f(workers = NULL, strategy = "multisession"))
        cfg <- get_parallel_config()
        expect_gte(cfg$workers, 1L)
    })

    test_that("setup_parallel_plan usa auto-detect quando PFVSH_WORKERS e string vazia", {
        withr::local_envvar(PFVSH_WORKERS = "")
        withr::defer(future::plan("sequential"))
        expect_no_error(f(workers = NULL, strategy = "multisession"))
        cfg <- get_parallel_config()
        expect_gte(cfg$workers, 1L)
    })
})

test_that("reset_parallel_plan", {
    f <- reset_parallel_plan
    expect_true(is.function(f))

    test_that("reset_parallel_plan restaura plano anterior", {
        withr::defer(future::plan("sequential"))
        future::plan("sequential")
        old <- setup_parallel_plan(workers = 2L)
        expect_equal(get_parallel_config()$workers, 2L)

        f(old)
        cfg <- get_parallel_config()
        expect_equal(cfg$strategy, "sequential")
    })

    test_that("reset_parallel_plan retorna invisible NULL", {
        withr::defer(future::plan("sequential"))
        old <- setup_parallel_plan(strategy = "sequential")
        result <- f(old)
        expect_null(result)
    })
})

test_that("get_parallel_config", {
    f <- get_parallel_config
    expect_true(is.function(f))

    test_that("get_parallel_config retorna estrutura correta", {
        withr::defer(future::plan("sequential"))
        setup_parallel_plan(workers = 2L, strategy = "multisession")
        cfg <- f()

        expect_true(is.list(cfg))
        expect_named(cfg, c("workers", "strategy"))
        expect_true(is.numeric(cfg$workers))
        expect_true(is.character(cfg$strategy))
    })

    test_that("get_parallel_config reflete plano sequential", {
        withr::defer(future::plan("sequential"))
        setup_parallel_plan(strategy = "sequential")
        cfg <- f()

        expect_equal(cfg$strategy, "sequential")
        expect_equal(cfg$workers, 1L)
    })

    test_that("get_parallel_config reflete plano multisession", {
        withr::defer(future::plan("sequential"))
        setup_parallel_plan(workers = 2L, strategy = "multisession")
        cfg <- f()

        expect_equal(cfg$strategy, "multisession")
        expect_equal(cfg$workers, 2L)
    })
})

test_that("split_args_by_plant", {
    f <- pfvsh:::split_args_by_plant
    expect_true(is.function(f))

    test_that("split_args_by_plant particiona data.tables por id_usina", {
        dt <- data.table::data.table(
            id_usina = c("A", "A", "B", "B"),
            valor = c(1, 2, 3, 4)
        )
        extra <- list(dt_obs = dt, escalar = 0.5)
        result <- f(extra, c("A", "B"))

        expect_named(result, c("A", "B"))
        expect_equal(result[["A"]]$.iu, "A")
        expect_equal(result[["B"]]$.iu, "B")
        expect_equal(nrow(result[["A"]]$dt_obs), 2L)
        expect_equal(result[["A"]]$dt_obs$valor, c(1, 2))
        expect_equal(nrow(result[["B"]]$dt_obs), 2L)
        expect_equal(result[["B"]]$dt_obs$valor, c(3, 4))
    })

    test_that("split_args_by_plant preserva escalares inalterados", {
        dt <- data.table::data.table(id_usina = "A", valor = 1)
        extra <- list(dt_obs = dt, fonte = c("PI", "CCEE"), tol = 1.1)
        result <- f(extra, "A")

        expect_equal(result[["A"]]$fonte, c("PI", "CCEE"))
        expect_equal(result[["A"]]$tol, 1.1)
    })

    test_that("split_args_by_plant retorna data.table vazio para usina ausente", {
        dt <- data.table::data.table(id_usina = "A", valor = 1)
        extra <- list(dt_obs = dt)
        result <- f(extra, c("A", "B"))

        expect_equal(nrow(result[["B"]]$dt_obs), 0L)
        expect_true("id_usina" %in% names(result[["B"]]$dt_obs))
        expect_true("valor" %in% names(result[["B"]]$dt_obs))
    })

    test_that("split_args_by_plant com data.table vazio retorna vazios", {
        dt <- data.table::data.table(id_usina = character(), valor = numeric())
        extra <- list(dt_obs = dt, x = 1)
        result <- f(extra, c("A", "B"))

        expect_equal(nrow(result[["A"]]$dt_obs), 0L)
        expect_equal(nrow(result[["B"]]$dt_obs), 0L)
        expect_equal(result[["A"]]$x, 1)
    })

    test_that("split_args_by_plant ignora data.tables sem id_usina", {
        dt_with <- data.table::data.table(id_usina = c("A", "B"), valor = 1:2)
        dt_without <- data.table::data.table(param = "cfg", valor = 99)
        extra <- list(dt_obs = dt_with, config_dt = dt_without)
        result <- f(extra, c("A", "B"))

        expect_equal(nrow(result[["A"]]$dt_obs), 1L)
        expect_identical(result[["A"]]$config_dt, dt_without)
        expect_identical(result[["B"]]$config_dt, dt_without)
    })

    test_that("split_args_by_plant com multiplos data.tables", {
        dt1 <- data.table::data.table(id_usina = c("X", "Y"), a = 1:2)
        dt2 <- data.table::data.table(id_usina = c("X", "X", "Y"), b = 10:12)
        extra <- list(d1 = dt1, d2 = dt2, s = "val")
        result <- f(extra, c("X", "Y"))

        expect_equal(nrow(result[["X"]]$d1), 1L)
        expect_equal(nrow(result[["X"]]$d2), 2L)
        expect_equal(nrow(result[["Y"]]$d1), 1L)
        expect_equal(nrow(result[["Y"]]$d2), 1L)
        expect_equal(result[["X"]]$s, "val")
    })
})

test_that("validate_workers", {
    f <- pfvsh:::validate_workers
    expect_true(is.function(f))

    test_that("validate_workers aceita NULL", {
        expect_silent(f(NULL))
    })

    test_that("validate_workers aceita inteiro positivo", {
        expect_silent(f(1L))
        expect_silent(f(4L))
    })

    test_that("validate_workers rejeita valores invalidos", {
        expect_error(f(-1L), "inteiro positivo")
        expect_error(f(0L), "inteiro positivo")
        expect_error(f(2.5), "inteiro positivo")
        expect_error(f(NA_integer_), "inteiro positivo")
        expect_error(f("2"), "inteiro positivo")
    })
})

test_that("is_valid_worker_count", {
    f <- pfvsh:::is_valid_worker_count
    expect_true(is.function(f))

    test_that("is_valid_worker_count returns TRUE for positive integer", {
        expect_true(f(1L))
        expect_true(f(4L))
        expect_true(f(1.0))
    })

    test_that("is_valid_worker_count returns FALSE for zero", {
        expect_false(f(0L))
    })

    test_that("is_valid_worker_count returns FALSE for negative", {
        expect_false(f(-1L))
    })

    test_that("is_valid_worker_count returns FALSE for fraction", {
        expect_false(f(2.5))
    })

    test_that("is_valid_worker_count returns FALSE for NA", {
        expect_false(f(NA_integer_))
    })

    test_that("is_valid_worker_count returns FALSE for non-numeric", {
        expect_false(f("2"))
        expect_false(f(NULL))
    })
})

test_that("read_env_workers_fallback", {
    f <- pfvsh:::read_env_workers_fallback
    expect_true(is.function(f))

    test_that("read_env_workers_fallback returns NULL when PFVSH_WORKERS not set", {
        withr::local_envvar(PFVSH_WORKERS = NA)
        expect_null(f())
    })

    test_that("read_env_workers_fallback returns NULL for empty string", {
        withr::local_envvar(PFVSH_WORKERS = "")
        expect_null(f())
    })

    test_that("read_env_workers_fallback returns integer for valid value", {
        withr::local_envvar(PFVSH_WORKERS = "4")
        expect_equal(f(), 4L)
        expect_true(is.integer(f()))
    })

    test_that("read_env_workers_fallback returns NULL for non-numeric value", {
        withr::local_envvar(PFVSH_WORKERS = "banana")
        expect_null(f())
    })

    test_that("read_env_workers_fallback returns NULL for zero", {
        withr::local_envvar(PFVSH_WORKERS = "0")
        expect_null(f())
    })

    test_that("read_env_workers_fallback returns NULL for negative value", {
        withr::local_envvar(PFVSH_WORKERS = "-1")
        expect_null(f())
    })
})

test_that("extract_strategy_name", {
    f <- pfvsh:::extract_strategy_name
    expect_true(is.function(f))

    test_that("extract_strategy_name returns sequential for sequential plan", {
        withr::defer(future::plan("sequential"))
        future::plan("sequential")
        plan_obj <- future::plan()
        expect_equal(f(plan_obj), "sequential")
    })

    test_that("extract_strategy_name returns multisession for multisession plan", {
        withr::defer(future::plan("sequential"))
        future::plan("multisession", workers = 2L)
        plan_obj <- future::plan()
        expect_equal(f(plan_obj), "multisession")
    })
})
