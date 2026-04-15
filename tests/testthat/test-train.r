mock_worker <- function(iu, ...) list(iu = iu, result = "ok")
fail_worker <- function(iu, ...) stop("boom")

make_prov <- function(ids = c("USI1", "USI2")) {
    cfg <- gen_config(ids_usinas = ids)
    create_provenance(cfg, "train", FALSE)
}

make_metrics <- function(prov) {
    create_metrics(prov$run_id, "train")
}

make_lg <- function() {
    lgr::get_logger("pfvsh")
}

# TESTS FOR load_train_resume_state --------------------------------------------

test_that("load_train_resume_state", {
    f <- load_train_resume_state
    expect_true(is.function(f))

    test_that("load_train_resume_state returns empty completed when no checkpoint", {
        tmp <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2"), artifact = tmp)
        prov <- make_prov(c("USI1", "USI2"))

        result <- f(cfg, prov)

        expect_type(result, "list")
        expect_named(result, c("provenance", "completed"))
        expect_equal(result$completed, character(0L))
    })

    test_that("load_train_resume_state returns completed plants from checkpoint", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2", "USI3")
        cfg <- gen_config(ids_usinas = ids, artifact = tmp)
        prov <- create_provenance(cfg, "train", FALSE)

        update_plant_status(prov, "USI1", "completed")
        update_plant_status(prov, "USI2", "completed")
        write_checkpoint(prov, tmp)

        prov2 <- create_provenance(cfg, "train", FALSE)
        result <- f(cfg, prov2)

        expect_equal(sort(result$completed), c("USI1", "USI2"))
    })

    test_that("load_train_resume_state updates provenance status for completed plants", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2")
        cfg <- gen_config(ids_usinas = ids, artifact = tmp)
        prov <- create_provenance(cfg, "train", FALSE)

        update_plant_status(prov, "USI1", "completed")
        write_checkpoint(prov, tmp)

        prov2 <- create_provenance(cfg, "train", FALSE)
        result <- f(cfg, prov2)

        expect_equal(result$provenance$plant_status[["USI1"]], "completed")
        expect_equal(result$provenance$plant_status[["USI2"]], "pending")
    })

    test_that("load_train_resume_state returns empty completed when all plants pending", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2")
        cfg <- gen_config(ids_usinas = ids, artifact = tmp)
        prov <- create_provenance(cfg, "train", FALSE)

        write_checkpoint(prov, tmp)

        prov2 <- create_provenance(cfg, "train", FALSE)
        result <- f(cfg, prov2)

        expect_equal(result$completed, character(0L))
    })
})

# TESTS FOR run_plants ---------------------------------------------------------

test_that("run_plants", {
    f <- run_plants
    expect_true(is.function(f))

    test_that("run_plants returns list of same length as v_usinas", {
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        result <- f(c("USI1", "USI2"), mock_worker, list(), FALSE, metrics, lg)

        expect_type(result, "list")
        expect_length(result, 2L)
    })

    test_that("run_plants returns worker results in order", {
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        result <- f(c("USI1", "USI2"), mock_worker, list(), FALSE, metrics, lg)

        expect_equal(result[[1]]$iu, "USI1")
        expect_equal(result[[2]]$iu, "USI2")
    })

    test_that("run_plants wraps worker errors in plant_error objects", {
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        result <- f(c("USI1", "USI2"), fail_worker, list(), FALSE, metrics, lg)

        expect_true(is_plant_error(result[[1]]))
        expect_true(is_plant_error(result[[2]]))
    })

    test_that("run_plants isolates per-plant errors without propagating", {
        mixed_worker <- function(iu, ...) {
            if (iu == "USI2") stop("only USI2 fails")
            list(iu = iu, result = "ok")
        }
        prov <- make_prov(c("USI1", "USI2", "USI3"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        result <- f(c("USI1", "USI2", "USI3"), mixed_worker, list(), FALSE, metrics, lg)

        expect_false(is_plant_error(result[[1]]))
        expect_true(is_plant_error(result[[2]]))
        expect_false(is_plant_error(result[[3]]))
        expect_equal(result[[2]]$id_usina, "USI2")
        expect_match(result[[2]]$error, "only USI2 fails")
    })

    test_that("run_plants passes extra_args to worker", {
        capturing_worker <- function(iu, extra_val, ...) list(iu = iu, received = extra_val)
        prov <- make_prov("USI1")
        metrics <- make_metrics(prov)
        lg <- make_lg()

        result <- f("USI1", capturing_worker, list(extra_val = 42L), FALSE, metrics, lg)

        expect_equal(result[[1]]$received, 42L)
    })

    test_that("run_plants records timing for each plant", {
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        f(c("USI1", "USI2"), mock_worker, list(), FALSE, metrics, lg)

        expect_false(is.null(metrics$plants[["USI1"]]$duration_seconds))
        expect_false(is.null(metrics$plants[["USI2"]]$duration_seconds))
    })
})

# TESTS FOR tally_train_results ------------------------------------------------

test_that("tally_train_results", {
    f <- tally_train_results
    expect_true(is.function(f))

    test_that("tally_train_results returns zero when all plants succeed", {
        tmp <- withr::local_tempdir()
        artifact_dir <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2"), output = tmp, artifact = artifact_dir)
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        raw_models <- replicate(2L, gen_model_entry(), simplify = FALSE)
        models <- list(raw_models, raw_models)

        mockery::stub(f, "pfvIO:::write_model_artifact", function(...) invisible(NULL))

        result <- f(models, c("USI1", "USI2"), prov, metrics, lg, FALSE, cfg)

        expect_equal(result, 0L)
    })

    test_that("tally_train_results counts plant_error as failure", {
        tmp <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2"), output = tmp)
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        raw_models <- replicate(2L, gen_model_entry(), simplify = FALSE)
        err_usi1 <- plant_error("USI1", simpleError("model failed"))
        models <- list(err_usi1, raw_models)

        mockery::stub(f, "pfvIO:::write_model_artifact", function(...) invisible(NULL))

        result <- f(models, c("USI1", "USI2"), prov, metrics, lg, FALSE, cfg)

        expect_equal(result, 1L)
    })

    test_that("tally_train_results sets provenance status to failed for plant_error", {
        tmp <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2"), output = tmp)
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        raw_models <- replicate(2L, gen_model_entry(), simplify = FALSE)
        err_usi1 <- plant_error("USI1", simpleError("model failed"))
        models <- list(err_usi1, raw_models)

        mockery::stub(f, "pfvIO:::write_model_artifact", function(...) invisible(NULL))
        f(models, c("USI1", "USI2"), prov, metrics, lg, FALSE, cfg)

        expect_equal(prov$plant_status[["USI1"]], "failed")
    })

    test_that("tally_train_results sets provenance status to completed for successes", {
        tmp <- withr::local_tempdir()
        artifact_dir <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2"), output = tmp, artifact = artifact_dir)
        prov <- make_prov(c("USI1", "USI2"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        raw_models <- replicate(2L, gen_model_entry(), simplify = FALSE)
        models <- list(raw_models, raw_models)

        mockery::stub(f, "pfvIO:::write_model_artifact", function(...) invisible(NULL))
        f(models, c("USI1", "USI2"), prov, metrics, lg, FALSE, cfg)

        expect_equal(prov$plant_status[["USI1"]], "completed")
        expect_equal(prov$plant_status[["USI2"]], "completed")
    })

    test_that("tally_train_results counts all failures when all plants fail", {
        tmp <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = c("USI1", "USI2", "USI3"), output = tmp)
        prov <- make_prov(c("USI1", "USI2", "USI3"))
        metrics <- make_metrics(prov)
        lg <- make_lg()

        models <- list(
            plant_error("USI1", simpleError("e1")),
            plant_error("USI2", simpleError("e2")),
            plant_error("USI3", simpleError("e3"))
        )

        result <- f(models, c("USI1", "USI2", "USI3"), prov, metrics, lg, FALSE, cfg)

        expect_equal(result, 3L)
    })

    test_that("tally_train_results writes checkpoint when resume is TRUE", {
        tmp <- withr::local_tempdir()
        artifact_dir <- withr::local_tempdir()
        cfg <- gen_config(ids_usinas = "USI1", output = tmp, artifact = artifact_dir)
        prov <- create_provenance(cfg, "train", FALSE)
        metrics <- create_metrics(prov$run_id, "train")
        lg <- make_lg()

        raw_models <- replicate(2L, gen_model_entry(), simplify = FALSE)
        models <- list(raw_models)

        mockery::stub(f, "pfvIO:::write_model_artifact", function(...) invisible(NULL))
        f(models, "USI1", prov, metrics, lg, TRUE, cfg)

        checkpoint_files <- list.files(artifact_dir, pattern = "^checkpoint-.*\\.json$")
        expect_length(checkpoint_files, 1L)
    })
})
