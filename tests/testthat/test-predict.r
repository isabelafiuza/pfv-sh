make_prov_predict <- function(ids = c("USI1", "USI2")) {
    cfg <- gen_config(mode = "predict", ids_usinas = ids)
    create_provenance(cfg, "predict", FALSE)
}

make_metrics_predict <- function(prov) {
    create_metrics(prov$run_id, "predict")
}

make_lg_predict <- function() {
    lgr::get_logger("pfvsh")
}

make_plant_result <- function(iu) {
    list(
        id_usina = iu,
        prev = list(list(combinacao_ajuste = list(), prev = 10.0))
    )
}

# TESTS FOR load_predict_resume_state ------------------------------------------

test_that("load_predict_resume_state", {
    f <- load_predict_resume_state
    expect_true(is.function(f))

    test_that("load_predict_resume_state returns empty completed when no checkpoint", {
        tmp <- withr::local_tempdir()
        cfg <- gen_config(mode = "predict", ids_usinas = c("USI1", "USI2"), output = tmp)
        prov <- create_provenance(cfg, "predict", FALSE)

        result <- f(cfg, prov)

        expect_type(result, "list")
        expect_named(result, c("provenance", "completed"))
        expect_equal(result$completed, character(0L))
    })

    test_that("load_predict_resume_state returns only plants with saved result", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2", "USI3")
        cfg <- gen_config(mode = "predict", ids_usinas = ids, output = tmp)
        prov <- create_provenance(cfg, "predict", FALSE)

        update_plant_status(prov, "USI1", "completed")
        update_plant_status(prov, "USI2", "completed")
        write_checkpoint(prov, tmp)

        write_plant_result(make_plant_result("USI1"), "USI1", tmp)

        prov2 <- create_provenance(cfg, "predict", FALSE)
        result <- f(cfg, prov2)

        expect_equal(result$completed, "USI1")
    })

    test_that("load_predict_resume_state excludes checkpoint-completed plants without result file", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2")
        cfg <- gen_config(mode = "predict", ids_usinas = ids, output = tmp)
        prov <- create_provenance(cfg, "predict", FALSE)

        update_plant_status(prov, "USI1", "completed")
        update_plant_status(prov, "USI2", "completed")
        write_checkpoint(prov, tmp)

        prov2 <- create_provenance(cfg, "predict", FALSE)
        result <- f(cfg, prov2)

        expect_equal(result$completed, character(0L))
    })

    test_that("load_predict_resume_state updates provenance status for completed plants", {
        tmp <- withr::local_tempdir()
        ids <- c("USI1", "USI2")
        cfg <- gen_config(mode = "predict", ids_usinas = ids, output = tmp)
        prov <- create_provenance(cfg, "predict", FALSE)

        update_plant_status(prov, "USI1", "completed")
        write_checkpoint(prov, tmp)
        write_plant_result(make_plant_result("USI1"), "USI1", tmp)

        prov2 <- create_provenance(cfg, "predict", FALSE)
        result <- f(cfg, prov2)

        expect_equal(result$provenance$plant_status[["USI1"]], "completed")
        expect_equal(result$provenance$plant_status[["USI2"]], "pending")
    })
})

# TESTS FOR tally_predict_results ----------------------------------------------

test_that("tally_predict_results", {
    f <- tally_predict_results
    expect_true(is.function(f))

    test_that("tally_predict_results returns zero when all plants succeed", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict(c("USI1", "USI2"))
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(make_plant_result("USI1"), make_plant_result("USI2"))
        result <- f(resultados, c("USI1", "USI2"), prov, metrics, lg, FALSE, tmp)

        expect_equal(result, 0L)
    })

    test_that("tally_predict_results counts plant_error as failure", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict(c("USI1", "USI2"))
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(
            plant_error("USI1", simpleError("prediction failed")),
            make_plant_result("USI2")
        )
        result <- f(resultados, c("USI1", "USI2"), prov, metrics, lg, FALSE, tmp)

        expect_equal(result, 1L)
    })

    test_that("tally_predict_results sets provenance status to failed for plant_error", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict(c("USI1", "USI2"))
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(
            plant_error("USI1", simpleError("prediction failed")),
            make_plant_result("USI2")
        )
        f(resultados, c("USI1", "USI2"), prov, metrics, lg, FALSE, tmp)

        expect_equal(prov$plant_status[["USI1"]], "failed")
    })

    test_that("tally_predict_results sets provenance status to completed for successes", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict(c("USI1", "USI2"))
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(make_plant_result("USI1"), make_plant_result("USI2"))
        f(resultados, c("USI1", "USI2"), prov, metrics, lg, FALSE, tmp)

        expect_equal(prov$plant_status[["USI1"]], "completed")
        expect_equal(prov$plant_status[["USI2"]], "completed")
    })

    test_that("tally_predict_results writes plant result and checkpoint when resume is TRUE", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict("USI1")
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(make_plant_result("USI1"))
        f(resultados, "USI1", prov, metrics, lg, TRUE, tmp)

        result_file <- file.path(tmp, "plant-result-USI1.rds")
        checkpoint_files <- list.files(tmp, pattern = "^checkpoint-.*\\.json$")
        expect_true(file.exists(result_file))
        expect_length(checkpoint_files, 1L)
    })

    test_that("tally_predict_results counts all failures when all plants fail", {
        tmp <- withr::local_tempdir()
        prov <- make_prov_predict(c("USI1", "USI2", "USI3"))
        metrics <- make_metrics_predict(prov)
        lg <- make_lg_predict()

        resultados <- list(
            plant_error("USI1", simpleError("e1")),
            plant_error("USI2", simpleError("e2")),
            plant_error("USI3", simpleError("e3"))
        )
        result <- f(resultados, c("USI1", "USI2", "USI3"), prov, metrics, lg, FALSE, tmp)

        expect_equal(result, 3L)
    })
})

# TESTS FOR collect_all_results ------------------------------------------------

test_that("collect_all_results", {
    f <- collect_all_results
    expect_true(is.function(f))

    test_that("collect_all_results returns new result for pending plants", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI1", "USI2")
        new_result <- make_plant_result("USI1")
        new_results <- list(new_result)

        result <- f(all_ids, "USI1", new_results, tmp)

        expect_identical(result[[1]], new_result)
    })

    test_that("collect_all_results reads saved result for previously completed plants", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI1", "USI2")
        saved_result <- make_plant_result("USI2")
        write_plant_result(saved_result, "USI2", tmp)

        new_result <- make_plant_result("USI1")
        new_results <- list(new_result)

        result <- f(all_ids, "USI1", new_results, tmp)

        expect_equal(result[[2]]$id_usina, "USI2")
    })

    test_that("collect_all_results converts plant_error to NULL", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI1", "USI2")
        err <- plant_error("USI1", simpleError("failed"))
        new_results <- list(err)

        result <- f(all_ids, "USI1", new_results, tmp)

        expect_null(result[[1]])
    })

    test_that("collect_all_results returns NULL for missing saved results", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI1", "USI2")
        new_results <- list(make_plant_result("USI1"))

        result <- f(all_ids, "USI1", new_results, tmp)

        expect_null(result[[2]])
    })

    test_that("collect_all_results handles mix of resumed and new results", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI1", "USI2", "USI3")
        saved_result <- make_plant_result("USI1")
        write_plant_result(saved_result, "USI1", tmp)

        new_results <- list(
            make_plant_result("USI2"),
            make_plant_result("USI3")
        )

        result <- f(all_ids, c("USI2", "USI3"), new_results, tmp)

        expect_equal(result[[1]]$id_usina, "USI1")
        expect_equal(result[[2]]$id_usina, "USI2")
        expect_equal(result[[3]]$id_usina, "USI3")
    })

    test_that("collect_all_results preserves all_ids order", {
        tmp <- withr::local_tempdir()
        all_ids <- c("USI3", "USI1", "USI2")
        new_results <- list(
            make_plant_result("USI3"),
            make_plant_result("USI1"),
            make_plant_result("USI2")
        )

        result <- f(all_ids, all_ids, new_results, tmp)

        expect_equal(result[[1]]$id_usina, "USI3")
        expect_equal(result[[2]]$id_usina, "USI1")
        expect_equal(result[[3]]$id_usina, "USI2")
    })
})
