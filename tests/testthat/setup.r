Sys.setenv(LOG_LEVEL = "warn")
lgr::get_logger("pfvsh")$set_threshold("warn")
lgr::get_logger("pfvsh")$set_propagate(FALSE)

skip_if_no_zstd <- function() {
    available <- tryCatch(
        arrow::codec_is_available("zstd"),
        error = function(e) FALSE
    )
    testthat::skip_if_not(available, "arrow not compiled with zstd support")
}
