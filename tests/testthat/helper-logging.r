capture_lgr_warns <- function(expr) {
    lg <- lgr::get_logger("pfvsh")
    prev_threshold <- lg$threshold
    lg$set_threshold("warn")
    buf <- lgr::AppenderBuffer$new()
    lg$add_appender(buf, name = "test_capture")
    on.exit({
        lg$remove_appender("test_capture")
        lg$set_threshold(prev_threshold)
    }, add = TRUE)
    force(expr)
    buf$buffer_dt$msg
}
