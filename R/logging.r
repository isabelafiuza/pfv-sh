get_ctx_filter_name <- function() "pfvsh_ctx"

configure_json_logging <- function(lg) {
    fmt <- Sys.getenv("PFVSH_LOG_FORMAT", unset = "")
    if (tolower(trimws(fmt)) != "json") return()

    tryCatch({
        lg$appenders$console$set_layout(LayoutJson$new())
    }, error = function(e) {
        warning(
            "Falha ao configurar log JSON: ", conditionMessage(e),
            call. = FALSE
        )
    })
}

logger_setup <- function() {
    lg <- lgr::get_logger("pfvsh")
    lg$set_threshold(Sys.getenv("LOG_LEVEL", unset = "info"))
    lg$set_propagate(FALSE)
    lg$add_appender(
        lgr::AppenderConsole$new(
            layout = LayoutFormat$new(timestamp_fmt = "%Y-%m-%d %H:%M:%S")
        ),
        name = "console"
    )
    configure_json_logging(lg)

    lg
}

#' Get Package Logger
#'
#' @return objeto `Logger` como retornado por `lgr::get_logger()`
#'
#' @export

get_pkg_logger <- function() {
    get("lg", envir = asNamespace("pfvsh"))
}

#' Define Contexto Estruturado no Logger
#'
#' Injeta campos `run_id`, `mode` e `stage` (opcional) em todos os eventos
#' de log subsequentes via `lgr::FilterInject`. Substitui filtro existente
#' de mesmo nome, se houver.
#'
#' @param run_id character escalar, identificador unico da execucao
#' @param mode character escalar, `"train"` ou `"predict"`
#' @param stage character escalar ou `NULL`
#'
#' @return `invisible(NULL)`
#'
#' @seealso [clear_log_context()]

set_log_context <- function(run_id, mode, stage = NULL) {
    lg <- lgr::get_logger("pfvsh")
    ctx_name <- get_ctx_filter_name()

    if (ctx_name %in% names(lg$filters)) {
        lg$remove_filter(ctx_name)
    }

    fields <- list(run_id = run_id, mode = mode)
    if (!is.null(stage)) fields$stage <- stage

    fi <- do.call(lgr::FilterInject$new, fields)
    lg$add_filter(fi, name = ctx_name)
}

#' Remove Contexto Estruturado do Logger
#'
#' Remove o filtro `"pfvsh_ctx"` do logger. No-op se o filtro nao existir.
#'
#' @return `invisible(NULL)`
#'
#' @seealso [set_log_context()]

clear_log_context <- function() {
    lg <- lgr::get_logger("pfvsh")
    ctx_name <- get_ctx_filter_name()

    if (ctx_name %in% names(lg$filters)) {
        lg$remove_filter(ctx_name)
    }
}
