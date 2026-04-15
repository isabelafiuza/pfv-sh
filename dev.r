devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(pfvIO))

parser <- get_parser()
args <- parser$parse_args()

tryCatch(
    {
        cli_main(
            datadir = args$datadir,
            parallel = args$parallel,
            resume = args$resume,
            workers = args$workers
        )
        q(status = 0)
    },
    error = function(e) {
        lg <- get_pkg_logger()
        lg$error(e)
        q(status = 1)
    }
)
