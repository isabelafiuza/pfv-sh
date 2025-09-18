suppressPackageStartupMessages(library(pfvIO))

source("R/parser.r")
source("R/config-file.r")

parser <- get_parser()
args <- parser$parse_args()

conn <- conectamock_pfv(args$datadir)
config <- get_config(conn)
config <- parse_config(config, conn)