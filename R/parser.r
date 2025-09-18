#' Constroi o interpretador de argumentos de linha de comando
#'
#' @return `ArgumentParser` com os argumentos suportados pelo modelo
#'
#' @export

get_parser <- function() {
    parser <- ArgumentParser(description = "Modelo de Consistencia do Melhor Historico de Geracao Solar")

    parser <- inner_parser_generic_args(parser)
    return(parser)
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Auxiliar para adicionar argumentos genericos
#'
#' Funcao interna, nao deve ser chamada diretamente pelo usuario

inner_parser_generic_args <- function(parser) {
    help_msg <- paste0("Diretorio de dados para execucao do melhor historico -- Veja ",
        "https://github.com/rjmalves/melhor-historico-solar para detalhes")
    parser$add_argument("--datadir",
        type = "character",
        default = "./data",
        help = help_msg
    )

    return(parser)
}