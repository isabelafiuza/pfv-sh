#' Constroi o interpretador de argumentos de linha de comando
#'
#' @return `ArgumentParser` com os argumentos suportados pelo modelo
#'
#' @export

get_parser <- function() {
    parser <- ArgumentParser(description = "Modelo de Previsao de Geracao Solar Fotovoltaica Semihoraria")

    parser <- inner_parser_generic_args(parser)
    return(parser)
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Auxiliar para adicionar argumentos genericos
#'
#' Funcao interna, nao deve ser chamada diretamente pelo usuario
#'
#' @param parser objeto ArgumentParser para adicionar argumentos
#'
#' @return parser com argumentos adicionados
inner_parser_generic_args <- function(parser) {
    help_msg <- paste0("Diretorio de dados para execucao do modelo de previsao -- Veja ",
        "https://github.com/isabelafiuza/pfv-sh para detalhes")
    parser$add_argument("--datadir",
        type = "character",
        default = "./data",
        help = help_msg
    )

    return(parser)
}
