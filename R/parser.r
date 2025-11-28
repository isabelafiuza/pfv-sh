#' Constroi Parser de Argumentos CLI
#'
#' Cria um ArgumentParser configurado com os argumentos suportados
#' pelo modelo de previsao solar fotovoltaica.
#'
#' @return Objeto \code{ArgumentParser} com argumentos configurados:
#'   \describe{
#'     \item{--datadir}{Diretorio de dados para execucao (default: "./data")}
#'   }
#'
#' @details
#' O parser e usado pelo \code{main.r} para processar argumentos de linha
#' de comando. O diretorio de dados deve conter:
#' \itemize{
#'   \item config.jsonc - Arquivo de configuracao
#'   \item usinas.csv - Cadastro de usinas
#'   \item geracao_observada.csv - Dados de geracao historica
#'   \item irradiancia_prevista.csv - Previsoes NWP
#' }
#'
#' @examples
#' \dontrun{
#' parser <- get_parser()
#' args <- parser$parse_args()
#' print(args$datadir)
#' }
#'
#' @seealso \code{\link[argparse]{ArgumentParser}}
#'
#' @export
get_parser <- function() {
    parser <- ArgumentParser(description = "Modelo de Previsao de Geracao Solar Fotovoltaica Semihoraria")

    parser <- inner_parser_generic_args(parser)
    return(parser)
}

# AUXILIARES ---------------------------------------------------------------------------------------

#' Adiciona Argumentos Genericos ao Parser
#'
#' Funcao interna que configura os argumentos padrao do CLI.
#'
#' @param parser Objeto ArgumentParser
#'
#' @return Parser com argumentos adicionados
#'
#' @keywords internal
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
