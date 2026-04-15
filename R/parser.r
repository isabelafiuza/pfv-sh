#' Constroi Parser de Argumentos CLI
#'
#' Cria um `ArgumentParser` configurado com os argumentos suportados
#' pelo modelo de previsao solar fotovoltaica.
#'
#' @return `ArgumentParser` com argumentos configurados:
#'   \describe{
#'     \item{`--datadir`}{Diretorio de dados para execucao (default: `"./data"`)}
#'     \item{`--parallel`}{Habilita processamento paralelo de usinas (flag booleana)}
#'     \item{`--resume`}{Habilita retomada a partir do ultimo checkpoint (flag booleana)}
#'     \item{`--workers`}{Numero de workers paralelos (inteiro, default: auto-detect)}
#'   }
#'
#' @details
#' O parser e usado pelo `cli_main()` para processar argumentos de linha de
#' comando. O diretorio de dados deve conter:
#' - `config.jsonc` -- arquivo de configuracao
#' - `usinas.csv` -- cadastro de usinas
#' - `geracao_observada.csv` -- dados de geracao historica
#' - `irradiancia_prevista.csv` -- previsoes NWP
#'
#' @examples
#' \dontrun{
#' parser <- get_parser()
#' args <- parser$parse_args(c("--parallel", "--workers", "4"))
#' print(args$parallel)
#' }
#'
#' @seealso [cli_main()], [argparse::ArgumentParser()]
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
#' @param parser Objeto `ArgumentParser`
#'
#' @return Parser com argumentos adicionados
#'
#' @keywords internal
inner_parser_generic_args <- function(parser) {
    help_msg <- paste0(
        "Diretorio de dados para execucao do modelo de previsao -- Veja ",
        "https://github.com/isabelafiuza/pfv-sh para detalhes"
    )
    parser$add_argument("--datadir",
        type = "character",
        default = "./data",
        help = help_msg
    )
    parser$add_argument("--parallel",
        action = "store_true",
        default = FALSE,
        help = "Habilita processamento paralelo de usinas"
    )
    parser$add_argument("--resume",
        action = "store_true",
        default = FALSE,
        help = "Habilita retomada do pipeline a partir do ultimo checkpoint"
    )
    parser$add_argument("--workers",
        type = "integer",
        default = NULL,
        help = "Numero de workers paralelos (padrao: auto-detect via availableCores() - 1)"
    )

    return(parser)
}
