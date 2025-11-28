#' Interpreta Arquivo de Configuracao
#'
#' Valida e transforma os valores do arquivo de configuracao,
#' convertendo tipos e resolvendo referencias.
#'
#' @param config Lista nomeada de configuracoes (lida de JSON/JSONC)
#' @param conn Objeto de conexao com banco de dados (via pfvIO)
#'
#' @return Lista de argumentos interpretados com tipos corretos
#'
#' @details
#' A funcao executa:
#' \enumerate{
#'   \item Valida presenca de todas as chaves obrigatorias
#'   \item Valida tipos dos valores
#'   \item Converte \code{modelos_previsao} para objetos S3
#'   \item Converte \code{data_referencia} para Date
#'   \item Resolve \code{ids_usinas} vazio para todas as usinas
#'   \item Converte listas para vetores onde apropriado
#' }
#'
#' @seealso
#' \code{\link{valida_nomes_config}}, \code{\link{valida_tipos_config}}
#'
#' @export
parse_config <- function(config, conn) {
    valida_nomes_config(config)
    valida_tipos_config(config)

    config$modelos_previsao <- lapply(config$modelos_previsao, parsearg_modelos_previsao)
    config$data_referencia <- parsearg_data_referencia(config$data_referencia)
    config$ids_usinas <- parsearg_ids_usinas(config$ids_usinas, conn)
    config$horizonte_dias <- parsearg_horizonte_dias(config$horizonte_dias)
    config$modelos_NWP <- parsearg_modelos_nwp(config$modelos_NWP)
    return(config)
}

# VALIDACOES DE CONFIG -----------------------------------------------------------------------------

#' Valida Chaves do Arquivo de Configuracao
#'
#' Verifica se todas as chaves obrigatorias estao presentes.
#'
#' @param config Lista de configuracoes
#'
#' @return \code{NULL} invisivel se valido; gera erro se chaves faltantes
#'
#' @keywords internal
valida_nomes_config <- function(config) {
    nomes <- config_names()

    has_all <- all(nomes %in% names(config))

    if (!has_all) {
        falta <- nomes[!(nomes %in% names(config))]
        falta <- paste0(falta, collapse = ",")
        msg <- paste0("Arquivo de configuracao nao possui chaves (", falta, ")")
        stop(msg)
    }

    invisible(NULL)
}

#' Retorna Nomes das Chaves Obrigatorias
#'
#' @return Vetor de strings com nomes das chaves de configuracao
#' @keywords internal
config_names <- function() {
    c(
        "mode", "input", "output", "artifact", "ids_usinas", "data_referencia", "horizonte_dias",
        "modelos_NWP", "modelos_previsao", "parametros_periodo_geracao"
    )
}

#' Valida Tipos das Chaves de Configuracao
#'
#' Verifica se os valores possuem os tipos esperados.
#'
#' @param config Lista de configuracoes
#'
#' @return \code{NULL} invisivel se valido; gera erro se tipos incorretos
#'
#' @keywords internal
valida_tipos_config <- function(config) {
    tipos <- config_types()

    config <- config[names(tipos)]
    valid <- mapply(valid_tipos, config, tipos, SIMPLIFY = TRUE)
    all_valid <- all(valid)

    if (!all_valid) {
        invalid <- names(config)[!valid]
        invalid <- paste0(invalid, collapse = ",")
        msg <- paste0("Chaves (", invalid, ") nao possuem os tipos corretos")
        stop(msg)
    }

    invisible(NULL)
}

#' Retorna Tipos Esperados por Chave
#'
#' @return Lista nomeada com tipos esperados para cada chave
#' @keywords internal
config_types <- function() {
    structure(
        list(
            "character", "character", "character", "character", list("character", "NULL"),
            "character", list("character", "NULL"), "character", list("character", "numeric", "numeric"),
            list("numeric", "numeric", "numeric")
        ),
        names = config_names()
    )
}

#' Valida Tipos de Uma Chave (Escalar ou Lista)
#'
#' Verifica se todos os elementos de uma chave possuem tipos validos.
#'
#' @param l Valor da chave (escalar, vetor ou lista)
#' @param tipos Tipos esperados (character, ou lista para multiplos tipos aceitos)
#'
#' @return Logico indicando se validacao passou
#'
#' @keywords internal
valid_tipos <- function(l, tipos) all(sapply(l, valid_tipos_unit, tipos = tipos))

#' Valida Tipo de Um Elemento
#'
#' Funcao auxiliar para validacao unitaria de tipo.
#'
#' @param x Elemento a validar
#' @param tipos Tipos esperados
#'
#' @return Logico indicando se tipo e valido
#'
#' @keywords internal
valid_tipos_unit <- function(x, tipos) {
    tipos <- unlist(tipos)
    if (!is.list(x)) {
        return(any(inherits(x, tipos)))
    }

    Reduce("|", mapply(inherits, x = x, tipos))
}

# PARSERS ------------------------------------------------------------------------------------------

#' Parser de data_referencia
#'
#' Interpreta o parametro de data de referencia, que pode ser:
#' \itemize{
#'   \item Numerico: numero de dias antes de hoje
#'   \item Character: data no formato "YYYY-MM-DD"
#'   \item Lista: combinacao dos anteriores
#' }
#'
#' @param x Valor da chave data_referencia
#'
#' @return Vetor Date ou lista de Dates
#'
#' @export
parsearg_data_referencia <- function(x) UseMethod("parsearg_data_referencia")

#' @rdname parsearg_data_referencia
#' @details Para input numerico N, retorna intervalo de N+1 dias antes de hoje
#' @export
parsearg_data_referencia.numeric <- function(x) Sys.Date() - c(x + 1, 1)

#' @rdname parsearg_data_referencia
#' @details Para input character, converte diretamente para Date
#' @export
parsearg_data_referencia.character <- function(x) as.Date(x)

#' @rdname parsearg_data_referencia
#' @details Para input lista, processa cada elemento recursivamente
#' @export
parsearg_data_referencia.list <- function(x) {
    lapply(x, function(elem) parsearg_data_referencia(elem))
}

#' Parser de ids_usinas
#'
#' Interpreta lista de IDs de usinas. Se vazia, retorna todas as usinas
#' disponiveis no banco de dados.
#'
#' @param x Lista de IDs de usinas (pode ser vazia)
#' @param conn Objeto de conexao com banco de dados
#'
#' @return Vetor character com IDs unicos de usinas
#'
#' @keywords internal
parsearg_ids_usinas <- function(x, conn) {
    if (length(x) == 0) x <- get_usinas(conn)$id_usina else x <- unlist(x)
    return(unique(x))
}

#' Parser de horizonte_dias
#'
#' Converte lista de horizontes para vetor character.
#'
#' @param x Lista de strings com horizontes (e.g., list("D+0", "D+1"))
#'
#' @return Vetor character com horizontes
#'
#' @keywords internal
parsearg_horizonte_dias <- function(x) {
    x <- unlist(x)
    return(x)
}

#' Parser de modelos_NWP
#'
#' Converte lista de modelos NWP para vetor character.
#'
#' @param x Lista de strings com modelos NWP (e.g., list("GFS", "ECMWF"))
#'
#' @return Vetor character com modelos NWP
#'
#' @keywords internal
parsearg_modelos_nwp <- function(x) {
    x <- unlist(x)
    return(x)
}

#' Parser de modelos_previsao
#'
#' Converte configuracao de modelo de previsao em objeto S3 com classe
#' definida pelo campo "tipo".
#'
#' @param x Lista com parametros do modelo, incluindo campo "tipo"
#'
#' @return Lista com classe S3 igual ao valor de "tipo"
#'
#' @keywords internal
parsearg_modelos_previsao <- function(x) {
    nome <- x$tipo
    class(x) <- nome
    return(x)
}
