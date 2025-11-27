#' Interpreta Arquivo De Configuracao
#'
#' @param config lista nomeada de configuracoes lida do arquivo
#' @param conn objeto de conexao com um banco
#'
#' @return lista de argumentos interpretados
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

#' Valida Chaves Do Arquivo De Configuracao
#'
#' Checa se um arquivo de configuracao lido possui todas as chaves necessarias
#'
#' @param config lista de configuracoes
#'
#' @return NULL se config possui todas as chaves; levanta erro do contrario
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

config_names <- function() {
    c(
        "mode", "input", "output", "artifact", "ids_usinas", "data_referencia", "horizonte_dias",
        "modelos_NWP", "modelos_previsao", "parametros_periodo_geracao"
    )
}

#' Valida Tipos Das Chaves Do Arquivo De Configuracao
#'
#' Checa se valores das chaves no arquivo lido sao dos tipos corretos
#'
#' @param config lista de configuracoes
#'
#' @return NULL se config possui todos os tipos corretos; levanta erro do contrario
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

#' Validacao Singular De Uma Chave
#'
#' Funcao interna auxiliar de `valida_tipos_config`
#'
#' Tanto `l` quanto `tipos` podem ser escalares, vetores ou listas. No caso de `l`, cada elemento sera checado
#' individualmente. Se `tipos` for uma lista, `l` sera checado contra cada um dos tipos e retorna
#' `TRUE` se a correspondencia for valida
#'
#' @param l valor de uma chave do arquivo de configuracao, escalar ou lista
#' @param tipos tipos esperados de `l`, escalar ou lista
#'
#' @return booleano indicando se validacao encerrou com sucesso ou nao
valid_tipos <- function(l, tipos) all(sapply(l, valid_tipos_unit, tipos = tipos))

#' Auxiliar De `valid_tipos`
#'
#' Funcao interna para isolar o loop ao longo de `l` em `valid_tipos`
#'
#' @param x escalar, vetor ou lista, elemento de uma chave do arquivo de configuracao
#' @param tipos tipos esperados de `x`, escalar, vetor ou lista
#'
#' @return booleano indicando se validacao encerrou com sucesso ou nao
valid_tipos_unit <- function(x, tipos) {
    tipos <- unlist(tipos)
    if (!is.list(x)) {
        return(any(inherits(x, tipos)))
    }

    Reduce("|", mapply(inherits, x = x, tipos))
}

# PARSERS ------------------------------------------------------------------------------------------

#' Interpretador De Chave `data_referencia`
#'
#' Funcao interna de `parse_config` para interpretar o parametro `data_referencia` da configuracao
#'
#' @param x valor da chave `data_referencia`; numerico ou vetor de duas strings de data
#'
#' @return vetor `Date` de duas posicoes indicando inicio e fim da janela de simulacao
#'
#' @export
parsearg_data_referencia <- function(x) UseMethod("parsearg_data_referencia")

#' @rdname parsearg_data_referencia
#' @export
parsearg_data_referencia.numeric <- function(x) Sys.Date() - c(x + 1, 1)

#' @rdname parsearg_data_referencia
#' @export
parsearg_data_referencia.character <- function(x) as.Date(x)

#' @rdname parsearg_data_referencia
#' @export
parsearg_data_referencia.list <- function(x) {
    lapply(x, function(elem) parsearg_data_referencia(elem))
}

#' Interpretador De Chave `ids_usinas`
#'
#' Funcao interna de `parse_config` para interpretar o parametro `ids_usinas` da configuracao
#'
#' @param x valor da chave `ids_usinas`; lista vazia ou de codigos de usinas
#' @param conn objeto de conexao com um banco
#'
#' @return se `x` era uma lista vazia, retorna um vetor com todos os ids no banco `conn`; do
#'     contrario retorna `x` vetorizado

parsearg_ids_usinas <- function(x, conn) {
    if (length(x) == 0) x <- get_usinas(conn)$id_usina else x <- unlist(x)
    return(unique(x))
}

#' Interpretador De Chave `horizonte_dias`
#'
#' Funcao interna de parse_config para interpretar o parametro `horizonte_dias` da configuracao
#'
#' @param x valor da chave `horizonte_dias`; lista de strings com horizontes
#'
#' @return vetor de strings com horizontes

parsearg_horizonte_dias <- function(x) {
    x <- unlist(x)
    return(x)
}

#' Interpretador De Chave `modelos_NWP`
#'
#' Funcao interna de parse_config para interpretar o parametro `modelos_NWP` da configuracao
#'
#' @param x valor da chave `modelos_NWP`; lista de modelos NWP
#'
#' @return vetor de strings com modelos NWP

parsearg_modelos_nwp <- function(x) {
    x <- unlist(x)
    return(x)
}

#' Interpretador De Chave `modelos_previsao`
#'
#' Funcao interna de parse_config para interpretar o parametro `modelos_previsao` da configuracao
#'
#' @param x valor da chave `modelos_previsao`; lista com parametros do modelo
#'
#' @return lista com classe definida pelo tipo do modelo

parsearg_modelos_previsao <- function(x) {
    nome <- x$tipo
    class(x) <- nome
    return(x)
}
