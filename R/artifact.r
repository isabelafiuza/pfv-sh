#' Constroi Artefato de Ensemble de Modelos Enriquecido
#'
#' Monta artefato com modelos ajustados da usina e metadados de proveniencia.
#' Cada usina possui N modelos — um por combinacao NWP × horizonte × meia-hora × tipo.
#'
#' @param id_usina character, identificador da usina
#' @param models lista de pares `combinacao_ajuste` + `modelo` de [treina_usina()]
#' @param config lista com configuracao de treinamento
#'
#' @return lista com `id_usina`, `models`, e `metadata` (tipo, contagens, timestamps, hash)
#'
#' @examples
#' cfg <- list(
#'     ids_usinas = "BAUFI1",
#'     modelos_previsao = c("arimax"),
#'     horizonte_dias = 10L
#' )
#' models <- list(
#'     list(
#'         combinacao_ajuste = list(
#'             id_modelo_nwp = "GFS", horiz_prev = 1L,
#'             hora_min = "00:00", modelo_prev = "arimax"
#'         ),
#'         modelo = list(
#'             modelo_escolhido = "arimax",
#'             modelo_final = NULL,
#'             variaveis_usadas = character(0)
#'         )
#'     )
#' )
#' artifact <- build_model_artifact("BAUFI1", models, cfg)
#'
#' @export
build_model_artifact <- function(id_usina, models, config) {
    list(
        id_usina = id_usina,
        models = models,
        metadata = list(
            type = "pfvsh_model_ensemble",
            n_combinacoes = length(models),
            n_modelos_validos = count_valid_models(models),
            timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            package_version = as.character(utils::packageVersion("pfvsh")),
            config_hash = digest::digest(
                normalize_config_for_hash(config), algo = "sha256"
            )
        )
    )
}

#' Valida Artefato de Modelos
#'
#' Verifica estrutura de artefato de modelos. Artefatos antigos (lista plana)
#' sao aceitos com aviso para compatibilidade retroativa.
#'
#' @param artifact lista, artefato a ser validado
#'
#' @return `invisible(TRUE)` se valido; stop() caso contrario
#'
#' @examples
#' cfg <- list(ids_usinas = "BAUFI1", horizonte_dias = 10L)
#' models <- list(
#'     list(
#'         combinacao_ajuste = list(
#'             id_modelo_nwp = "GFS", horiz_prev = 1L,
#'             hora_min = "00:00", modelo_prev = "arimax"
#'         ),
#'         modelo = list(
#'             modelo_escolhido = "arimax",
#'             modelo_final = NULL,
#'             variaveis_usadas = character(0)
#'         )
#'     )
#' )
#' artifact <- build_model_artifact("BAUFI1", models, cfg)
#' validate_artifact(artifact)
#'
#' @export
validate_artifact <- function(artifact) {
    if (!is.list(artifact)) {
        stop("Artefato deve ser uma lista", call. = FALSE)
    }

    if (is_legacy_artifact(artifact)) {
        lg <- lgr::get_logger("pfvsh")
        lg$warn("Artefato carregado em formato antigo (sem metadados)")
        return(invisible(TRUE))
    }

    errors <- character(0L)
    errors <- check_artifact_id_usina(errors, artifact)
    errors <- check_artifact_models(errors, artifact)

    if (length(errors) > 0L) {
        msg <- paste0(
            "Validacao do artefato falhou:\n",
            paste0("- ", errors, collapse = "\n")
        )
        stop(msg, call. = FALSE)
    }

    check_artifact_metadata(artifact)

    invisible(TRUE)
}

is_legacy_artifact <- function(artifact) {
    !any(c("id_usina", "models", "metadata") %in% names(artifact))
}

#' @keywords internal
count_valid_models <- function(models) {
    sum(vapply(models, function(m) {
        !identical(m$modelo$modelo_escolhido, "ARIMA(0,0,0)")
    }, logical(1L)))
}

#' @keywords internal
check_artifact_id_usina <- function(errors, artifact) {
    if (!"id_usina" %in% names(artifact)) {
        return(c(errors, "Campo obrigatorio ausente: 'id_usina'"))
    }
    if (!is.character(artifact$id_usina) || length(artifact$id_usina) != 1L) {
        return(c(errors, "'id_usina' deve ser character escalar"))
    }
    errors
}

#' @keywords internal
check_artifact_models <- function(errors, artifact) {
    if (!"models" %in% names(artifact)) {
        return(c(errors, "Campo obrigatorio ausente: 'models'"))
    }
    if (!is.list(artifact$models) || length(artifact$models) == 0L) {
        return(c(errors, "'models' deve ser lista nao-vazia"))
    }
    errors
}

#' @keywords internal
check_artifact_metadata <- function(artifact) {
    if (!"metadata" %in% names(artifact)) {
        lg <- lgr::get_logger("pfvsh")
        lg$warn(
            "Artefato para usina '%s' sem metadados (formato antigo)",
            artifact$id_usina
        )
    }
    invisible(NULL)
}
