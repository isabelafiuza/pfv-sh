.onLoad <- function(libname, pkgname) {
    lg <- logger_setup()
    assign("lg", lg, asNamespace("pfvsh"))
}

.onUnload <- function(libname, pkgname) {
    ns <- asNamespace("pfvsh")
    if (exists("lg", envir = ns, inherits = FALSE)) {
        try(rm(lg, envir = ns), silent = TRUE)
    }
}

utils::globalVariables(c(
    ".", "..col_var", "..cols_norm", "..var_exog",
    "data_fim", "data_hora", "data_hora_observacao", "data_hora_previsao",
    "data_hora_rodada", "data_inicio", "data_prev", "data_referencia",
    "dias_com_geracao", "distancia", "dt_prev_final",
    "ger_obs", "ger_usi", "hora_min",
    "id_modelo_nwp", "id_modelo_prev", "id_usina", "irrad_prev",
    "latitude", "longitude", "med",
    "passo_prev", "prev_met_usi",
    "v_horizonte", "v_modelos_nwp", "valor", "variavel"
))
