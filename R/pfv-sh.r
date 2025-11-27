#' `pfv.sh`
#'
#' Modelo para Previsao De Solar Fotovoltaica
#'
#' Este pacote contem as funcionalidades necessarias para a realizacao de previsoes
#' de geracao solar fotovoltaica a partir de dados meteorologicos.
#'
#' @import utils stats
#' @import data.table argparse lgr pfvIO
#' @importFrom forecast auto.arima Arima forecast
#' @importFrom zoo na.approx
#' @importFrom lubridate hm days
#'
#' @keywords internal
"_PACKAGE"

# Global variables for data.table NSE
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
