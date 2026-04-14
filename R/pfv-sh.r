#' @title pfvsh: Previsao de Geracao Solar Fotovoltaica Semi-horaria
#'
#' @description
#' O pacote \code{pfvsh} fornece funcionalidades para previsao de geracao
#' solar fotovoltaica com resolucao semi-horaria (30 minutos), desenvolvido
#' para suportar a operacao do Sistema Interligado Nacional (SIN) brasileiro.
#'
#' @details
#' O pacote implementa dois modos de operacao:
#' \itemize{
#'   \item \strong{train}: Treinamento de modelos de previsao usando dados historicos
#'   \item \strong{predict}: Geracao de previsoes usando modelos pre-treinados
#' }
#'
#' \strong{Modelos de Previsao Suportados:}
#' \itemize{
#'   \item \strong{ARIMAX}: Auto-regressivo integrado de media movel com variaveis exogenas
#'   \item \strong{Fisico-Estimado}: Regressao linear simples (RLS) ou multipla (RLM)
#' }
#'
#' \strong{Caracteristicas Principais:}
#' \itemize{
#'   \item Previsoes semi-horarias (48 pontos por dia)
#'   \item Horizontes de D+0 a D+9 (ate 10 dias a frente)
#'   \item Suporte a multiplos modelos NWP (GFS, ECMWF)
#'   \item Associacao automatica de usinas as quadriculas NWP
#'   \item Interpolacao temporal de dados NWP
#' }
#'
#' @section Funcoes Principais:
#' \describe{
#'   \item{\code{\link{train_main}}}{Ponto de entrada para treinamento de modelos}
#'   \item{\code{\link{predict_main}}}{Ponto de entrada para geracao de previsoes}
#'   \item{\code{\link{parse_config}}}{Validacao e parsing de configuracao}
#'   \item{\code{\link{get_parser}}}{Construtor do parser de argumentos CLI}
#' }
#'
#' @section Dados de Entrada:
#' O pacote espera os seguintes arquivos no diretorio de dados:
#' \describe{
#'   \item{usinas.csv}{Cadastro de usinas (id, lat, lon, capacidade)}
#'   \item{geracao_observada.csv}{Dados historicos de geracao}
#'   \item{irradiancia_prevista.csv}{Previsoes NWP de irradiancia}
#'   \item{config.jsonc}{Arquivo de configuracao}
#' }
#'
#' @section Convencoes Temporais:
#' \itemize{
#'   \item Fuso horario: UTC
#'   \item Resolucao: 30 minutos
#'   \item Formato de data: ISO 8601
#' }
#'
#' @author Isabela Fiuza \email{isabela.fiuza@@ons.org.br}
#'
#'
#' @import utils stats
#' @import data.table argparse lgr pfvIO
#' @importFrom forecast auto.arima Arima forecast
#' @importFrom zoo na.approx
#' @importFrom lubridate hm days
#'
#' @keywords internal
"_PACKAGE"
