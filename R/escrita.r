#' Escrita de Melhor Historico de Geracao
#'
#' Salva os dados de melhor historico de geracao em disco, formatados e validados
#'
#' @param dt `data.table` com os dados a serem salvos
#' @param output_dir diretorio de saida onde sera salvo o arquivo CSV
#'
#' @return vazio, apenas escreve arquivo
write_previsao_geracao_fotovoltaica <- function(dt, output_dir = ".") {
    lg <- get_pkg_logger()
    lg$debug("Escrevendo dados de previsao de geracao fotovoltaica...")

    write_dataset(dt, "previsao_geracao_fotovoltaica.parquet", output_dir)

    lg$debug("Dados de previsao de geracao fotovoltaica salvos com sucesso")
}