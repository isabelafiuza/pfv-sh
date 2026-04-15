#' Constroi Relatorio de Saude do Pipeline
#'
#' Agrega proveniencia e metricas em relatorio com classificacao de saude.
#' Registra aviso se proveniencia nao estiver finalizada.
#'
#' @param provenance environment/lista de proveniencia
#' @param metrics lista de metricas, ou `NULL`
#'
#' @return lista com saude do pipeline e relatorio por usina
build_health_report <- function(provenance, metrics = NULL) {
    stopifnot(is.environment(provenance) || is.list(provenance))

    lg <- lgr::get_logger("pfvsh")
    if (provenance$status == "running") {
        lg$warn("Relatorio de saude gerado com proveniencia nao finalizada")
    }

    plant_reports <- build_plant_reports(provenance, metrics)
    plant_healths <- vapply(plant_reports, function(p) p$health, character(1L))

    overall <- classify_overall_health(provenance, plant_healths)

    list(
        run_id = provenance$run_id,
        mode = provenance$mode,
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        overall_health = overall,
        summary = list(
            status = provenance$status,
            duration_seconds = provenance$duration_seconds,
            n_plants = provenance$n_plants,
            n_plants_completed = sum(plant_healths != "failed"),
            n_plants_failed = sum(plant_healths == "failed"),
            n_plants_warning = sum(plant_healths == "warning"),
            package_version = provenance$package_version,
            config_hash = provenance$config_hash
        ),
        plants = plant_reports,
        warnings = collect_warnings(plant_reports),
        errors = collect_errors(plant_reports)
    )
}

classify_plant_health <- function(provenance_status, plant_metrics) {
    if (provenance_status != "completed") return("failed")
    "healthy"
}

classify_overall_health <- function(provenance, plant_healths) {
    if (provenance$status == "failed") return("failed")
    if (any(plant_healths == "failed")) return("failed")
    if (any(plant_healths == "warning")) return("degraded")
    "healthy"
}

#' Escreve Relatorio de Saude em JSON
#'
#' Serializa relatorio como `health-{run_id}.json`.
#' Falhas de I/O geram aviso no log, sem interrupao do pipeline.
#'
#' @param report lista gerada por [build_health_report()]
#' @param output_dir character, diretorio de saida
#'
#' @return filepath (invisivelmente)
write_health_report <- function(report, output_dir) {
    lg <- lgr::get_logger("pfvsh")
    filename <- paste0("health-", report$run_id, ".json")
    filepath <- file.path(output_dir, filename)

    tryCatch({
        if (!dir.exists(output_dir)) {
            suppressWarnings(dir.create(output_dir, recursive = TRUE))
        }
        json_str <- jsonlite::toJSON(
            report, pretty = TRUE, auto_unbox = TRUE, null = "null"
        )
        writeLines(json_str, filepath)
        lg$info("Relatorio de saude escrito em: %s", filepath)
    }, error = function(e) {
        lg$warn(
            "Falha ao escrever relatorio de saude em '%s': %s",
            filepath, conditionMessage(e)
        )
    })

    invisible(filepath)
}

build_plant_reports <- function(provenance, metrics) {
    reports <- list()
    for (iu in provenance$plant_ids) {
        prov_status <- provenance$plant_status[[iu]]
        if (is.null(prov_status)) prov_status <- "pending"

        plant_metrics <- if (!is.null(metrics)) metrics$plants[[iu]] else NULL

        data_quality <- NULL
        if (!is.null(plant_metrics$data_volume)) {
            data_quality <- list(
                na_rate = plant_metrics$data_volume$na_rate,
                n_rows = plant_metrics$data_volume$n_rows
            )
        }

        reports[[iu]] <- list(
            health = classify_plant_health(prov_status, plant_metrics),
            provenance_status = prov_status,
            duration_seconds = plant_metrics$duration_seconds,
            data_quality = data_quality
        )
    }
    reports
}

collect_warnings <- function(plant_reports) {
    character(0L)
}

collect_errors <- function(plant_reports) {
    failed_plants <- names(plant_reports)[
        vapply(plant_reports, function(p) p$health == "failed", logical(1L))
    ]
    sprintf("Usina %s: processamento falhou", failed_plants)
}
