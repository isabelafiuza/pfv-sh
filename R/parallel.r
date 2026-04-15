#' @importFrom future plan availableCores nbrOfWorkers
#' @importFrom future.apply future_lapply
NULL

#' Configura Backend de Execucao Paralela
#'
#' Inicializa o plano de execucao paralela do pacote `future` com a estrategia
#' e numero de workers especificados. O plano anterior e retornado
#' invisivelmente para permitir restauracao posterior via [reset_parallel_plan()].
#'
#' Quando `workers = NULL`, a variavel de ambiente `PFVSH_WORKERS` e consultada
#' como fallback antes de recorrer ao auto-detect via `availableCores() - 1`.
#'
#' @param workers inteiro positivo indicando o numero de workers, ou `NULL`
#'     para usar `PFVSH_WORKERS` (se definida) ou `future::availableCores() - 1`
#'     (minimo 1)
#' @param strategy character escalar com a estrategia de paralelismo. Deve ser
#'     um entre `"multisession"`, `"multicore"` ou `"sequential"`
#'
#' @return o plano anterior (invisivelmente), como retornado por `future::plan()`
#'
#' @examples
#' old <- setup_parallel_plan(workers = 2L, strategy = "multisession")
#' get_parallel_config()
#' reset_parallel_plan(old)
#'
#' @export
setup_parallel_plan <- function(
    workers = NULL,
    strategy = c("multisession", "multicore", "sequential")
) {
    strategy <- match.arg(strategy)

    if (is.null(workers)) {
        workers <- read_env_workers_fallback()
    }

    validate_workers(workers)

    if (is.null(workers)) {
        workers <- max(1L, future::availableCores() - 1L)
    }

    lg <- lgr::get_logger("pfvsh")

    if (strategy == "sequential") {
        old_plan <- future::plan("sequential")
        lg$info("Plano paralelo configurado: strategy=sequential")
        return(invisible(old_plan))
    }

    old_plan <- future::plan(strategy, workers = workers)
    lg$info("Plano paralelo configurado: strategy=%s, workers=%d",
        strategy, workers)

    invisible(old_plan)
}

#' Restaura Plano de Execucao Paralela Anterior
#'
#' Restaura um plano de execucao paralela salvo previamente, tipicamente
#' obtido como retorno de [setup_parallel_plan()].
#'
#' @param old_plan plano anterior retornado por [setup_parallel_plan()]
#'
#' @return `invisible(NULL)`
#'
#' @examples
#' old <- setup_parallel_plan(strategy = "sequential")
#' reset_parallel_plan(old)
#'
#' @export
reset_parallel_plan <- function(old_plan) {
    future::plan(old_plan)
    invisible(NULL)
}

#' Consulta Configuracao Atual do Plano Paralelo
#'
#' Retorna a configuracao ativa do backend paralelo, incluindo o numero de
#' workers e a estrategia em uso.
#'
#' @return lista nomeada com os elementos:
#' \describe{
#'   \item{workers}{inteiro com o numero de workers ativos}
#'   \item{strategy}{character com o nome da estrategia em uso}
#' }
#'
#' @examples
#' get_parallel_config()
#'
#' @export
get_parallel_config <- function() {
    list(
        workers = future::nbrOfWorkers(),
        strategy = extract_strategy_name(future::plan())
    )
}

#' Divide Argumentos por Usina para Despacho Paralelo
#'
#' Particiona os data.tables contidos em `extra_args` por `id_usina`,
#' gerando uma lista de argumentos por usina pronta para ser enviada a
#' workers paralelos. Elementos que nao sao data.tables ou que nao possuem
#' a coluna `id_usina` sao replicados inalterados para cada usina.
#'
#' Esta funcao trata o schema do pfv-sh: `data_set_ger` (data.table com coluna
#' `id_usina`) e particionado por usina; `data_set_met` (lista nomeada de
#' data.tables) nao e um data.table em si e e replicado para cada worker.
#' O filtro por usina nos elementos de `data_set_met` ocorre dentro da funcao
#' worker (`treina_usina` / `predict_usina`).
#'
#' @param extra_args lista nomeada de argumentos, como construida por
#'     `predict_main` ou `train_main`
#' @param v_usinas vetor de IDs de usinas a processar
#'
#' @return lista nomeada por `id_usina`, onde cada elemento e uma lista
#'     com a mesma estrutura de `extra_args`, porem com data.tables filtrados
#'     para a usina correspondente, mais o elemento `.iu` com o ID da usina
#'
#' @export
split_args_by_plant <- function(extra_args, v_usinas) {
    is_splittable <- vapply(extra_args, function(x) {
        data.table::is.data.table(x) && "id_usina" %in% names(x)
    }, logical(1L))

    splittable_names <- names(extra_args)[is_splittable]

    splits <- lapply(splittable_names, function(nm) {
        split(extra_args[[nm]], by = "id_usina", keep.by = TRUE)
    })
    names(splits) <- splittable_names

    empties <- lapply(splittable_names, function(nm) {
        extra_args[[nm]][0L]
    })
    names(empties) <- splittable_names

    shared <- extra_args[!is_splittable]

    out <- lapply(v_usinas, function(iu) {
        per_plant <- lapply(splittable_names, function(nm) {
            result <- splits[[nm]][[iu]]
            if (is.null(result)) empties[[nm]] else result
        })
        names(per_plant) <- splittable_names
        c(list(.iu = iu), per_plant, shared)
    })
    names(out) <- v_usinas
    out
}

#' Extrai Nome da Estrategia do Plano Atual
#'
#' @param plan_obj objeto de plano retornado por `future::plan()`
#'
#' @return character com o nome da estrategia
#'
#' @keywords internal
extract_strategy_name <- function(plan_obj) {
    known <- c("sequential", "multisession", "multicore")
    classes <- class(plan_obj)
    found <- intersect(classes, known)
    if (length(found) == 0L) return(classes[1L])
    found[1L]
}

#' Le Numero de Workers da Variavel de Ambiente
#'
#' Consulta `PFVSH_WORKERS` como fallback para o numero de workers. Retorna
#' `NULL` silenciosamente se a variavel nao estiver definida, ou emite aviso
#' via logger se o valor nao puder ser interpretado como inteiro positivo.
#'
#' @return inteiro positivo ou `NULL`
#'
#' @keywords internal
read_env_workers_fallback <- function() {
    env_workers <- Sys.getenv("PFVSH_WORKERS", unset = "")
    if (env_workers == "") return(NULL)

    parsed <- suppressWarnings(as.integer(env_workers))
    if (is.na(parsed) || parsed < 1L) {
        lgr::get_logger("pfvsh")$warn(
            "PFVSH_WORKERS invalido: '%s'. Usando auto-detect.", env_workers
        )
        return(NULL)
    }
    parsed
}

#' Verifica Validade do Numero de Workers
#'
#' @param workers valor a verificar
#'
#' @return `TRUE` se valido, `FALSE` caso contrario
#'
#' @keywords internal
is_valid_worker_count <- function(workers) {
    is.numeric(workers) && length(workers) == 1L &&
        !is.na(workers) && workers >= 1L && workers == as.integer(workers)
}

#' Valida Numero de Workers
#'
#' Lanca erro descritivo se `workers` nao for `NULL` nem um inteiro positivo.
#'
#' @param workers valor a validar (`NULL` e permitido)
#'
#' @return `invisible(NULL)`
#'
#' @keywords internal
validate_workers <- function(workers) {
    if (is.null(workers)) return(invisible(NULL))
    if (!is_valid_worker_count(workers)) {
        stop(
            "workers deve ser NULL ou um inteiro positivo, recebido: ",
            deparse(workers),
            call. = FALSE
        )
    }
    invisible(NULL)
}
