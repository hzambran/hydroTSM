# File QC_parallel.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Shared parallel backend for station quality-control workflows                 #
################################################################################

.QC_parallel_setup <- function(ncores=1L,
                               parallel.type=c("socket", "fork", "serial"),
                               cluster.nodes=NULL, nstations) {

  # Validate the requested worker count before creating external processes.
  if (!is.numeric(ncores) || length(ncores) != 1L || is.na(ncores) ||
      !is.finite(ncores) || ncores < 1 ||
      abs(ncores - round(ncores)) > sqrt(.Machine$double.eps))
    stop("Invalid argument: 'ncores' must be a positive integer !")
  ncores <- as.integer(round(ncores))
  nstations <- as.integer(nstations)
  parallel.type <- match.arg(parallel.type)

  # Validate optional PSOCK host names used for network clusters.
  if (!is.null(cluster.nodes) &&
      (!is.character(cluster.nodes) || length(cluster.nodes) == 0L ||
       anyNA(cluster.nodes) || any(!nzchar(cluster.nodes))))
    stop("Invalid argument: 'cluster.nodes' must be NULL or non-empty host names !")
  if (parallel.type != "socket" && !is.null(cluster.nodes))
    stop("Invalid argument: 'cluster.nodes' can only be used with 'parallel.type=\"socket\"' !")

  # Never create more workers than independent station columns.
  nworkers <- min(ncores, max(1L, nstations))
  local.node <- unname(Sys.info()[["nodename"]])
  if (is.null(local.node) || is.na(local.node) || !nzchar(local.node))
    local.node <- "localhost"

  # A single worker deliberately uses the zero-overhead serial path.
  if (nworkers == 1L || parallel.type == "serial") {
    if (parallel.type == "serial" && ncores != 1L)
      stop("Invalid argument: 'ncores' must be 1 when 'parallel.type=\"serial\"' !")
    return(list(type="serial", nworkers=1L, nnodes=1L,
                nodes=local.node, cluster=NULL))
  }

  # Fork workers share memory by copy-on-write but are unavailable on Windows.
  if (parallel.type == "fork") {
    if (.Platform$OS.type == "windows")
      stop("Invalid argument: 'parallel.type=\"fork\"' is not available on Windows; use 'socket' !")
    return(list(type="fork", nworkers=nworkers, nnodes=1L,
                nodes=local.node, cluster=NULL))
  }

  # Repeat host names so 'ncores' always controls the number of PSOCK workers.
  hosts <- if (is.null(cluster.nodes)) {
    rep("localhost", nworkers)
  } else {
    rep(cluster.nodes, length.out=nworkers)
  }
  library.paths <- .libPaths()
  package.version <- as.character(utils::packageVersion("hydroTSM"))
  old.rlibs <- Sys.getenv("R_LIBS", unset=NA_character_)
  Sys.setenv(R_LIBS=paste(library.paths, collapse=.Platform$path.sep))
  cluster <- tryCatch(
    parallel::makePSOCKcluster(hosts),
    finally={
      if (is.na(old.rlibs)) Sys.unsetenv("R_LIBS") else
        Sys.setenv(R_LIBS=old.rlibs)
    }
  )
  ready <- FALSE
  on.exit(if (!ready) parallel::stopCluster(cluster), add=TRUE)

  # Remote workers must have the same hydroTSM version installed and visible.
  invisible(parallel::clusterCall(cluster, function(paths, version) {
    # Retain node-local libraries and prepend any shared master libraries.
    shared.paths <- paths[dir.exists(paths)]
    .libPaths(unique(c(shared.paths, .libPaths())))
    suppressPackageStartupMessages(library("hydroTSM", character.only=TRUE))
    if (as.character(utils::packageVersion("hydroTSM")) != version)
      stop("Socket workers must use the same hydroTSM version as the master process !")
    TRUE
  }, library.paths, package.version))
  ready <- TRUE

  list(type="socket", nworkers=nworkers,
       nnodes=length(unique(hosts)), nodes=unique(hosts), cluster=cluster)

} # '.QC_parallel_setup' END


.QC_parallel_stop <- function(backend) {

  # Only clusters created for the socket backend require explicit shutdown.
  if (!is.null(backend$cluster))
    parallel::stopCluster(backend$cluster)

  invisible(NULL)

} # '.QC_parallel_stop' END


.QC_parallel_apply <- function(backend, x, fun, ...) {

  # Dispatch balanced coarse tasks without changing their returned order.
  if (is.character(fun) && length(fun) == 1L) {
    worker <- utils::getFromNamespace(fun, "hydroTSM")
  } else {
    worker <- match.fun(fun)
  }
  if (backend$type == "socket") {
    arguments <- list(...)
    worker.name <- if (is.character(fun)) fun else NULL
    if (!is.null(worker.name))
      return(parallel::parLapply(
        backend$cluster, x,
        function(item, name, arguments) {
          worker <- utils::getFromNamespace(name, "hydroTSM")
          do.call(worker, c(list(item), arguments))
        }, name=worker.name, arguments=arguments
      ))
    return(parallel::parLapply(backend$cluster, x, worker, ...))
  }
  if (backend$type == "fork")
    return(parallel::mclapply(x, worker, ..., mc.cores=backend$nworkers,
                              mc.preschedule=TRUE))

  lapply(x, worker, ...)

} # '.QC_parallel_apply' END


.QC_parallel_chunks <- function(nstations, nworkers) {

  # Contiguous chunks make result reconstruction deterministic and inexpensive.
  index <- seq_len(nstations)
  if (nstations == 1L || nworkers == 1L)
    return(list(index))
  groups <- cut(index, breaks=min(nstations, nworkers), labels=FALSE)
  unname(split(index, groups))

} # '.QC_parallel_chunks' END


.QC_parallel_parts <- function(x, backend) {

  # Transfer only each worker's station subset for station-local QC tests.
  chunks <- .QC_parallel_chunks(NCOL(x), backend$nworkers)
  lapply(chunks, function(index) {
    list(index=index, x=x[, index, drop=FALSE])
  })

} # '.QC_parallel_parts' END


.QC_station_parameter <- function(x, index, nstations, name) {

  # Preserve scalar controls and subset valid station-specific vectors.
  if (is.null(x) || length(x) == 1L)
    return(x)
  if (length(x) != nstations)
    stop("Invalid argument: '", name,
         "' must be scalar or contain one value per station !")

  x[index]

} # '.QC_station_parameter' END


.QC_combine_station_results <- function(parts, x) {

  # Reassemble named flag matrices in the original contiguous station order.
  flag.names <- names(parts[[1L]]$flags)
  flags <- stats::setNames(vector("list", length(flag.names)), flag.names)
  for (name in flag.names) {
    values <- do.call(cbind, lapply(parts, function(part)
      .precipQC_matrix(part$flags[[name]], allow.logical=TRUE)))
    flags[[name]] <- .precipQC_zoo(values, x)
  }

  # Breakpoint rows follow the same station-chunk order as the reconstructed data.
  breakpoint <- do.call(rbind, lapply(parts, `[[`, "breakpoint"))
  rownames(breakpoint) <- NULL

  list(flags=flags, breakpoint=breakpoint)

} # '.QC_combine_station_results' END


.QC_parallel_description <- function(backend) {

  # Return one concise description suitable for verbose workflow logs.
  paste0("Using ", backend$type, " backend with ", backend$nworkers,
         " worker(s) across ", backend$nnodes, " node(s): ",
         paste(backend$nodes, collapse=", "), ".")

} # '.QC_parallel_description' END
