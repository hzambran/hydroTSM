# File streamflowQC.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Daily and sub-daily streamflow quality-control workflows                     #
################################################################################

.streamflowQC_progress <- function(verbose, ...) {

  # Use message() so progress can be captured independently of results.
  if (verbose)
    message("[streamflowQC] ", ...)

  invisible(NULL)

} # '.streamflowQC_progress' END


.streamflowQC_finish <- function(
  prepared, flags, breakpoint, highflow, correction,
  max.missing, max.review, max.suspicious, min.years, min.evidence,
  hard.tests, discard.breakpoint, spatial.estimate, spatial.score,
  spatial.support, neighbours, resolution, settings) {

  # Point-level evidence and the common correction audit use the established QC engine.
  out <- .precipQC_finish(
    prepared, flags, breakpoint, correction, max.missing, max.suspicious,
    min.years, min.evidence, hard.tests, discard.breakpoint,
    spatial.estimate, spatial.score, neighbours, resolution, settings,
    object.class="streamflowQC"
  )

  # Excessive review flags may discard a station without rejecting every point.
  if (!is.numeric(max.review) || length(max.review) != 1L ||
      is.na(max.review) || !is.finite(max.review) ||
      max.review < 0 || max.review > 1)
    stop("Invalid argument: 'max.review' must be in [0, 1] !")
  summary <- out$station.summary
  excessive.review <- summary$review.percent > max.review
  newly.discarded <- excessive.review & summary$recommendation == "accept"
  summary$recommendation[excessive.review] <- "discard"
  summary$reason[newly.discarded] <- "review fraction exceeds limit"
  already.discarded <- excessive.review & !newly.discarded
  summary$reason[already.discarded] <- paste(
    summary$reason[already.discarded], "review fraction exceeds limit",
    sep="; "
  )
  out$station.summary <- summary

  # Rebuild station partitions after applying the review-fraction criterion.
  accept <- summary$recommendation == "accept"
  diagnostics <- summary[, setdiff(names(summary), "station"), drop=FALSE]
  metadata <- cbind(prepared$metadata, diagnostics)
  out$accepted.metadata <- metadata[accept, , drop=FALSE]
  out$discarded.metadata <- metadata[!accept, , drop=FALSE]
  out$accepted.data <- zoo::zoo(prepared$values[, accept, drop=FALSE],
                                prepared$datetimes)
  out$discarded.data <- zoo::zoo(prepared$values[, !accept, drop=FALSE],
                                 prepared$datetimes)

  # Reconstruct the corrected full matrix from the auditable correction table.
  corrected <- prepared$values
  if (NROW(out$corrections) > 0L) {
    rows <- match(out$corrections$time, prepared$datetimes)
    columns <- match(out$corrections$station, prepared$stations)
    corrected[cbind(rows, columns)] <- out$corrections$corrected
  }
  out$accepted.corrected <- zoo::zoo(corrected[, accept, drop=FALSE],
                                     prepared$datetimes)
  out$discarded.corrected <- zoo::zoo(corrected[, !accept, drop=FALSE],
                                      prepared$datetimes)

  # Preserve high-flow and spatial-corroboration diagnostics even when unavailable.
  out["highflow"] <- list(highflow)
  out["return.period"] <- list(if (is.null(highflow)) NULL else
                                highflow$return.period)
  out["spatial.support"] <- list(spatial.support)
  out$settings$max.review <- max.review
  class(out) <- c("streamflowQC", "list")
  out

} # '.streamflowQC_finish' END


.streamflowQC_daily_station_part <- function(part, checks, parameters) {

  # Run station-local daily tests on one balanced column subset.
  x <- part$x
  flags <- list()
  if (checks["duplicate"])
    flags$duplicate <- streamflowQC_duplicate(
      x, min.month.values=parameters$duplicate.min.month,
      min.year.values=parameters$duplicate.min.year,
      min.nonzero=parameters$duplicate.min.nonzero
    )
  if (checks["gap"])
    flags$gap <- streamflowQC_gap(x, interval=1)
  if (checks["climatology"])
    flags$climatology <- streamflowQC_climatology(
      x, group="dayofyear", window=parameters$climatology.window,
      z=parameters$climatology.z,
      min.samples=parameters$climatology.min.samples,
      offset=parameters$log.offset
    )
  if (checks["flatline"])
    flags$flatline <- streamflowQC_flatline(
      x, run=parameters$flatline.run,
      tolerance=parameters$flatline.tolerance,
      flag.zero=parameters$flag.zero
    )
  if (checks["spike"])
    flags$spike <- streamflowQC_spike(
      x, factor=parameters$spike.factor, prob=parameters$spike.prob
    )
  if (checks["rate"]) {
    max.rise <- .QC_station_parameter(
      parameters$rate.max.rise, part$index, parameters$nstations,
      "rate.max.rise"
    )
    max.fall <- .QC_station_parameter(
      parameters$rate.max.fall, part$index, parameters$nstations,
      "rate.max.fall"
    )
    flags$rate <- streamflowQC_rate(
      x, group="month", z=parameters$rate.z,
      min.samples=parameters$rate.min.samples,
      max.rise=max.rise, max.fall=max.fall,
      drop.factor=parameters$rate.drop.factor,
      offset=parameters$log.offset
    )
  }

  # Keep correlated high-flow diagnostics together as one evidence family.
  highflow <- NULL
  if (checks["highflow"]) {
    highflow <- streamflowQC_highflow(
      x, log.z=parameters$highflow.log.z,
      amax.ratio=parameters$highflow.amax.ratio,
      return.period=parameters$highflow.return.period,
      min.years=parameters$highflow.min.years,
      offset=parameters$log.offset
    )
    flags$highflow <- highflow$flags
  }

  # Keep homogeneity diagnostics aligned with the same station subset.
  breakpoint <- if (checks["breakpoint"]) {
    streamflowQC_breakpoint(
      x, min.years=parameters$breakpoint.min.years,
      alpha=parameters$breakpoint.alpha,
      min.relative.change=parameters$breakpoint.min.relative.change,
      min.completeness=parameters$breakpoint.min.completeness
    )
  } else {
    .precipQC_empty_breakpoint(colnames(x))
  }

  list(flags=flags, breakpoint=breakpoint, highflow=highflow)

} # '.streamflowQC_daily_station_part' END


.streamflowQC_subdaily_station_part <- function(part, checks, parameters) {

  # Run station-local sub-daily tests on one balanced column subset.
  x <- part$x
  flags <- list()
  if (checks["gap"])
    flags$gap <- streamflowQC_gap(x, interval=parameters$interval.seconds)
  if (checks["climatology"])
    flags$climatology <- streamflowQC_climatology(
      x, group="month", window=1L, z=parameters$climatology.z,
      min.samples=parameters$climatology.min.samples,
      offset=parameters$log.offset
    )
  if (checks["flatline"])
    flags$flatline <- streamflowQC_flatline(
      x, run=parameters$flatline.run,
      high.run=parameters$high.flatline.run,
      high.prob=parameters$flatline.high.prob,
      tolerance=parameters$flatline.tolerance,
      flag.zero=parameters$flag.zero
    )
  if (checks["spike"])
    flags$spike <- streamflowQC_spike(
      x, factor=parameters$spike.factor, prob=parameters$spike.prob
    )
  if (checks["rate"]) {
    max.rise <- .QC_station_parameter(
      parameters$rate.max.rise, part$index, parameters$nstations,
      "rate.max.rise"
    )
    max.fall <- .QC_station_parameter(
      parameters$rate.max.fall, part$index, parameters$nstations,
      "rate.max.fall"
    )
    flags$rate <- streamflowQC_rate(
      x, group="month", z=parameters$rate.z,
      min.samples=parameters$rate.min.samples,
      max.rise=max.rise, max.fall=max.fall,
      drop.factor=parameters$rate.drop.factor,
      offset=parameters$log.offset
    )
  }
  if (checks["fluctuation"])
    flags$fluctuation <- streamflowQC_fluctuation(
      x, window=parameters$fluctuation.window,
      minimum=parameters$fluctuation.minimum,
      low.diff.prob=parameters$fluctuation.low.diff.prob
    )

  # Keep correlated high-flow diagnostics together as one evidence family.
  highflow <- NULL
  if (checks["highflow"]) {
    highflow <- streamflowQC_highflow(
      x, log.z=parameters$highflow.log.z,
      amax.ratio=parameters$highflow.amax.ratio,
      return.period=parameters$highflow.return.period,
      min.years=parameters$highflow.min.years,
      offset=parameters$log.offset
    )
    flags$highflow <- highflow$flags
  }

  # Keep homogeneity diagnostics aligned with the same station subset.
  breakpoint <- if (checks["breakpoint"]) {
    streamflowQC_breakpoint(
      x, min.years=parameters$breakpoint.min.years,
      alpha=parameters$breakpoint.alpha,
      min.relative.change=parameters$breakpoint.min.relative.change,
      min.completeness=parameters$breakpoint.min.completeness
    )
  } else {
    .precipQC_empty_breakpoint(colnames(x))
  }

  list(flags=flags, breakpoint=breakpoint, highflow=highflow)

} # '.streamflowQC_subdaily_station_part' END


.streamflowQC_combine_station_results <- function(parts, x,
                                                   combine.highflow) {

  # Reuse the common deterministic flag and breakpoint reconstruction.
  combined <- .QC_combine_station_results(parts, x)
  if (!combine.highflow) {
    combined$highflow <- NULL
    return(combined)
  }

  # Reassemble the correlated high-flow diagnostics without changing order.
  highflow.parts <- lapply(parts, `[[`, "highflow")
  component.names <- names(highflow.parts[[1L]]$components)
  components <- stats::setNames(lapply(component.names, function(name) {
    values <- do.call(cbind, lapply(highflow.parts, function(part)
      .precipQC_matrix(part$components[[name]], allow.logical=TRUE)))
    .precipQC_zoo(values, x)
  }), component.names)
  return.values <- do.call(cbind, lapply(highflow.parts, function(part)
    .precipQC_matrix(part$return.period)))
  diagnostics <- do.call(rbind, lapply(highflow.parts, `[[`, "diagnostics"))
  rownames(diagnostics) <- NULL
  highflow <- list(
    flags=combined$flags$highflow, components=components,
    return.period=.precipQC_zoo(return.values, x),
    diagnostics=diagnostics
  )
  class(highflow) <- c("streamflowQC_highflow", "list")
  combined$highflow <- highflow
  combined

} # '.streamflowQC_combine_station_results' END


streamflowQC_daily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, gap=TRUE, climatology=TRUE,
           flatline=TRUE, spike=TRUE, rate=TRUE, highflow=TRUE,
           spatial=!is.null(metadata), breakpoint=TRUE),
  lower=0, upper=Inf,
  duplicate.min.month=20L, duplicate.min.year=300L,
  duplicate.min.nonzero=1L,
  climatology.window=5L, climatology.z=6,
  climatology.min.samples=30L, log.offset=0.01,
  flatline.run=11L, flatline.tolerance=0, flag.zero=FALSE,
  spike.factor=5, spike.prob=0.99,
  rate.z=6, rate.min.samples=30L,
  rate.max.rise=Inf, rate.max.fall=Inf, rate.drop.factor=5,
  highflow.log.z=6, highflow.amax.ratio=2,
  highflow.return.period=1000, highflow.min.years=10L,
  spatial.window.days=3L, spatial.high.prob=0.99,
  spatial.support.prob=0.95, spatial.f=3,
  n.neighbours=5L, max.distance=400, min.neighbours=2L,
  min.support=1L, min.overlap=90L, min.correlation=0.5,
  breakpoint.min.years=10L, breakpoint.alpha=0.01,
  breakpoint.min.relative.change=0.5,
  breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=3L, max.missing=0.2, max.review=0.2,
  max.suspicious=0.05, min.years=1, discard.breakpoint=FALSE,
  area=NULL, elevation=NULL, elevation.scale=500, verbose=TRUE,
  ncores=1L, parallel.type=c("socket", "fork", "serial"),
  cluster.nodes=NULL, from=start(x), to=end(x)) {

  # Validate controls before emitting any informational output.
  verbose <- .precipQC_validate_logical(verbose, "verbose")
  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a zoo object !")
  x <- check_from_and_to_and_subset(x, from=from, to=to)
  defaults <- c(range=TRUE, duplicate=TRUE, gap=TRUE, climatology=TRUE,
                flatline=TRUE, spike=TRUE, rate=TRUE, highflow=TRUE,
                spatial=!is.null(metadata), breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "daily", elevation=elevation
  )
  backend <- .QC_parallel_setup(ncores, parallel.type, cluster.nodes,
                                NCOL(prepared$values))
  on.exit(.QC_parallel_stop(backend), add=TRUE)
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  highflow <- spatial.estimate <- spatial.score <- spatial.support <-
    neighbours <- NULL

  # Announce each enabled test immediately before its computation.
  .streamflowQC_progress(verbose, "Starting daily QC for ",
                         NROW(prepared$values), " observations at ",
                         NCOL(prepared$values), " station(s) ...")
  .streamflowQC_progress(verbose, .QC_parallel_description(backend))
  if (checks["range"]) {
    .streamflowQC_progress(verbose, "Running physical-range check ...")
    flags$range <- streamflowQC_range(prepared$x, lower=lower, upper=upper)
  }
  if (checks["duplicate"])
    .streamflowQC_progress(verbose, "Running copied-block check ...")
  if (checks["gap"])
    .streamflowQC_progress(verbose, "Running gap check ...")
  if (checks["climatology"])
    .streamflowQC_progress(verbose,
                           "Running climatological-plausibility check ...")
  if (checks["flatline"])
    .streamflowQC_progress(verbose, "Running flat-line check ...")
  if (checks["spike"])
    .streamflowQC_progress(verbose, "Running isolated-spike check ...")
  if (checks["rate"])
    .streamflowQC_progress(verbose, "Running rate-of-change check ...")
  if (checks["highflow"])
    .streamflowQC_progress(verbose, "Running high-flow checks ...")
  if (checks["breakpoint"])
    .streamflowQC_progress(verbose,
                           "Running homogeneity-breakpoint check ...")

  # Distribute all station-local checks in balanced column chunks.
  parameters <- list(
    nstations=NCOL(prepared$values),
    duplicate.min.month=duplicate.min.month,
    duplicate.min.year=duplicate.min.year,
    duplicate.min.nonzero=duplicate.min.nonzero,
    climatology.window=climatology.window,
    climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    log.offset=log.offset, flatline.run=flatline.run,
    flatline.tolerance=flatline.tolerance, flag.zero=flag.zero,
    spike.factor=spike.factor, spike.prob=spike.prob,
    rate.z=rate.z, rate.min.samples=rate.min.samples,
    rate.max.rise=rate.max.rise, rate.max.fall=rate.max.fall,
    rate.drop.factor=rate.drop.factor,
    highflow.log.z=highflow.log.z,
    highflow.amax.ratio=highflow.amax.ratio,
    highflow.return.period=highflow.return.period,
    highflow.min.years=highflow.min.years,
    breakpoint.min.years=breakpoint.min.years,
    breakpoint.alpha=breakpoint.alpha,
    breakpoint.min.relative.change=breakpoint.min.relative.change,
    breakpoint.min.completeness=breakpoint.min.completeness
  )
  parts <- .QC_parallel_parts(prepared$x, backend)
  local <- .QC_parallel_apply(
    backend, parts, ".streamflowQC_daily_station_part", checks=checks,
    parameters=parameters
  )
  local <- .streamflowQC_combine_station_results(
    local, prepared$x, checks["highflow"]
  )
  flags <- c(flags, local$flags)
  breakpoint <- local$breakpoint
  highflow <- local$highflow

  # Spatial corroboration is meaningful only with a target and two alternatives.
  if (checks["spatial"]) {
    if (NCOL(prepared$values) < 3L) {
      warning("Spatial checks require at least three station columns; the spatial check was skipped.")
    } else {
      .streamflowQC_progress(verbose,
                             "Running spatial-corroboration check ...")
      spatial <- .streamflowQC_spatial_core(
        prepared$x, metadata=metadata, station.id=station.id, coords=coords,
        area=area, elevation=elevation, elevation.scale=elevation.scale,
        n.neighbours=n.neighbours, max.distance=max.distance,
        min.neighbours=min.neighbours, min.support=min.support,
        min.overlap=min.overlap, min.correlation=min.correlation,
        window=spatial.window.days, high.prob=spatial.high.prob,
        support.prob=spatial.support.prob, f=spatial.f, backend=backend
      )
      flags$spatial <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$scores
      spatial.support <- spatial$support.count
      neighbours <- spatial$neighbours
    }
  }

  # Record every resolved decision parameter for reproducibility.
  settings <- list(
    resolution="daily", checks=checks, lower=lower, upper=upper,
    duplicate.min.month=duplicate.min.month,
    duplicate.min.year=duplicate.min.year,
    duplicate.min.nonzero=duplicate.min.nonzero,
    climatology.window=climatology.window, climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    flatline.run=flatline.run, flatline.tolerance=flatline.tolerance,
    spike.factor=spike.factor, spike.prob=spike.prob,
    rate.z=rate.z, rate.min.samples=rate.min.samples,
    highflow.return.period=highflow.return.period,
    highflow.min.years=highflow.min.years,
    spatial.window.days=spatial.window.days, spatial.f=spatial.f,
    station.id=prepared$station.id, coords=coords, area=area,
    elevation=elevation, elevation.scale=elevation.scale,
    correction=match.arg(correction), min.evidence=min.evidence,
    max.missing=max.missing, max.review=max.review,
    max.suspicious=max.suspicious, min.years=min.years,
    discard.breakpoint=discard.breakpoint, verbose=verbose,
    ncores=backend$nworkers, parallel.type=backend$type,
    cluster.nodes=backend$nodes, from=start(prepared$x), to=end(prepared$x)
  )

  # Combine evidence only after all enabled checks have completed.
  .streamflowQC_progress(verbose,
                         "Combining evidence and station recommendations ...")
  out <- .streamflowQC_finish(
    prepared, flags, breakpoint, highflow, correction,
    max.missing, max.review, max.suspicious, min.years, min.evidence,
    hard.tests=c("range", "duplicate"), discard.breakpoint,
    spatial.estimate, spatial.score, spatial.support, neighbours,
    "daily", settings
  )

  # Close with a concise summary useful in interactive and batch logs.
  .streamflowQC_progress(
    verbose, "Completed daily QC: ",
    sum(out$station.summary$recommendation == "accept"), " accepted, ",
    sum(out$station.summary$recommendation == "discard"), " discarded; ",
    NROW(out$suspicious), " flagged observation(s), ",
    sum(out$suspicious$action == "reject"), " confirmed rejection(s)."
  )
  out

} # 'streamflowQC_daily' END


streamflowQC_subdaily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, gap=TRUE, climatology=TRUE, flatline=TRUE,
           spike=TRUE, rate=TRUE, fluctuation=TRUE, highflow=TRUE,
           spatial=!is.null(metadata), breakpoint=TRUE),
  lower=0, upper=Inf,
  climatology.z=6, climatology.min.samples=100L, log.offset=0.01,
  flatline.hours=168, high.flatline.hours=24,
  flatline.high.prob=0.99, flatline.tolerance=0, flag.zero=FALSE,
  spike.factor=5, spike.prob=0.99,
  rate.z=6, rate.min.samples=100L,
  rate.max.rise=Inf, rate.max.fall=Inf, rate.drop.factor=5,
  fluctuation.window=16L, fluctuation.minimum=8L,
  fluctuation.low.diff.prob=0.05,
  highflow.log.z=6, highflow.amax.ratio=2,
  highflow.return.period=1000, highflow.min.years=10L,
  spatial.window.hours=72, spatial.high.prob=0.99,
  spatial.support.prob=0.95, spatial.f=3,
  n.neighbours=5L, max.distance=200, min.neighbours=2L,
  min.support=1L, min.overlap=100L, min.correlation=0.5,
  breakpoint.min.years=10L, breakpoint.alpha=0.01,
  breakpoint.min.relative.change=0.5,
  breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=3L, max.missing=0.2, max.review=0.2,
  max.suspicious=0.05, min.years=1, discard.breakpoint=FALSE,
  area=NULL, elevation=NULL, elevation.scale=500, verbose=TRUE,
  ncores=1L, parallel.type=c("socket", "fork", "serial"),
  cluster.nodes=NULL, from=start(x), to=end(x)) {

  # Validate controls before emitting any informational output.
  verbose <- .precipQC_validate_logical(verbose, "verbose")
  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a zoo object !")
  x <- check_from_and_to_and_subset(x, from=from, to=to)
  defaults <- c(range=TRUE, gap=TRUE, climatology=TRUE, flatline=TRUE,
                spike=TRUE, rate=TRUE, fluctuation=TRUE, highflow=TRUE,
                spatial=!is.null(metadata), breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "subdaily", elevation=elevation
  )
  backend <- .QC_parallel_setup(ncores, parallel.type, cluster.nodes,
                                NCOL(prepared$values))
  on.exit(.QC_parallel_stop(backend), add=TRUE)
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  highflow <- spatial.estimate <- spatial.score <- spatial.support <-
    neighbours <- NULL

  # Convert paper durations to the actual regular interval of the input.
  for (name in c("flatline.hours", "high.flatline.hours",
                 "spatial.window.hours")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value <= 0)
      stop("Invalid argument: '", name, "' must be a positive finite number !")
  }
  flatline.run <- max(2L, ceiling(flatline.hours /
                                  prepared$interval.hours))
  high.flatline.run <- max(2L, ceiling(high.flatline.hours /
                                       prepared$interval.hours))
  spatial.window <- max(1L, ceiling(spatial.window.hours /
                                    prepared$interval.hours))

  # Announce each enabled test immediately before its computation.
  .streamflowQC_progress(verbose, "Starting sub-daily QC for ",
                         NROW(prepared$values), " observations at ",
                         NCOL(prepared$values), " station(s) ...")
  .streamflowQC_progress(verbose, .QC_parallel_description(backend))
  if (checks["range"]) {
    .streamflowQC_progress(verbose, "Running physical-range check ...")
    flags$range <- streamflowQC_range(prepared$x, lower=lower, upper=upper)
  }
  if (checks["gap"])
    .streamflowQC_progress(verbose, "Running gap check ...")
  if (checks["climatology"])
    .streamflowQC_progress(verbose,
                           "Running climatological-plausibility check ...")
  if (checks["flatline"])
    .streamflowQC_progress(verbose, "Running flat-line check ...")
  if (checks["spike"])
    .streamflowQC_progress(verbose, "Running isolated-spike check ...")
  if (checks["rate"])
    .streamflowQC_progress(verbose, "Running rate-of-change check ...")
  if (checks["fluctuation"])
    .streamflowQC_progress(verbose, "Running rapid-fluctuation check ...")
  if (checks["highflow"])
    .streamflowQC_progress(verbose, "Running high-flow checks ...")
  if (checks["breakpoint"])
    .streamflowQC_progress(verbose,
                           "Running homogeneity-breakpoint check ...")

  # Distribute all station-local checks in balanced column chunks.
  parameters <- list(
    nstations=NCOL(prepared$values),
    interval.seconds=prepared$interval.hours * 3600,
    climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    log.offset=log.offset, flatline.run=flatline.run,
    high.flatline.run=high.flatline.run,
    flatline.high.prob=flatline.high.prob,
    flatline.tolerance=flatline.tolerance, flag.zero=flag.zero,
    spike.factor=spike.factor, spike.prob=spike.prob,
    rate.z=rate.z, rate.min.samples=rate.min.samples,
    rate.max.rise=rate.max.rise, rate.max.fall=rate.max.fall,
    rate.drop.factor=rate.drop.factor,
    fluctuation.window=fluctuation.window,
    fluctuation.minimum=fluctuation.minimum,
    fluctuation.low.diff.prob=fluctuation.low.diff.prob,
    highflow.log.z=highflow.log.z,
    highflow.amax.ratio=highflow.amax.ratio,
    highflow.return.period=highflow.return.period,
    highflow.min.years=highflow.min.years,
    breakpoint.min.years=breakpoint.min.years,
    breakpoint.alpha=breakpoint.alpha,
    breakpoint.min.relative.change=breakpoint.min.relative.change,
    breakpoint.min.completeness=breakpoint.min.completeness
  )
  parts <- .QC_parallel_parts(prepared$x, backend)
  local <- .QC_parallel_apply(
    backend, parts, ".streamflowQC_subdaily_station_part", checks=checks,
    parameters=parameters
  )
  local <- .streamflowQC_combine_station_results(
    local, prepared$x, checks["highflow"]
  )
  flags <- c(flags, local$flags)
  breakpoint <- local$breakpoint
  highflow <- local$highflow

  # Neighbour-event support uses a three-day default expressed in input steps.
  if (checks["spatial"]) {
    if (NCOL(prepared$values) < 3L) {
      warning("Spatial checks require at least three station columns; the spatial check was skipped.")
    } else {
      .streamflowQC_progress(verbose,
                             "Running spatial-corroboration check ...")
      spatial <- .streamflowQC_spatial_core(
        prepared$x, metadata=metadata, station.id=station.id, coords=coords,
        area=area, elevation=elevation, elevation.scale=elevation.scale,
        n.neighbours=n.neighbours, max.distance=max.distance,
        min.neighbours=min.neighbours, min.support=min.support,
        min.overlap=min.overlap, min.correlation=min.correlation,
        window=spatial.window, high.prob=spatial.high.prob,
        support.prob=spatial.support.prob, f=spatial.f, backend=backend
      )
      flags$spatial <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$scores
      spatial.support <- spatial$support.count
      neighbours <- spatial$neighbours
    }
  }

  # Store resolved durations and thresholds so every result is auditable.
  settings <- list(
    resolution="subdaily", checks=checks, lower=lower, upper=upper,
    interval.hours=prepared$interval.hours,
    climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    flatline.hours=flatline.hours, flatline.run=flatline.run,
    high.flatline.hours=high.flatline.hours,
    high.flatline.run=high.flatline.run,
    spike.factor=spike.factor, spike.prob=spike.prob,
    rate.z=rate.z, rate.min.samples=rate.min.samples,
    fluctuation.window=fluctuation.window,
    fluctuation.minimum=fluctuation.minimum,
    highflow.return.period=highflow.return.period,
    highflow.min.years=highflow.min.years,
    spatial.window.hours=spatial.window.hours,
    spatial.window=spatial.window, spatial.f=spatial.f,
    station.id=prepared$station.id, coords=coords, area=area,
    elevation=elevation, elevation.scale=elevation.scale,
    correction=match.arg(correction), min.evidence=min.evidence,
    max.missing=max.missing, max.review=max.review,
    max.suspicious=max.suspicious, min.years=min.years,
    discard.breakpoint=discard.breakpoint, verbose=verbose,
    ncores=backend$nworkers, parallel.type=backend$type,
    cluster.nodes=backend$nodes, from=start(prepared$x), to=end(prepared$x)
  )

  # Combine evidence only after all enabled checks have completed.
  .streamflowQC_progress(verbose,
                         "Combining evidence and station recommendations ...")
  out <- .streamflowQC_finish(
    prepared, flags, breakpoint, highflow, correction,
    max.missing, max.review, max.suspicious, min.years, min.evidence,
    hard.tests="range", discard.breakpoint,
    spatial.estimate, spatial.score, spatial.support, neighbours,
    "subdaily", settings
  )

  # Close with a concise summary useful in interactive and batch logs.
  .streamflowQC_progress(
    verbose, "Completed sub-daily QC: ",
    sum(out$station.summary$recommendation == "accept"), " accepted, ",
    sum(out$station.summary$recommendation == "discard"), " discarded; ",
    NROW(out$suspicious), " flagged observation(s), ",
    sum(out$suspicious$action == "reject"), " confirmed rejection(s)."
  )
  out

} # 'streamflowQC_subdaily' END


streamflowQC <- function(x, metadata=NULL, station.id="station",
                         coords=c("lon", "lat"), ...,
                         area=NULL, elevation=NULL,
                         elevation.scale=500, verbose=TRUE, ncores=1L,
                         parallel.type=c("socket", "fork", "serial"),
                         cluster.nodes=NULL, from=start(x), to=end(x)) {

  # Validate verbosity at the dispatch boundary for consistent errors.
  verbose <- .precipQC_validate_logical(verbose, "verbose")
  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a zoo object !")
  x <- check_from_and_to_and_subset(x, from=from, to=to)
  frequency <- sfreq(x)
  if (identical(frequency, "daily")) {
    .streamflowQC_progress(
      verbose, "Detected daily data; dispatching to streamflowQC_daily() ..."
    )
    return(streamflowQC_daily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      area=area, elevation=elevation, elevation.scale=elevation.scale,
      verbose=verbose, ncores=ncores, parallel.type=parallel.type,
      cluster.nodes=cluster.nodes
    ))
  }
  if (frequency %in% c("minute", "hourly")) {
    .streamflowQC_progress(
      verbose, "Detected ", frequency,
      " data; dispatching to streamflowQC_subdaily() ..."
    )
    return(streamflowQC_subdaily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      area=area, elevation=elevation, elevation.scale=elevation.scale,
      verbose=verbose, ncores=ncores, parallel.type=parallel.type,
      cluster.nodes=cluster.nodes
    ))
  }

  stop("Invalid sampling frequency: sfreq(x) returned '", frequency,
       "'; only minute, hourly, and daily streamflow are supported !")

} # 'streamflowQC' END


print.streamflowQC <- function(x, ...) {

  # Print only the main auditable outcome counts.
  if (!inherits(x, "streamflowQC"))
    stop("Invalid argument: 'x' must inherit from class 'streamflowQC' !")
  summary <- x$station.summary
  cat("Streamflow quality-control result\n")
  cat("  resolution :", x$settings$resolution, "\n")
  cat("  stations   :", NROW(summary), "\n")
  cat("  accepted   :", sum(summary$recommendation == "accept"), "\n")
  cat("  discarded  :", sum(summary$recommendation == "discard"), "\n")
  cat("  review data:", sum(summary$review.count), "\n")
  cat("  rejected   :", sum(summary$suspicious.count), "\n")
  invisible(x)

} # 'print.streamflowQC' END


plot.streamflowQC <- function(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...) {

  # Validate the result and preserve the caller's graphics state.
  if (!inherits(x, "streamflowQC"))
    stop("Invalid argument: 'x' must inherit from class 'streamflowQC' !")
  max.stations <- .precipQC_check_positive_integer(max.stations,
                                                    "max.stations")
  if (!is.character(col) || length(col) < 3L)
    stop("Invalid argument: 'col' must contain at least three colours !")
  old.par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  graphics::par(mfrow=c(2, 2), mar=c(4, 4, 3, 1))

  # Panel one summarizes station-level decisions.
  station.summary <- x$station.summary
  decision <- table(factor(station.summary$recommendation,
                           levels=c("accept", "discard")))
  graphics::barplot(decision, col=col[1:2], ylab="Stations",
                    main="Station recommendations")

  # Panel two compares missing, review, and rejected fractions.
  order.stations <- order(station.summary$review.percent,
                          station.summary$missing.percent,
                          decreasing=TRUE)
  order.stations <- utils::head(order.stations, max.stations)
  station.values <- rbind(
    missing=100 * station.summary$missing.percent[order.stations],
    review=100 * station.summary$review.percent[order.stations],
    rejected=100 * station.summary$suspicious.percent[order.stations]
  )
  graphics::barplot(station.values, beside=TRUE,
                    col=col[c(3, 1, 2)],
                    names.arg=station.summary$station[order.stations],
                    las=2, cex.names=0.7, ylab="Percent",
                    main="Most affected stations")
  graphics::legend("topright", legend=rownames(station.values),
                   fill=col[c(3, 1, 2)], bty="n", cex=0.8)

  # Panel three exposes which tests generated the evidence.
  flag.counts <- vapply(x$flags, function(z) {
    sum(.precipQC_matrix(z, allow.logical=TRUE))
  }, numeric(1))
  if (length(flag.counts) == 0L) {
    graphics::plot.new()
    graphics::title("Flags by test")
    graphics::text(0.5, 0.5, "No active point-level tests")
  } else {
    graphics::barplot(flag.counts, horiz=TRUE, las=1, col=col[3],
                      xlab="Flagged values", main="Flags by test")
  }

  # Panel four locates confirmed suspicious observations in time.
  rejected <- .precipQC_matrix(x$rejected, allow.logical=TRUE)
  graphics::plot(zoo::index(x$rejected), rowSums(rejected), type="h",
                 col=col[2], xlab="Time", ylab="Rejected values",
                 main="Confirmed suspicious data", ...)
  invisible(x)

} # 'plot.streamflowQC' END
