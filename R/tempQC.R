# File tempQC.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Daily and sub-daily air-temperature quality-control workflows                 #
################################################################################

.tempQC_run_spatial <- function(prepared, group, metadata, station.id,
                                coords, n.neighbours, max.distance,
                                min.neighbours, min.overlap,
                                min.group.overlap, min.correlation,
                                f, min.difference, elevation=NULL,
                                elevation.scale=500) {

  if (NCOL(prepared$values) < min.neighbours + 1L) {
    warning("Spatial checks require the target plus at least 'min.neighbours' station columns; the spatial check was skipped.")
    return(NULL)
  }
  tempQC_spatial(
    prepared$x, metadata=metadata, station.id=station.id, coords=coords,
    group=group, n.neighbours=n.neighbours,
    max.distance=max.distance, min.neighbours=min.neighbours,
    min.overlap=min.overlap, min.group.overlap=min.group.overlap,
    min.correlation=min.correlation, f=f,
    min.difference=min.difference, elevation=elevation,
    elevation.scale=elevation.scale
  )

} # '.tempQC_run_spatial' END


tempQC_daily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, climatology=TRUE,
           persistence=TRUE, step=TRUE, spike=TRUE,
           spatial=TRUE, breakpoint=TRUE),
  lower=-89.4, upper=57.7,
  duplicate.min.month=20L, duplicate.min.year=300L,
  climatology.window=15L, climatology.z=6,
  climatology.min.samples=100L,
  persistence.run=7L, persistence.tolerance=0,
  step.z=3, step.min.samples=20L,
  step.max.rise=Inf, step.max.fall=Inf,
  spike.threshold=25,
  n.neighbours=5L, max.distance=200, min.neighbours=2L,
  min.overlap=90L, min.group.overlap=20L,
  min.correlation=0.6, spatial.f=4, spatial.min.difference=8,
  breakpoint.min.years=10L, breakpoint.alpha=0.05,
  breakpoint.min.shift=1, breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500) {

  defaults <- c(range=TRUE, duplicate=TRUE, climatology=TRUE,
                persistence=TRUE, step=TRUE, spike=TRUE,
                spatial=TRUE, breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "daily", elevation=elevation
  )
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  spatial.estimate <- spatial.score <- neighbours <- NULL

  if (checks["range"])
    flags$range <- tempQC_range(prepared$x, lower=lower, upper=upper)
  if (checks["duplicate"])
    flags$duplicate <- tempQC_duplicate(
      prepared$x, min.month.values=duplicate.min.month,
      min.year.values=duplicate.min.year
    )
  if (checks["climatology"])
    flags$climatology <- tempQC_climatology(
      prepared$x, group="dayofyear", window=climatology.window,
      z=climatology.z, min.samples=climatology.min.samples
    )
  if (checks["persistence"])
    flags$persistence <- tempQC_persistence(
      prepared$x, run=persistence.run, tolerance=persistence.tolerance
    )
  if (checks["step"])
    flags$step <- tempQC_step(
      prepared$x, group="month", z=step.z,
      min.samples=step.min.samples, max.rise=step.max.rise,
      max.fall=step.max.fall
    )
  if (checks["spike"])
    flags$spike <- tempQC_spike(prepared$x, threshold=spike.threshold)
  if (checks["spatial"]) {
    spatial <- .tempQC_run_spatial(
      prepared, "month", prepared$metadata, prepared$station.id,
      coords, n.neighbours, max.distance, min.neighbours,
      min.overlap, min.group.overlap, min.correlation,
      spatial.f, spatial.min.difference, elevation, elevation.scale
    )
    if (!is.null(spatial)) {
      flags$spatial <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$scores
      neighbours <- spatial$neighbours
    }
  }
  if (checks["breakpoint"])
    breakpoint <- tempQC_breakpoint(
      prepared$x, min.years=breakpoint.min.years,
      alpha=breakpoint.alpha, min.shift=breakpoint.min.shift,
      min.completeness=breakpoint.min.completeness
    )

  settings <- list(
    resolution="daily", checks=checks, lower=lower, upper=upper,
    duplicate.min.month=duplicate.min.month,
    duplicate.min.year=duplicate.min.year,
    climatology.window=climatology.window, climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    persistence.run=persistence.run,
    persistence.tolerance=persistence.tolerance,
    step.z=step.z, step.min.samples=step.min.samples,
    spike.threshold=spike.threshold, spatial.f=spatial.f,
    spatial.min.difference=spatial.min.difference,
    station.id=prepared$station.id, coords=coords,
    elevation=elevation, elevation.scale=elevation.scale,
    correction=match.arg(correction), min.evidence=min.evidence,
    max.missing=max.missing, max.suspicious=max.suspicious,
    min.years=min.years
  )

  .precipQC_finish(
    prepared, flags, breakpoint, correction, max.missing, max.suspicious,
    min.years, min.evidence,
    hard.tests=c("range", "duplicate", "persistence", "spike"),
    discard.breakpoint, spatial.estimate, spatial.score, neighbours,
    "daily", settings, object.class="tempQC"
  )

} # 'tempQC_daily' END


tempQC_subdaily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, climatology=TRUE, persistence=TRUE,
           step=TRUE, spike=TRUE, spatial=TRUE, breakpoint=TRUE),
  lower=-89.4, upper=57.7,
  climatology.z=6, climatology.min.samples=30L,
  persistence.hours=3, persistence.tolerance=0.05,
  step.z=5, step.min.samples=30L,
  step.max.rise=Inf, step.max.fall=Inf,
  spike.threshold=12,
  n.neighbours=5L, max.distance=50, min.neighbours=2L,
  min.overlap=100L, min.group.overlap=20L,
  min.correlation=0.7, spatial.f=4, spatial.min.difference=5,
  breakpoint.min.years=10L, breakpoint.alpha=0.05,
  breakpoint.min.shift=1, breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500) {

  defaults <- c(range=TRUE, climatology=TRUE, persistence=TRUE,
                step=TRUE, spike=TRUE, spatial=TRUE, breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "subdaily", elevation=elevation
  )
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  spatial.estimate <- spatial.score <- neighbours <- NULL

  if (!is.numeric(persistence.hours) || length(persistence.hours) != 1L ||
      is.na(persistence.hours) || !is.finite(persistence.hours) ||
      persistence.hours <= 0)
    stop("Invalid argument: 'persistence.hours' must be a positive number !")
  persistence.run <- max(2L, ceiling(persistence.hours /
                                      prepared$interval.hours) + 1L)

  if (checks["range"])
    flags$range <- tempQC_range(prepared$x, lower=lower, upper=upper)
  if (checks["climatology"])
    flags$climatology <- tempQC_climatology(
      prepared$x, group="month_hour", window=1L,
      z=climatology.z, min.samples=climatology.min.samples
    )
  if (checks["persistence"])
    flags$persistence <- tempQC_persistence(
      prepared$x, run=persistence.run, tolerance=persistence.tolerance
    )
  if (checks["step"])
    flags$step <- tempQC_step(
      prepared$x, group="month_hour", z=step.z,
      min.samples=step.min.samples, max.rise=step.max.rise,
      max.fall=step.max.fall
    )
  if (checks["spike"])
    flags$spike <- tempQC_spike(prepared$x, threshold=spike.threshold)
  if (checks["spatial"]) {
    spatial <- .tempQC_run_spatial(
      prepared, "month_hour", prepared$metadata, prepared$station.id,
      coords, n.neighbours, max.distance, min.neighbours,
      min.overlap, min.group.overlap, min.correlation,
      spatial.f, spatial.min.difference, elevation, elevation.scale
    )
    if (!is.null(spatial)) {
      flags$spatial <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$scores
      neighbours <- spatial$neighbours
    }
  }
  if (checks["breakpoint"])
    breakpoint <- tempQC_breakpoint(
      prepared$x, min.years=breakpoint.min.years,
      alpha=breakpoint.alpha, min.shift=breakpoint.min.shift,
      min.completeness=breakpoint.min.completeness
    )

  settings <- list(
    resolution="subdaily", checks=checks, lower=lower, upper=upper,
    interval.hours=prepared$interval.hours,
    climatology.z=climatology.z,
    climatology.min.samples=climatology.min.samples,
    persistence.hours=persistence.hours,
    persistence.run=persistence.run,
    persistence.tolerance=persistence.tolerance,
    step.z=step.z, step.min.samples=step.min.samples,
    spike.threshold=spike.threshold, spatial.f=spatial.f,
    spatial.min.difference=spatial.min.difference,
    station.id=prepared$station.id, coords=coords,
    elevation=elevation, elevation.scale=elevation.scale,
    correction=match.arg(correction), min.evidence=min.evidence,
    max.missing=max.missing, max.suspicious=max.suspicious,
    min.years=min.years
  )

  .precipQC_finish(
    prepared, flags, breakpoint, correction, max.missing, max.suspicious,
    min.years, min.evidence,
    hard.tests=c("range", "persistence", "spike"),
    discard.breakpoint, spatial.estimate, spatial.score, neighbours,
    "subdaily", settings, object.class="tempQC"
  )

} # 'tempQC_subdaily' END


tempQC <- function(x, metadata=NULL, station.id="station",
                   coords=c("lon", "lat"), ...,
                   elevation=NULL, elevation.scale=500) {

  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a zoo object !")
  frequency <- sfreq(x)
  if (identical(frequency, "daily"))
    return(tempQC_daily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      elevation=elevation, elevation.scale=elevation.scale
    ))
  if (frequency %in% c("minute", "hourly"))
    return(tempQC_subdaily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      elevation=elevation, elevation.scale=elevation.scale
    ))
  stop("Invalid sampling frequency: sfreq(x) returned '", frequency,
       "'; only minute, hourly, and daily air temperature are supported !")

} # 'tempQC' END


print.tempQC <- function(x, ...) {

  if (!inherits(x, "tempQC"))
    stop("Invalid argument: 'x' must inherit from class 'tempQC' !")
  summary <- x$station.summary
  cat("Air-temperature quality-control result\n")
  cat("  resolution :", x$settings$resolution, "\n")
  cat("  stations   :", NROW(summary), "\n")
  cat("  accepted   :", sum(summary$recommendation == "accept"), "\n")
  cat("  discarded  :", sum(summary$recommendation == "discard"), "\n")
  cat("  review data:", sum(summary$review.count), "\n")
  cat("  rejected   :", sum(summary$suspicious.count), "\n")
  invisible(x)

} # 'print.tempQC' END


plot.tempQC <- function(x, max.stations=20L,
                        col=c("#2878B5", "#D55E00", "#E69F00"), ...) {

  if (!inherits(x, "tempQC"))
    stop("Invalid argument: 'x' must inherit from class 'tempQC' !")
  max.stations <- .precipQC_check_positive_integer(max.stations,
                                                    "max.stations")
  if (!is.character(col) || length(col) < 3L)
    stop("Invalid argument: 'col' must contain at least three colours !")
  station.summary <- x$station.summary
  old.par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  graphics::par(mfrow=c(2, 2), mar=c(4, 4, 3, 1))

  decision <- table(factor(station.summary$recommendation,
                           levels=c("accept", "discard")))
  graphics::barplot(decision, col=col[1:2], ylab="Stations",
                    main="Station recommendations")
  order.stations <- order(station.summary$suspicious.percent,
                          station.summary$missing.percent,
                          decreasing=TRUE)
  order.stations <- utils::head(order.stations, max.stations)
  station.values <- rbind(
    missing=100 * station.summary$missing.percent[order.stations],
    suspicious=100 * station.summary$suspicious.percent[order.stations]
  )
  graphics::barplot(station.values, beside=TRUE, col=col[c(3, 2)],
                    names.arg=station.summary$station[order.stations],
                    las=2, cex.names=0.7, ylab="Percent",
                    main="Most affected stations")
  graphics::legend("topright", legend=rownames(station.values),
                   fill=col[c(3, 2)], bty="n", cex=0.8)

  flag.counts <- vapply(x$flags, function(z)
    sum(.precipQC_matrix(z, allow.logical=TRUE)), numeric(1))
  if (length(flag.counts) == 0L) {
    graphics::plot.new()
    graphics::title("Flags by test")
    graphics::text(0.5, 0.5, "No active point-level tests")
  } else {
    graphics::barplot(flag.counts, horiz=TRUE, las=1, col=col[3],
                      xlab="Flagged values", main="Flags by test")
  }
  rejected <- .precipQC_matrix(x$rejected, allow.logical=TRUE)
  graphics::plot(zoo::index(x$rejected), rowSums(rejected), type="h",
                 col=col[2], xlab="Time", ylab="Rejected values",
                 main="Confirmed suspicious data", ...)
  invisible(x)

} # 'plot.tempQC' END
