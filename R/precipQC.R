# File precipQC.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Daily and sub-daily precipitation quality-control workflows                  #
################################################################################

.precipQC_validate_logical <- function(x, name) {

  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop("Invalid argument: '", name, "' must be a logical value !")

  x

} # '.precipQC_validate_logical' END


.precipQC_checks <- function(checks, defaults) {

  if (!is.logical(checks) || is.null(names(checks)) || anyNA(checks) ||
      any(!nzchar(names(checks))) || anyDuplicated(names(checks)))
    stop("Invalid argument: 'checks' must be a named logical vector !")

  unknown <- setdiff(names(checks), names(defaults))
  if (length(unknown) > 0L)
    stop("Invalid argument: unknown checks: ", paste(unknown, collapse=", "),
         " !")

  defaults[names(checks)] <- checks
  defaults

} # '.precipQC_checks' END


.precipQC_validate_common <- function(x, metadata, station.id, coords,
                                      resolution, elevation=NULL) {

  values <- .precipQC_matrix(x)
  datetimes <- zoo::index(x)

  if (NROW(values) < 2L)
    stop("Invalid argument: 'x' must contain at least two time steps !")
  if (length(datetimes) != NROW(values))
    stop("Invalid argument: the time index and values in 'x' must have the same length !")

  if (resolution == "subdaily") {
    if (!inherits(datetimes, "POSIXt"))
      stop("Invalid argument: sub-daily 'x' must have a POSIXt time index !")
    delta <- as.numeric(diff(as.POSIXct(datetimes)), units="secs")
    if (any(!is.finite(delta)) || any(delta <= 0))
      stop("Invalid argument: 'time(x)' must be strictly increasing and must not contain duplicated values !")

    rounded <- as.character(round(delta))
    interval.seconds <- as.numeric(names(sort(table(rounded),
                                               decreasing=TRUE))[1L])
    if (!is.finite(interval.seconds) || interval.seconds >= 86400)
      stop("Invalid argument: 'x' must have a sub-daily temporal resolution !")

    ratio <- delta / interval.seconds
    if (any(abs(ratio - round(ratio)) > 1e-6))
      stop("Invalid argument: all time steps in 'x' must be integer multiples of the modal temporal resolution !")
    expected <- floor(as.numeric(difftime(max(datetimes), min(datetimes),
                                          units="secs")) /
                      interval.seconds) + 1L
    interval.hours <- interval.seconds / 3600
  } else {
    if (!(inherits(datetimes, "Date") || inherits(datetimes, "POSIXt")))
      stop("Invalid argument: daily 'x' must have a Date or POSIXt time index !")
    date.index <- as.Date(datetimes)
    delta <- as.numeric(diff(date.index))
    if (any(!is.finite(delta)) || any(delta <= 0))
      stop("Invalid argument: 'time(x)' must be strictly increasing and must not contain duplicated days !")
    expected <- as.integer(max(date.index) - min(date.index)) + 1L
    interval.hours <- 24
  }

  stations <- colnames(values)
  if (anyNA(stations) || any(!nzchar(stations)) || anyDuplicated(stations))
    stop("Invalid argument: every column in 'x' must have a unique, non-empty station name !")

  meta <- .precipQC_metadata(stations, metadata, station.id, coords,
                             elevation)
  x <- zoo::zoo(values, datetimes)

  list(x=x, values=values, datetimes=datetimes, stations=stations,
       metadata=meta$metadata, station.id=meta$station.id,
       coords=meta$coords, elevation=meta$elevation,
       has.coords=meta$has.coords, has.elevation=meta$has.elevation,
       coordinate.available=meta$coordinate.available,
       expected=expected,
       interval.hours=interval.hours)

} # '.precipQC_validate_common' END


.precipQC_rollsum <- function(x, width) {

  width <- .precipQC_check_positive_integer(width, "width")
  if (width == 1L)
    return(x)

  zoo::rollapply(x, width=width, align="right", fill=NA_real_,
                 by.column=TRUE,
                 FUN=function(z) {
                   if (anyNA(z)) NA_real_ else sum(z)
                 })

} # '.precipQC_rollsum' END


.precipQC_expand_aggregate_flags <- function(flags, width, x) {

  aggregated <- .precipQC_matrix(flags, allow.logical=TRUE)
  expanded <- matrix(FALSE, nrow=NROW(aggregated), ncol=NCOL(aggregated),
                     dimnames=dimnames(aggregated))
  flagged <- which(aggregated, arr.ind=TRUE)

  if (NROW(flagged) > 0L) {
    for (k in seq_len(NROW(flagged))) {
      row <- flagged[k, "row"]
      column <- flagged[k, "col"]
      expanded[max(1L, row - width + 1L):row, column] <- TRUE
    }
  }

  .precipQC_zoo(expanded, x)

} # '.precipQC_expand_aggregate_flags' END


.precipQC_weekday_subdaily <- function(x, interval.hours, min.coverage,
                                       wet.threshold, min.wet.days,
                                       alpha, underreporting.ratio) {

  if (!is.numeric(min.coverage) || length(min.coverage) != 1L ||
      is.na(min.coverage) || !is.finite(min.coverage) ||
      min.coverage <= 0 || min.coverage > 1)
    stop("Invalid argument: 'weekday.min.coverage' must be in (0, 1] !")

  values <- .precipQC_matrix(x)
  dates <- as.Date(zoo::index(x))
  all.dates <- seq(min(dates), max(dates), by="day")
  day.index <- match(dates, all.dates)
  expected <- max(1L, as.integer(round(24 / interval.hours)))
  daily.values <- matrix(NA_real_, nrow=length(all.dates),
                         ncol=NCOL(values),
                         dimnames=list(NULL, colnames(values)))

  for (j in seq_len(NCOL(values))) {
    daily.values[, j] <- vapply(seq_along(all.dates), function(i) {
      z <- values[day.index == i, j]
      observed <- sum(is.finite(z))
      if (observed / expected < min.coverage) NA_real_ else
        sum(z[is.finite(z)])
    }, numeric(1))
  }

  daily <- zoo::zoo(daily.values, all.dates)
  daily.flags <- .precipQC_matrix(precipQC_weekday(
    daily, wet.threshold=wet.threshold, min.wet.days=min.wet.days,
    alpha=alpha, underreporting.ratio=underreporting.ratio
  ), allow.logical=TRUE)

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values)))
    flags[, j] <- daily.flags[day.index, j] &
                  is.finite(values[, j]) & values[, j] < wet.threshold

  .precipQC_zoo(flags, x)

} # '.precipQC_weekday_subdaily' END


.precipQC_add_spatial <- function(flags, x, widths, labels, metadata,
                                  station.id, coords, n.neighbours,
                                  max.distance, min.neighbours,
                                  min.overlap, min.correlation,
                                  cr.threshold, wet.threshold,
                                  elevation=NULL, elevation.scale=500) {

  native.estimate <- NULL
  native.score <- NULL
  neighbours <- NULL

  for (i in seq_along(widths)) {
    width <- widths[i]
    aggregated <- .precipQC_rollsum(x, width)
    result <- precipQC_spatial(
      aggregated, metadata=metadata, station.id=station.id, coords=coords,
      n.neighbours=n.neighbours, max.distance=max.distance,
      min.neighbours=min.neighbours, min.overlap=min.overlap,
      min.correlation=min.correlation, cr.threshold=cr.threshold,
      wet.threshold=wet.threshold, elevation=elevation,
      elevation.scale=elevation.scale
    )
    flags[[labels[i]]] <- .precipQC_expand_aggregate_flags(result$flags,
                                                            width, x)
    if (width == 1L) {
      native.estimate <- result$estimate
      native.score <- result$scores
      neighbours <- result$neighbours
    }
  }

  list(flags=flags, estimate=native.estimate, score=native.score,
       neighbours=neighbours)

} # '.precipQC_add_spatial' END


.precipQC_empty_breakpoint <- function(stations) {

  data.frame(station=stations, breakpoint.year=NA_integer_,
             indicator=NA_character_, p.value=NA_real_,
             relative.change=NA_real_, n.indicators=0L, flagged=FALSE,
             stringsAsFactors=FALSE)

} # '.precipQC_empty_breakpoint' END


.precipQC_finish <- function(prepared, flags, breakpoint,
                             correction, max.missing, max.suspicious,
                             min.years, min.evidence, hard.tests,
                             discard.breakpoint, spatial.estimate,
                             spatial.score, neighbours, resolution,
                             settings, object.class="precipQC") {

  correction <- match.arg(correction, c("none", "set_na", "spatial"))
  min.evidence <- .precipQC_check_positive_integer(min.evidence,
                                                    "min.evidence")
  discard.breakpoint <- .precipQC_validate_logical(discard.breakpoint,
                                                    "discard.breakpoint")

  for (arg in c("max.missing", "max.suspicious")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value > 1)
      stop("Invalid argument: '", arg, "' must be in [0, 1] !")
  }
  if (!is.numeric(min.years) || length(min.years) != 1L ||
      is.na(min.years) || !is.finite(min.years) || min.years < 0)
    stop("Invalid argument: 'min.years' must be a non-negative number !")

  values <- prepared$values
  flag.matrices <- lapply(flags, .precipQC_matrix, allow.logical=TRUE)
  ntests <- if (length(flag.matrices) == 0L) {
    matrix(0L, nrow=NROW(values), ncol=NCOL(values),
           dimnames=dimnames(values))
  } else {
    Reduce(`+`, lapply(flag.matrices, function(z) z * 1L))
  }
  any.flag <- ntests > 0L

  hard <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                 dimnames=dimnames(values))
  active.hard <- intersect(hard.tests, names(flag.matrices))
  if (length(active.hard) > 0L)
    hard <- Reduce(`|`, flag.matrices[active.hard])

  confirmed <- hard | ntests >= min.evidence
  corrected <- values
  correction.method <- matrix("none", nrow=NROW(values), ncol=NCOL(values),
                              dimnames=dimnames(values))

  spatial.values <- if (is.null(spatial.estimate)) {
    matrix(NA_real_, nrow=NROW(values), ncol=NCOL(values),
           dimnames=dimnames(values))
  } else {
    .precipQC_matrix(spatial.estimate)
  }

  if (correction == "set_na") {
    corrected[confirmed] <- NA_real_
    correction.method[confirmed] <- "set_na"
  } else if (correction == "spatial") {
    replace.spatial <- confirmed & is.finite(spatial.values)
    corrected[replace.spatial] <- spatial.values[replace.spatial]
    correction.method[replace.spatial] <- "spatial"
    replace.na <- confirmed & !replace.spatial
    corrected[replace.na] <- NA_real_
    correction.method[replace.na] <- "set_na"
  }

  observed <- colSums(is.finite(values))
  missing.percent <- pmax(0, 1 - observed / prepared$expected)
  review.count <- colSums(any.flag)
  suspicious.count <- colSums(confirmed)
  denominator <- pmax(1, observed)
  review.percent <- review.count / denominator
  suspicious.percent <- suspicious.count / denominator
  record.years <- if (resolution == "daily") {
    prepared$expected / 365.25
  } else {
    prepared$expected * prepared$interval.hours / (24 * 365.25)
  }

  bp <- breakpoint[match(prepared$stations, breakpoint$station),,
                   drop=FALSE]
  accept <- missing.percent <= max.missing &
            suspicious.percent <= max.suspicious &
            record.years >= min.years
  if (discard.breakpoint)
    accept <- accept & !bp$flagged

  reasons <- character(length(accept))
  for (j in seq_along(accept)) {
    why <- character()
    if (missing.percent[j] > max.missing)
      why <- c(why, "missing fraction exceeds limit")
    if (suspicious.percent[j] > max.suspicious)
      why <- c(why, "suspicious fraction exceeds limit")
    if (record.years < min.years)
      why <- c(why, "record is shorter than required")
    if (discard.breakpoint && isTRUE(bp$flagged[j]))
      why <- c(why, "large statistically significant breakpoint")
    reasons[j] <- if (length(why) == 0L) "within acceptance limits" else
                  paste(why, collapse="; ")
  }

  station.summary <- data.frame(
    station=prepared$stations,
    expected=rep(prepared$expected, length(prepared$stations)),
    observed=observed,
    missing.percent=missing.percent,
    review.count=review.count,
    review.percent=review.percent,
    suspicious.count=suspicious.count,
    suspicious.percent=suspicious.percent,
    breakpoint.year=bp$breakpoint.year,
    breakpoint.indicator=bp$indicator,
    breakpoint.p.value=bp$p.value,
    breakpoint.relative.change=bp$relative.change,
    breakpoint.n.indicators=bp$n.indicators,
    breakpoint.flag=bp$flagged,
    record.years=rep(record.years, length(prepared$stations)),
    recommendation=ifelse(accept, "accept", "discard"),
    reason=reasons,
    stringsAsFactors=FALSE
  )

  metadata <- prepared$metadata
  diagnostics <- station.summary[, setdiff(names(station.summary), "station"),
                                 drop=FALSE]
  metadata <- cbind(metadata, diagnostics)
  accepted.metadata <- metadata[accept, , drop=FALSE]
  discarded.metadata <- metadata[!accept, , drop=FALSE]

  flagged <- which(any.flag, arr.ind=TRUE)
  if (NROW(flagged) == 0L) {
    suspicious <- data.frame(
      time=prepared$datetimes[integer()], station=character(),
      original=numeric(), spatial.estimate=numeric(),
      spatial.score=numeric(), n.tests=integer(), tests=character(),
      action=character(), corrected=numeric(), correction=character(),
      stringsAsFactors=FALSE
    )
  } else {
    test.names <- vapply(seq_len(NROW(flagged)), function(k) {
      row <- flagged[k, "row"]
      column <- flagged[k, "col"]
      paste(names(flag.matrices)[vapply(flag.matrices,
        function(z) isTRUE(z[row, column]), logical(1))], collapse=", ")
    }, character(1))

    score.values <- if (is.null(spatial.score)) {
      matrix(NA_real_, nrow=NROW(values), ncol=NCOL(values))
    } else {
      .precipQC_matrix(spatial.score)
    }
    linear <- cbind(flagged[, "row"], flagged[, "col"])
    suspicious <- data.frame(
      time=prepared$datetimes[flagged[, "row"]],
      station=prepared$stations[flagged[, "col"]],
      original=values[linear],
      spatial.estimate=spatial.values[linear],
      spatial.score=score.values[linear],
      n.tests=ntests[linear],
      tests=test.names,
      action=ifelse(confirmed[linear], "reject", "review"),
      corrected=corrected[linear],
      correction=correction.method[linear],
      stringsAsFactors=FALSE
    )
  }

  changed <- which(correction.method != "none", arr.ind=TRUE)
  if (NROW(changed) == 0L) {
    corrections <- suspicious[FALSE,
      c("time", "station", "original", "corrected", "correction"),
      drop=FALSE]
  } else {
    linear <- cbind(changed[, "row"], changed[, "col"])
    corrections <- data.frame(
      time=prepared$datetimes[changed[, "row"]],
      station=prepared$stations[changed[, "col"]],
      original=values[linear], corrected=corrected[linear],
      correction=correction.method[linear], stringsAsFactors=FALSE
    )
  }

  accepted <- which(accept)
  accepted.data <- zoo::zoo(values[, accepted, drop=FALSE],
                            prepared$datetimes)
  accepted.corrected <- zoo::zoo(corrected[, accepted, drop=FALSE],
                                 prepared$datetimes)

  out <- list(
    accepted.metadata=accepted.metadata,
    discarded.metadata=discarded.metadata,
    accepted.data=accepted.data,
    accepted.corrected=accepted.corrected,
    suspicious=suspicious,
    corrections=corrections,
    flags=flags,
    flag.count=.precipQC_zoo(ntests, prepared$x),
    rejected=.precipQC_zoo(confirmed, prepared$x),
    station.summary=station.summary,
    breakpoint=breakpoint,
    spatial.estimate=spatial.estimate,
    spatial.score=spatial.score,
    neighbours=neighbours,
    settings=settings
  )
  class(out) <- c(object.class, "list")
  out

} # '.precipQC_finish' END


precipQC_daily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, frequency=TRUE, gap=TRUE,
           climatology=TRUE, persistence=TRUE, accumulation=TRUE,
           weekday=TRUE, spatial=TRUE, dryspell=TRUE, breakpoint=TRUE),
  lower=0, upper=1825, wet.threshold=0.1,
  duplicate.min.month=20L, duplicate.min.year=300L,
  duplicate.min.nonzero=3L,
  frequency.window=10L, frequency.min.samples=20L,
  gap.threshold=300, gap.min.samples=30L,
  climatology.window=15L, climatology.prob=0.999,
  climatology.z=8, climatology.min.samples=30L,
  persistence.threshold=10, persistence.run=5L,
  weekday.min.wet=20L, weekday.alpha=0.001,
  weekday.ratio=0.5,
  spatial.days=c(1L, 3L, 7L), spatial.cr=3,
  n.neighbours=10L, max.distance=400, min.neighbours=2L,
  min.overlap=30L, min.correlation=0,
  dryspell.days=15L, neighbour.wet.days=3L,
  neighbour.fraction=1,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500) {

  defaults <- c(range=TRUE, duplicate=TRUE, frequency=TRUE, gap=TRUE,
                climatology=TRUE, persistence=TRUE, accumulation=TRUE,
                weekday=TRUE, spatial=TRUE, dryspell=TRUE, breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "daily", elevation=elevation
  )
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  spatial.estimate <- spatial.score <- neighbours <- NULL

  if (checks["range"])
    flags$range <- precipQC_range(prepared$x, lower=lower, upper=upper)
  if (checks["duplicate"])
    flags$duplicate <- precipQC_duplicate(
      prepared$x, min.month.values=duplicate.min.month,
      min.year.values=duplicate.min.year,
      min.nonzero=duplicate.min.nonzero, wet.threshold=wet.threshold
    )
  if (checks["frequency"])
    flags$frequency <- precipQC_frequency(
      prepared$x, window=frequency.window,
      min.samples=frequency.min.samples, wet.threshold=wet.threshold
    )
  if (checks["gap"])
    flags$gap <- precipQC_gap(
      prepared$x, gap=gap.threshold, min.samples=gap.min.samples,
      wet.threshold=wet.threshold
    )
  if (checks["climatology"])
    flags$climatology <- precipQC_climatology(
      prepared$x, group="dayofyear", window=climatology.window,
      prob=climatology.prob, z=climatology.z,
      min.samples=climatology.min.samples, wet.threshold=wet.threshold
    )
  if (checks["persistence"])
    flags$persistence <- precipQC_persistence(
      prepared$x, high.threshold=persistence.threshold,
      high.run=persistence.run, long.run=Inf,
      wet.threshold=wet.threshold
    )
  if (checks["accumulation"])
    flags$accumulation <- precipQC_accumulation(
      prepared$x, interval.hours=24, preceding.hours=24,
      following.hours=0, threshold.factor=2,
      wet.threshold=wet.threshold, missing.only=TRUE
    )
  if (checks["weekday"])
    flags$weekday <- precipQC_weekday(
      prepared$x, wet.threshold=wet.threshold,
      min.wet.days=weekday.min.wet, alpha=weekday.alpha,
      underreporting.ratio=weekday.ratio
    )
  if (checks["spatial"]) {
    if (NCOL(prepared$values) < 3L) {
      warning("Spatial checks require at least three station columns; the spatial check was skipped.")
    } else {
      if (!is.numeric(spatial.days) || anyNA(spatial.days) ||
          any(spatial.days < 1) ||
          any(abs(spatial.days - round(spatial.days)) >
              sqrt(.Machine$double.eps)))
        stop("Invalid argument: 'spatial.days' must contain positive integers !")
      widths <- unique(as.integer(round(spatial.days)))
      labels <- paste0("spatial_", widths, "d")
      spatial <- .precipQC_add_spatial(
        flags, prepared$x, widths, labels, prepared$metadata,
        prepared$station.id, coords, n.neighbours, max.distance,
        min.neighbours, min.overlap, min.correlation, spatial.cr,
        wet.threshold, elevation, elevation.scale
      )
      flags <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$score
      neighbours <- spatial$neighbours
    }
  }
  if (checks["dryspell"] && NCOL(prepared$values) >= 2L)
    flags$dryspell <- precipQC_dryspell(
      prepared$x, metadata=prepared$metadata,
      station.id=prepared$station.id, coords=coords,
      n.neighbours=n.neighbours, max.distance=max.distance,
      window.days=dryspell.days, neighbour.wet.days=neighbour.wet.days,
      neighbour.fraction=neighbour.fraction,
      wet.threshold=wet.threshold, elevation=elevation,
      elevation.scale=elevation.scale
    )
  if (checks["breakpoint"])
    breakpoint <- precipQC_breakpoint(prepared$x,
      wet.threshold=wet.threshold)

  settings <- list(resolution="daily", checks=checks, lower=lower,
                   upper=upper, wet.threshold=wet.threshold,
                   duplicate.min.month=duplicate.min.month,
                   duplicate.min.year=duplicate.min.year,
                   duplicate.min.nonzero=duplicate.min.nonzero,
                   frequency.window=frequency.window,
                   frequency.min.samples=frequency.min.samples,
                   gap.threshold=gap.threshold,
                   gap.min.samples=gap.min.samples,
                   weekday.min.wet=weekday.min.wet,
                   weekday.alpha=weekday.alpha,
                   weekday.ratio=weekday.ratio,
                   station.id=prepared$station.id, coords=coords,
                   elevation=elevation, elevation.scale=elevation.scale,
                   spatial.days=spatial.days, correction=match.arg(correction),
                   min.evidence=min.evidence, max.missing=max.missing,
                   max.suspicious=max.suspicious, min.years=min.years)

  .precipQC_finish(
    prepared, flags, breakpoint, correction, max.missing, max.suspicious,
    min.years, min.evidence,
    hard.tests=c("range", "duplicate", "frequency", "gap",
                 "persistence", "accumulation", "dryspell"),
    discard.breakpoint, spatial.estimate, spatial.score, neighbours,
    "daily", settings
  )

} # 'precipQC_daily' END


precipQC_subdaily <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, climatology=TRUE, persistence=TRUE,
           daily.accumulation=TRUE, monthly.accumulation=TRUE,
           weekday=TRUE, spatial=TRUE, dryspell=TRUE, breakpoint=TRUE),
  lower=0, max.rate=401, wet.threshold=0.1,
  climatology.prob=0.999, climatology.z=8,
  climatology.min.samples=100L,
  persistence.threshold=NULL, persistence.high.run=2L,
  persistence.long.hours=24,
  accumulation.factor=2,
  weekday.min.wet=20L, weekday.alpha=0.001,
  weekday.ratio=0.5, weekday.min.coverage=0.9,
  spatial.hours=c(0, 1, 3, 6, 24), spatial.cr=3,
  n.neighbours=10L, max.distance=50, min.neighbours=2L,
  min.overlap=100L, min.correlation=0,
  dryspell.days=15L, neighbour.wet.days=3L,
  neighbour.fraction=1,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500) {

  defaults <- c(range=TRUE, climatology=TRUE, persistence=TRUE,
                daily.accumulation=TRUE, monthly.accumulation=TRUE,
                weekday=TRUE, spatial=TRUE, dryspell=TRUE, breakpoint=TRUE)
  checks <- .precipQC_checks(checks, defaults)
  prepared <- .precipQC_validate_common(
    x, metadata, station.id, coords, "subdaily", elevation=elevation
  )
  flags <- list()
  breakpoint <- .precipQC_empty_breakpoint(prepared$stations)
  spatial.estimate <- spatial.score <- neighbours <- NULL

  if (!is.numeric(max.rate) || length(max.rate) != 1L || is.na(max.rate) ||
      !is.finite(max.rate) || max.rate <= 0)
    stop("Invalid argument: 'max.rate' must be a positive number !")
  upper <- max.rate * prepared$interval.hours

  if (checks["range"])
    flags$range <- precipQC_range(prepared$x, lower=lower, upper=upper)
  if (checks["climatology"])
    flags$climatology <- precipQC_climatology(
      prepared$x, group="month", window=1L, prob=climatology.prob,
      z=climatology.z, min.samples=climatology.min.samples,
      wet.threshold=wet.threshold
    )
  if (checks["persistence"]) {
    if (is.null(persistence.threshold)) {
      persistence.threshold <- 2 * .precipQC_wet_day_mean(
        prepared$x, wet.threshold
      )
      persistence.threshold[!is.finite(persistence.threshold)] <- upper
    }
    long.run <- max(1L, ceiling(persistence.long.hours /
                                prepared$interval.hours))
    flags$persistence <- precipQC_persistence(
      prepared$x, high.threshold=persistence.threshold,
      high.run=persistence.high.run, long.run=long.run,
      wet.threshold=wet.threshold
    )
  }
  if (checks["daily.accumulation"])
    flags$daily_accumulation <- precipQC_accumulation(
      prepared$x, interval.hours=prepared$interval.hours,
      preceding.hours=23, following.hours=23,
      threshold.factor=accumulation.factor,
      wet.threshold=wet.threshold, missing.only=FALSE
    )
  if (checks["monthly.accumulation"])
    flags$monthly_accumulation <- precipQC_accumulation(
      prepared$x, interval.hours=prepared$interval.hours,
      preceding.hours=24 * 28, following.hours=23,
      threshold.factor=accumulation.factor,
      wet.threshold=wet.threshold, missing.only=FALSE
    )
  if (checks["weekday"])
    flags$weekday <- .precipQC_weekday_subdaily(
      prepared$x, interval.hours=prepared$interval.hours,
      min.coverage=weekday.min.coverage, wet.threshold=wet.threshold,
      min.wet.days=weekday.min.wet, alpha=weekday.alpha,
      underreporting.ratio=weekday.ratio
    )
  if (checks["spatial"]) {
    if (NCOL(prepared$values) < 3L) {
      warning("Spatial checks require at least three station columns; the spatial check was skipped.")
    } else {
      if (!is.numeric(spatial.hours) || anyNA(spatial.hours) ||
          any(spatial.hours < 0))
        stop("Invalid argument: 'spatial.hours' must contain non-negative numbers !")
      widths <- ifelse(spatial.hours == 0, 1L,
                       round(spatial.hours / prepared$interval.hours))
      valid <- widths >= 1L &
               abs(ifelse(spatial.hours == 0, 0,
                          widths * prepared$interval.hours - spatial.hours)) <
               1e-6
      if (!all(valid))
        stop("Invalid argument: positive values in 'spatial.hours' must be integer multiples of the temporal resolution of 'x' !")
      keep <- !duplicated(widths)
      widths <- as.integer(widths[keep])
      hours <- spatial.hours[keep]
      labels <- ifelse(hours == 0, "spatial_native",
                       paste0("spatial_", format(hours, trim=TRUE), "h"))
      spatial <- .precipQC_add_spatial(
        flags, prepared$x, widths, labels, prepared$metadata,
        prepared$station.id, coords, n.neighbours, max.distance,
        min.neighbours, min.overlap, min.correlation, spatial.cr,
        wet.threshold, elevation, elevation.scale
      )
      flags <- spatial$flags
      spatial.estimate <- spatial$estimate
      spatial.score <- spatial$score
      neighbours <- spatial$neighbours
    }
  }
  if (checks["dryspell"] && NCOL(prepared$values) >= 2L)
    flags$dryspell <- precipQC_dryspell(
      prepared$x, metadata=prepared$metadata,
      station.id=prepared$station.id, coords=coords,
      n.neighbours=n.neighbours, max.distance=max.distance,
      window.days=dryspell.days, neighbour.wet.days=neighbour.wet.days,
      neighbour.fraction=neighbour.fraction,
      wet.threshold=wet.threshold, elevation=elevation,
      elevation.scale=elevation.scale
    )
  if (checks["breakpoint"])
    breakpoint <- precipQC_breakpoint(prepared$x,
      wet.threshold=wet.threshold)

  settings <- list(resolution="subdaily", checks=checks, lower=lower,
                   max.rate=max.rate, interval.hours=prepared$interval.hours,
                   wet.threshold=wet.threshold,
                   weekday.min.wet=weekday.min.wet,
                   weekday.alpha=weekday.alpha,
                   weekday.ratio=weekday.ratio,
                   weekday.min.coverage=weekday.min.coverage,
                   station.id=prepared$station.id, coords=coords,
                   elevation=elevation, elevation.scale=elevation.scale,
                   spatial.hours=spatial.hours,
                   correction=match.arg(correction),
                   min.evidence=min.evidence, max.missing=max.missing,
                   max.suspicious=max.suspicious, min.years=min.years)

  .precipQC_finish(
    prepared, flags, breakpoint, correction, max.missing, max.suspicious,
    min.years, min.evidence,
    hard.tests=c("range", "persistence", "daily_accumulation",
                 "monthly_accumulation", "dryspell"),
    discard.breakpoint, spatial.estimate, spatial.score, neighbours,
    "subdaily", settings
  )

} # 'precipQC_subdaily' END


precipQC <- function(x, metadata=NULL, station.id="station",
                     coords=c("lon", "lat"), ...,
                     elevation=NULL, elevation.scale=500) {

  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a zoo object !")

  frequency <- sfreq(x)
  if (identical(frequency, "daily"))
    return(precipQC_daily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      elevation=elevation, elevation.scale=elevation.scale
    ))
  if (frequency %in% c("minute", "hourly"))
    return(precipQC_subdaily(
      x, metadata=metadata, station.id=station.id, coords=coords, ...,
      elevation=elevation, elevation.scale=elevation.scale
    ))

  stop("Invalid sampling frequency: sfreq(x) returned '", frequency,
       "'; only minute, hourly, and daily precipitation are supported !")

} # 'precipQC' END


print.precipQC <- function(x, ...) {

  if (!inherits(x, "precipQC"))
    stop("Invalid argument: 'x' must inherit from class 'precipQC' !")

  summary <- x$station.summary
  cat("Precipitation quality-control result\n")
  cat("  resolution :", x$settings$resolution, "\n")
  cat("  stations   :", NROW(summary), "\n")
  cat("  accepted   :", sum(summary$recommendation == "accept"), "\n")
  cat("  discarded  :", sum(summary$recommendation == "discard"), "\n")
  cat("  review data:", sum(summary$review.count), "\n")
  cat("  rejected   :", sum(summary$suspicious.count), "\n")
  invisible(x)

} # 'print.precipQC' END


plot.precipQC <- function(x, max.stations=20L,
                          col=c("#2878B5", "#D55E00", "#E69F00"), ...) {

  if (!inherits(x, "precipQC"))
    stop("Invalid argument: 'x' must inherit from class 'precipQC' !")
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
    sum(.precipQC_matrix(z, allow.logical=TRUE)),
                        numeric(1))
  if (length(flag.counts) == 0L) {
    graphics::plot.new()
    graphics::title("Flags by test")
    graphics::text(0.5, 0.5, "No active point-level tests")
  } else {
    graphics::barplot(flag.counts, horiz=TRUE, las=1, col=col[3],
                      xlab="Flagged values", main="Flags by test")
  }

  rejected <- .precipQC_matrix(x$rejected, allow.logical=TRUE)
  rejected.by.time <- rowSums(rejected)
  graphics::plot(zoo::index(x$rejected), rejected.by.time, type="h",
                 col=col[2], xlab="Time", ylab="Rejected values",
                 main="Confirmed suspicious data", ...)

  invisible(x)

} # 'plot.precipQC' END
