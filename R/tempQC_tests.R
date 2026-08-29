# File tempQC_tests.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Individual quality-control tests for air-temperature time series              #
################################################################################

.tempQC_scale <- function(x, minimum=10L) {

  x <- x[is.finite(x)]
  if (length(x) < minimum)
    return(c(centre=NA_real_, spread=NA_real_))
  centre <- stats::median(x)
  spread <- stats::mad(x, center=centre, constant=1.4826)
  if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
    spread <- stats::IQR(x) / 1.349
  if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
    spread <- NA_real_
  c(centre=centre, spread=spread)

} # '.tempQC_scale' END


.tempQC_group <- function(x, group) {

  index <- zoo::index(x)
  dates <- as.Date(index)
  switch(group,
    dayofyear=as.integer(format(dates, "%j")),
    month=as.integer(format(dates, "%m")),
    month_hour=paste(format(dates, "%m"), format(index, "%H"), sep="-")
  )

} # '.tempQC_group' END


tempQC_range <- function(x, lower=-89.4, upper=57.7) {

  values <- .precipQC_matrix(x)
  if (!is.numeric(lower) || !(length(lower) %in% c(1L, NCOL(values))) ||
      anyNA(lower) || !is.numeric(upper) ||
      !(length(upper) %in% c(1L, NCOL(values))) || anyNA(upper))
    stop("Invalid arguments: 'lower' and 'upper' must be scalar or station-specific numeric values !")
  lower <- rep(lower, length.out=NCOL(values))
  upper <- rep(upper, length.out=NCOL(values))
  if (any(lower >= upper))
    stop("Invalid argument: every value in 'lower' must be smaller than 'upper' !")

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values)))
    flags[, j] <- is.finite(values[, j]) &
                  (values[, j] < lower[j] | values[, j] > upper[j])
  .precipQC_zoo(flags, x)

} # 'tempQC_range' END


tempQC_duplicate <- function(x, min.month.values=20L,
                             min.year.values=300L) {

  .QC_duplicate_blocks(x, min.month.values=min.month.values,
                       min.year.values=min.year.values,
                       min.nonzero=0L)

} # 'tempQC_duplicate' END


tempQC_persistence <- function(x, run=7L, tolerance=0) {

  values <- .precipQC_matrix(x)
  run <- .precipQC_check_positive_integer(run, "run")
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance < 0)
    stop("Invalid argument: 'tolerance' must be a non-negative number !")

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    if (NROW(values) < run) next
    for (end in seq.int(run, NROW(values))) {
      rows <- (end - run + 1L):end
      z <- values[rows, j]
      if (all(is.finite(z)) && diff(range(z)) <= tolerance)
        flags[rows, j] <- TRUE
    }
  }
  .precipQC_zoo(flags, x)

} # 'tempQC_persistence' END


tempQC_climatology <- function(x,
                               group=c("dayofyear", "month", "month_hour"),
                               window=15L, z=6, min.samples=30L) {

  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  window <- .precipQC_check_positive_integer(window, "window")
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  if (!is.numeric(z) || length(z) != 1L || is.na(z) ||
      !is.finite(z) || z <= 0)
    stop("Invalid argument: 'z' must be a positive number !")

  groups <- .tempQC_group(x, group)
  half.window <- floor(window / 2)
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    for (g in unique(groups)) {
      target <- which(groups == g & is.finite(values[, j]))
      if (length(target) == 0L) next
      reference <- if (group == "dayofyear") {
        distance <- abs(groups - g)
        which(pmin(distance, 366L - distance) <= half.window)
      } else {
        which(groups == g)
      }
      scale <- .tempQC_scale(values[reference, j], min.samples)
      if (all(is.finite(scale)))
        flags[target, j] <- abs(values[target, j] - scale["centre"]) /
                            scale["spread"] >= z
    }
  }
  .precipQC_zoo(flags, x)

} # 'tempQC_climatology' END


tempQC_step <- function(x, group=c("month", "month_hour"), z=3,
                        min.samples=20L, max.rise=Inf, max.fall=Inf) {

  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  for (arg in c("z", "max.rise", "max.fall")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        value <= 0)
      stop("Invalid argument: '", arg, "' must be a positive number !")
  }

  groups <- .tempQC_group(x, group)
  change <- rbind(rep(NA_real_, NCOL(values)), diff(values))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    flags[, j] <- is.finite(change[, j]) &
                  (change[, j] > max.rise | change[, j] < -max.fall)
    for (g in unique(groups)) {
      rows <- which(groups == g & is.finite(change[, j]))
      scale <- .tempQC_scale(change[rows, j], min.samples)
      if (all(is.finite(scale)))
        flags[rows, j] <- flags[rows, j] |
          abs(change[rows, j] - scale["centre"]) / scale["spread"] >= z
    }
  }
  .precipQC_zoo(flags, x)

} # 'tempQC_step' END


tempQC_spike <- function(x, threshold=25) {

  values <- .precipQC_matrix(x)
  if (!is.numeric(threshold) ||
      !(length(threshold) %in% c(1L, NCOL(values))) ||
      anyNA(threshold) || any(!is.finite(threshold)) ||
      any(threshold <= 0))
    stop("Invalid argument: 'threshold' must contain positive finite values !")
  threshold <- rep(threshold, length.out=NCOL(values))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  if (NROW(values) >= 3L) {
    for (j in seq_len(NCOL(values))) {
      previous <- values[1:(NROW(values) - 2L), j]
      current <- values[2:(NROW(values) - 1L), j]
      following <- values[3:NROW(values), j]
      isolated <- is.finite(previous) & is.finite(current) &
                  is.finite(following) &
                  abs(current - previous) >= threshold[j] &
                  abs(current - following) >= threshold[j] &
                  sign(current - previous) == sign(current - following)
      flags[which(isolated) + 1L, j] <- TRUE
    }
  }
  .precipQC_zoo(flags, x)

} # 'tempQC_spike' END


.tempQC_linear_fit <- function(x, y, minimum) {

  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < minimum)
    return(NULL)
  spread <- stats::sd(x[ok])
  if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
    return(NULL)
  fit <- stats::lm.fit(cbind(1, x[ok]), y[ok])
  if (any(!is.finite(fit$coefficients))) return(NULL)
  rmse <- sqrt(mean(fit$residuals^2))
  if (!is.finite(rmse)) return(NULL)
  list(coefficients=fit$coefficients, rmse=rmse)

} # '.tempQC_linear_fit' END


tempQC_spatial <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  group=c("month", "month_hour"), n.neighbours=5L,
  max.distance=200, min.neighbours=2L, min.overlap=90L,
  min.group.overlap=20L, min.correlation=0.6,
  f=4, min.difference=8, min.se=0.1, elevation=NULL,
  elevation.scale=500) {

  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  n.neighbours <- .precipQC_check_positive_integer(n.neighbours,
                                                    "n.neighbours")
  min.neighbours <- .precipQC_check_positive_integer(min.neighbours,
                                                      "min.neighbours")
  min.overlap <- .precipQC_check_positive_integer(min.overlap,
                                                   "min.overlap")
  min.group.overlap <- .precipQC_check_positive_integer(
    min.group.overlap, "min.group.overlap")
  for (arg in c("max.distance", "f", "min.difference", "min.se")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value <= 0)
      stop("Invalid argument: '", arg, "' must be a positive finite number !")
  }
  if (!is.numeric(min.correlation) || length(min.correlation) != 1L ||
      is.na(min.correlation) || min.correlation < -1 ||
      min.correlation > 1)
    stop("Invalid argument: 'min.correlation' must be in [-1, 1] !")
  if (!is.numeric(elevation.scale) || length(elevation.scale) != 1L ||
      is.na(elevation.scale) || !is.finite(elevation.scale) ||
      elevation.scale <= 0)
    stop("Invalid argument: 'elevation.scale' must be a positive finite number !")

  stations <- colnames(values)
  meta <- .precipQC_metadata(stations, metadata, station.id, coords,
                             elevation)
  distances <- matrix(1, NCOL(values), NCOL(values),
                      dimnames=list(stations, stations))
  if (meta$has.coords) {
    distances <- .precipQC_haversine(
      as.numeric(meta$metadata[[coords[1L]]]),
      as.numeric(meta$metadata[[coords[2L]]])
    )
    dimnames(distances) <- list(stations, stations)
  }
  diag(distances) <- 0
  proximity <- matrix(1, NCOL(values), NCOL(values),
                      dimnames=list(stations, stations))
  if (meta$has.coords)
    proximity <- exp(-distances / max.distance)
  if (meta$has.elevation) {
    heights <- as.numeric(meta$metadata[[meta$elevation]])
    height.difference <- abs(outer(heights, heights, "-"))
    proximity <- proximity * exp(-height.difference / elevation.scale)
  }
  diag(proximity) <- 1
  groups <- .tempQC_group(x, group)
  estimate <- score <- matrix(NA_real_, nrow=NROW(values),
                              ncol=NCOL(values), dimnames=dimnames(values))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  selected <- vector("list", NCOL(values))
  names(selected) <- stations

  for (j in seq_len(NCOL(values))) {
    candidates <- which(seq_len(NCOL(values)) != j &
                        distances[j, ] <= max.distance)
    if (length(candidates) == 0L) next
    correlations <- errors <- rep(NA_real_, length(candidates))
    for (k in seq_along(candidates)) {
      neighbour <- candidates[k]
      ok <- is.finite(values[, j]) & is.finite(values[, neighbour])
      if (sum(ok) < min.overlap) next
      correlations[k] <- suppressWarnings(stats::cor(
        values[ok, j], values[ok, neighbour]))
      fit <- .tempQC_linear_fit(values[, neighbour], values[, j],
                                min.overlap)
      if (!is.null(fit)) errors[k] <- fit$rmse
    }
    eligible <- is.finite(correlations) & is.finite(errors) &
                correlations >= min.correlation
    candidates <- candidates[eligible]
    errors <- errors[eligible]
    if (length(candidates) < min.neighbours) next
    adjusted.errors <- errors /
                       sqrt(pmax(proximity[j, candidates],
                                 .Machine$double.xmin))
    candidates <- candidates[order(adjusted.errors)]
    candidates <- utils::head(candidates, n.neighbours)
    selected[[j]] <- stations[candidates]

    for (g in unique(groups)) {
      rows <- which(groups == g)
      predictions <- matrix(NA_real_, nrow=length(rows),
                            ncol=length(candidates))
      standard.errors <- rep(NA_real_, length(candidates))
      for (k in seq_along(candidates)) {
        neighbour <- candidates[k]
        fit <- .tempQC_linear_fit(values[rows, neighbour],
                                  values[rows, j], min.group.overlap)
        if (is.null(fit)) next
        predictions[, k] <- fit$coefficients[1L] +
                             fit$coefficients[2L] * values[rows, neighbour]
        standard.errors[k] <- max(fit$rmse, min.se)
      }
      for (i in seq_along(rows)) {
        ok <- is.finite(predictions[i, ]) & is.finite(standard.errors)
        if (sum(ok) < min.neighbours) next
        weights <- proximity[j, candidates[ok]] /
                   standard.errors[ok]^2
        estimate[rows[i], j] <- sum(predictions[i, ok] * weights) /
                                sum(weights)
        combined.se <- sqrt(sum(ok) / sum(weights))
        residual <- values[rows[i], j] - estimate[rows[i], j]
        if (is.finite(residual)) {
          score[rows[i], j] <- abs(residual) / combined.se
          flags[rows[i], j] <- abs(residual) >= min.difference &&
                               score[rows[i], j] >= f
        }
      }
    }
  }

  out <- list(flags=.precipQC_zoo(flags, x),
              scores=.precipQC_zoo(score, x),
              estimate=.precipQC_zoo(estimate, x),
              neighbours=selected)
  class(out) <- c("tempQC_spatial", "list")
  out

} # 'tempQC_spatial' END


tempQC_internal <- function(tmin, tmax, lagged=TRUE, max.range=40) {

  minimum <- .precipQC_matrix(tmin)
  maximum <- .precipQC_matrix(tmax)
  if (!identical(zoo::index(tmin), zoo::index(tmax)) ||
      !identical(dim(minimum), dim(maximum)) ||
      !identical(colnames(minimum), colnames(maximum)))
    stop("Invalid arguments: 'tmin' and 'tmax' must have identical indices and station columns !")
  lagged <- .precipQC_validate_logical(lagged, "lagged")
  if (!is.numeric(max.range) || length(max.range) != 1L ||
      is.na(max.range) || !is.finite(max.range) || max.range <= 0)
    stop("Invalid argument: 'max.range' must be a positive number !")

  min.flags <- max.flags <- matrix(FALSE, nrow=NROW(minimum),
                                   ncol=NCOL(minimum),
                                   dimnames=dimnames(minimum))
  inconsistent <- is.finite(minimum) & is.finite(maximum) &
                  minimum > maximum
  min.flags[inconsistent] <- max.flags[inconsistent] <- TRUE
  if (lagged && NROW(minimum) >= 3L) {
    for (j in seq_len(NCOL(minimum))) {
      for (i in 2:(NROW(minimum) - 1L)) {
        min.window <- minimum[(i - 1L):(i + 1L), j]
        max.window <- maximum[(i - 1L):(i + 1L), j]
        if (is.finite(maximum[i, j]) && any(is.finite(min.window))) {
          mixed <- maximum[i, j] < min(min.window, na.rm=TRUE)
          excessive <- maximum[i, j] >= max(min.window, na.rm=TRUE) +
                       max.range
          max.flags[i, j] <- max.flags[i, j] | mixed | excessive
        }
        if (is.finite(minimum[i, j]) && any(is.finite(max.window))) {
          mixed <- minimum[i, j] > max(max.window, na.rm=TRUE)
          excessive <- minimum[i, j] <= min(max.window, na.rm=TRUE) -
                       max.range
          min.flags[i, j] <- min.flags[i, j] | mixed | excessive
        }
      }
    }
  }
  list(tmin=.precipQC_zoo(min.flags, tmin),
       tmax=.precipQC_zoo(max.flags, tmax))

} # 'tempQC_internal' END


tempQC_breakpoint <- function(
  x, min.years=10L, alpha=0.05, min.shift=1,
  min.completeness=0.8, indicators=c("mean", "sd")) {

  values <- .precipQC_matrix(x)
  min.years <- .precipQC_check_positive_integer(min.years, "min.years")
  alpha <- .precipQC_check_probability(alpha, "alpha")
  if (!is.numeric(min.shift) || length(min.shift) != 1L ||
      is.na(min.shift) || !is.finite(min.shift) || min.shift < 0)
    stop("Invalid argument: 'min.shift' must be a non-negative number !")
  if (!is.numeric(min.completeness) || length(min.completeness) != 1L ||
      is.na(min.completeness) || !is.finite(min.completeness) ||
      min.completeness <= 0 || min.completeness > 1)
    stop("Invalid argument: 'min.completeness' must be in (0, 1] !")
  allowed <- c("mean", "sd", "minimum", "maximum")
  if (!is.character(indicators) || length(indicators) == 0L ||
      anyNA(indicators) || any(!indicators %in% allowed) ||
      anyDuplicated(indicators))
    stop("Invalid argument: 'indicators' must be a unique subset of mean, sd, minimum, maximum !")

  dates <- as.Date(zoo::index(x))
  daily.dates <- sort(unique(dates))
  day <- match(dates, daily.dates)
  daily <- matrix(NA_real_, nrow=length(daily.dates),
                  ncol=NCOL(values), dimnames=list(NULL, colnames(values)))
  for (j in seq_len(NCOL(values)))
    daily[, j] <- vapply(seq_along(daily.dates), function(i) {
      z <- values[day == i, j]
      if (all(!is.finite(z))) NA_real_ else mean(z[is.finite(z)])
    }, numeric(1))
  years <- as.integer(format(daily.dates, "%Y"))
  year.levels <- sort(unique(years))
  expected <- vapply(year.levels, function(year)
    as.integer(as.Date(paste0(year + 1L, "-01-01")) -
               as.Date(paste0(year, "-01-01"))), integer(1))
  out <- data.frame(station=colnames(values), breakpoint.year=NA_integer_,
                    indicator=NA_character_, p.value=NA_real_,
                    relative.change=NA_real_, n.indicators=0L,
                    flagged=FALSE, stringsAsFactors=FALSE)

  for (j in seq_len(NCOL(values))) {
    complete <- vapply(year.levels, function(year)
      sum(is.finite(daily[years == year, j])), integer(1)) /
      expected >= min.completeness
    annual <- lapply(indicators, function(indicator) {
      z <- vapply(year.levels, function(year) {
        y <- daily[years == year, j]
        y <- y[is.finite(y)]
        if (length(y) == 0L) return(NA_real_)
        switch(indicator, mean=mean(y), sd=stats::sd(y),
               minimum=min(y), maximum=max(y))
      }, numeric(1))
      z[!complete] <- NA_real_
      names(z) <- year.levels
      z
    })
    names(annual) <- indicators
    diagnostics <- lapply(annual, function(z) {
      z <- z[is.finite(z)]
      if (length(z) < min.years) return(NULL)
      test <- .precipQC_pettitt(z)
      split <- as.integer(test["split"])
      before <- stats::median(z[seq_len(split)])
      after <- stats::median(z[(split + 1L):length(z)])
      list(year=as.integer(names(z)[split]), p.value=unname(test["p.value"]),
           shift=abs(after - before))
    })
    valid <- !vapply(diagnostics, is.null, logical(1))
    if (!any(valid)) next
    diagnostics <- diagnostics[valid]
    adjusted <- stats::p.adjust(vapply(diagnostics, `[[`, numeric(1),
                                       "p.value"), method="holm")
    shifts <- vapply(diagnostics, `[[`, numeric(1), "shift")
    flagged <- adjusted < alpha & shifts >= min.shift
    best <- order(adjusted, -shifts)[1L]
    names.valid <- names(diagnostics)
    out$breakpoint.year[j] <- diagnostics[[best]]$year
    out$indicator[j] <- names.valid[best]
    out$p.value[j] <- adjusted[best]
    out$relative.change[j] <- shifts[best]
    out$n.indicators[j] <- sum(flagged)
    out$flagged[j] <- any(flagged)
  }
  out

} # 'tempQC_breakpoint' END
