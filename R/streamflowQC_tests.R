# File streamflowQC_tests.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Individual quality-control tests for streamflow time series                  #
################################################################################

streamflowQC_range <- function(x, lower=0, upper=Inf) {

  # Work with a station-by-time numeric matrix while preserving column names.
  values <- .precipQC_matrix(x)

  # Allow one physical limit for all stations or one limit per station.
  if (!is.numeric(lower) || !(length(lower) %in% c(1L, NCOL(values))) ||
      anyNA(lower))
    stop("Invalid argument: 'lower' must contain one value or one value per station !")
  if (!is.numeric(upper) || !(length(upper) %in% c(1L, NCOL(values))) ||
      anyNA(upper))
    stop("Invalid argument: 'upper' must contain one value or one value per station !")

  lower <- rep(lower, length.out=NCOL(values))
  upper <- rep(upper, length.out=NCOL(values))
  if (any(lower >= upper))
    stop("Invalid argument: every value in 'lower' must be smaller than 'upper' !")

  # Missing values are handled through completeness; other invalid values fail.
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values)))
    flags[, j] <- !is.na(values[, j]) &
                  (!is.finite(values[, j]) | values[, j] < lower[j] |
                   values[, j] > upper[j])

  .precipQC_zoo(flags, x)

} # 'streamflowQC_range' END


streamflowQC_duplicate <- function(x, min.month.values=20L,
                                   min.year.values=300L,
                                   min.nonzero=1L) {

  # Reuse the package-wide exact copied-block detector with flow-safe zero rules.
  .QC_duplicate_blocks(
    x, min.month.values=min.month.values,
    min.year.values=min.year.values, min.nonzero=min.nonzero,
    zero.threshold=0
  )

} # 'streamflowQC_duplicate' END


streamflowQC_gap <- function(x, interval=NULL) {

  # A gap flag marks the first finite value following absent data.
  values <- .precipQC_matrix(x)
  times <- zoo::index(x)
  if (!(inherits(times, "Date") || inherits(times, "POSIXt")))
    stop("Invalid argument: 'x' must have a Date or POSIXt time index !")

  # Infer the expected interval from the most common positive difference.
  differences <- if (inherits(times, "Date")) {
    as.numeric(diff(times))
  } else {
    as.numeric(diff(as.POSIXct(times)), units="secs")
  }
  if (is.null(interval)) {
    if (length(differences) == 0L || any(!is.finite(differences)) ||
        any(differences <= 0))
      stop("Invalid argument: the time index in 'x' must be strictly increasing !")
    labels <- as.character(round(differences, 6))
    interval <- as.numeric(names(sort(table(labels), decreasing=TRUE))[1L])
  }
  if (!is.numeric(interval) || length(interval) != 1L || is.na(interval) ||
      !is.finite(interval) || interval <= 0)
    stop("Invalid argument: 'interval' must be a positive finite number !")

  # Index gaps affect all stations; internal NA runs are station-specific.
  index.gap <- c(FALSE, differences > 1.5 * interval)
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  if (NROW(values) >= 2L) {
    for (j in seq_len(NCOL(values))) {
      follows.missing <- c(FALSE, !is.finite(values[-NROW(values), j]) &
                                  is.finite(values[-1L, j]))
      flags[, j] <- is.finite(values[, j]) & (index.gap | follows.missing)
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_gap' END


streamflowQC_climatology <- function(
  x, group=c("dayofyear", "month", "month_slot"), window=5L,
  z=6, min.samples=30L, offset=0.01) {

  # Validate the reproducible seasonal-reference controls.
  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  window <- .precipQC_check_positive_integer(window, "window")
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  if (!is.numeric(z) || length(z) != 1L || is.na(z) ||
      !is.finite(z) || z <= 0)
    stop("Invalid argument: 'z' must be a positive finite number !")
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) ||
      !is.finite(offset) || offset <= 0)
    stop("Invalid argument: 'offset' must be a positive finite number !")

  # Daily windows are circular; sub-daily groups retain their calendar slot.
  times <- zoo::index(x)
  dates <- as.Date(times)
  groups <- switch(group,
    dayofyear=as.integer(format(dates, "%j")),
    month=as.integer(format(dates, "%m")),
    month_slot=paste(format(dates, "%m"), format(times, "%H:%M"), sep="-")
  )
  half.window <- floor(window / 2L)
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  # Compare log-transformed flow with the matching seasonal distribution.
  for (j in seq_len(NCOL(values))) {
    transformed <- rep(NA_real_, NROW(values))
    valid <- is.finite(values[, j]) & values[, j] >= 0
    transformed[valid] <- log(values[valid, j] + offset)
    for (g in unique(groups)) {
      target <- which(groups == g & valid)
      if (length(target) == 0L) next

      if (group == "dayofyear") {
        distance <- abs(as.integer(groups) - as.integer(g))
        distance <- pmin(distance, 366L - distance)
        reference <- which(distance <= half.window & valid)
      } else {
        reference <- which(groups == g & valid)
      }
      if (length(reference) < min.samples) next

      centre <- mean(transformed[reference])
      spread <- stats::sd(transformed[reference])
      if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps)) next
      flags[target, j] <- abs(transformed[target] - centre) > z * spread
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_climatology' END


streamflowQC_flatline <- function(
  x, run=11L, high.run=Inf, high.prob=0.99,
  tolerance=0, flag.zero=FALSE) {

  # Validate duration, precision, and high-flow controls.
  values <- .precipQC_matrix(x)
  run <- .precipQC_check_positive_integer(run, "run")
  if (!is.numeric(high.run) || length(high.run) != 1L ||
      is.na(high.run) || high.run < 1)
    stop("Invalid argument: 'high.run' must be a positive number !")
  if (is.finite(high.run))
    high.run <- .precipQC_check_positive_integer(high.run, "high.run")
  high.prob <- .precipQC_check_probability(high.prob, "high.prob")
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      is.na(tolerance) || !is.finite(tolerance) || tolerance < 0)
    stop("Invalid argument: 'tolerance' must be a non-negative finite number !")
  flag.zero <- .precipQC_validate_logical(flag.zero, "flag.zero")

  # Flag every value in sufficiently long near-constant runs.
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    finite <- values[is.finite(values[, j]), j]
    high.threshold <- if (length(finite) > 0L) {
      as.numeric(stats::quantile(finite, high.prob, names=FALSE, type=8))
    } else {
      NA_real_
    }
    start <- 1L
    for (i in seq_len(NROW(values) + 1L)) {
      continues <- i <= NROW(values) && i > start &&
                   is.finite(values[i, j]) &&
                   is.finite(values[i - 1L, j]) &&
                   abs(values[i, j] - values[i - 1L, j]) <= tolerance
      if (i == start || continues) next

      finish <- i - 1L
      length.run <- finish - start + 1L
      representative <- values[start, j]
      eligible <- is.finite(representative) &&
                  (flag.zero || representative != 0)
      ordinary <- eligible && length.run >= run
      high <- eligible && is.finite(high.run) &&
              length.run >= high.run && is.finite(high.threshold) &&
              representative > high.threshold
      if (ordinary || high)
        flags[start:finish, j] <- TRUE
      start <- i
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_flatline' END


streamflowQC_spike <- function(x, factor=5, prob=0.99) {

  # Isolated peaks and dips must reverse at the immediately following value.
  values <- .precipQC_matrix(x)
  if (!is.numeric(factor) || length(factor) != 1L || is.na(factor) ||
      !is.finite(factor) || factor <= 1)
    stop("Invalid argument: 'factor' must be larger than one !")
  prob <- .precipQC_check_probability(prob, "prob")

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  if (NROW(values) >= 3L) {
    for (j in seq_len(NCOL(values))) {
      threshold <- as.numeric(stats::quantile(
        values[is.finite(values[, j]), j], prob, names=FALSE, type=8,
        na.rm=TRUE
      ))
      for (i in 2:(NROW(values) - 1L)) {
        triplet <- values[(i - 1L):(i + 1L), j]
        if (any(!is.finite(triplet))) next
        relative <- (triplet[2L] > factor * triplet[1L] &&
                     triplet[2L] > factor * triplet[3L]) ||
                    (triplet[1L] > factor * triplet[2L] &&
                     triplet[3L] > factor * triplet[2L])
        absolute <- is.finite(threshold) &&
                    abs(triplet[2L] - triplet[1L]) > threshold &&
                    abs(triplet[2L] - triplet[3L]) > threshold &&
                    sign(triplet[2L] - triplet[1L]) ==
                    sign(triplet[2L] - triplet[3L])
        flags[i, j] <- relative || absolute
      }
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_spike' END


streamflowQC_rate <- function(
  x, group=c("month", "month_slot"), z=6, min.samples=30L,
  max.rise=Inf, max.fall=Inf, drop.factor=5, offset=0.01) {

  # Rate tests operate on signed consecutive changes in log-flow.
  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  if (!is.numeric(z) || length(z) != 1L || is.na(z) ||
      !is.finite(z) || z <= 0)
    stop("Invalid argument: 'z' must be a positive finite number !")
  if (!is.numeric(drop.factor) || length(drop.factor) != 1L ||
      is.na(drop.factor) || !is.finite(drop.factor) || drop.factor <= 1)
    stop("Invalid argument: 'drop.factor' must be larger than one !")
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) ||
      !is.finite(offset) || offset <= 0)
    stop("Invalid argument: 'offset' must be a positive finite number !")

  # Absolute limits may be common or station-specific.
  for (name in c("max.rise", "max.fall")) {
    limit <- get(name)
    if (!is.numeric(limit) || !(length(limit) %in% c(1L, NCOL(values))) ||
        anyNA(limit) || any(limit <= 0))
      stop("Invalid argument: '", name,
           "' must contain positive values !")
  }
  max.rise <- rep(max.rise, length.out=NCOL(values))
  max.fall <- rep(max.fall, length.out=NCOL(values))

  # Calendar grouping avoids treating normal seasonal changes as errors.
  times <- zoo::index(x)
  groups <- if (group == "month") {
    format(as.Date(times), "%m")
  } else {
    paste(format(as.Date(times), "%m"), format(times, "%H:%M"), sep="-")
  }
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  for (j in seq_len(NCOL(values))) {
    valid <- is.finite(values[, j]) & values[, j] >= 0
    transformed <- rep(NA_real_, NROW(values))
    transformed[valid] <- log(values[valid, j] + offset)
    change <- c(NA_real_, diff(transformed))
    raw.change <- c(NA_real_, diff(values[, j]))

    for (g in unique(groups)) {
      reference <- which(groups == g & is.finite(change))
      if (length(reference) < min.samples) next
      centre <- stats::median(change[reference])
      spread <- stats::mad(change[reference], center=centre,
                           constant=1.4826)
      if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
        spread <- stats::IQR(change[reference]) / 1.349
      if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps)) next
      target <- which(groups == g & is.finite(change))
      flags[target, j] <- abs(change[target] - centre) > z * spread
    }

    # User limits and a five-fold abrupt-drop rule complement robust history.
    flags[, j] <- flags[, j] |
                  (is.finite(raw.change) & raw.change > max.rise[j]) |
                  (is.finite(raw.change) & -raw.change > max.fall[j])
    if (NROW(values) >= 2L) {
      rapid.drop <- c(FALSE, is.finite(values[-NROW(values), j]) &
                             is.finite(values[-1L, j]) &
                             values[-NROW(values), j] >
                             drop.factor * values[-1L, j])
      flags[, j] <- flags[, j] | rapid.drop
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_rate' END


streamflowQC_fluctuation <- function(
  x, window=16L, minimum=8L, low.diff.prob=0.05) {

  # The test identifies repeated sign reversals after ignoring tiny changes.
  values <- .precipQC_matrix(x)
  window <- .precipQC_check_positive_integer(window, "window")
  minimum <- .precipQC_check_positive_integer(minimum, "minimum")
  low.diff.prob <- .precipQC_check_probability(low.diff.prob,
                                                "low.diff.prob")
  if (minimum >= window)
    stop("Invalid argument: 'minimum' must be smaller than 'window' !")

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    changes <- c(NA_real_, diff(values[, j]))
    finite <- abs(changes[is.finite(changes)])
    if (length(finite) == 0L || NROW(values) < window) next
    small <- as.numeric(stats::quantile(finite, low.diff.prob,
                                        names=FALSE, type=8))
    direction <- sign(changes)
    direction[is.finite(changes) & abs(changes) < small] <- 0
    reversals <- c(FALSE, direction[-1L] * direction[-NROW(values)] < 0)
    for (finish in seq.int(window, NROW(values))) {
      rows <- (finish - window + 1L):finish
      if (sum(reversals[rows], na.rm=TRUE) > minimum)
        flags[rows, j] <- TRUE
    }
  }

  .precipQC_zoo(flags, x)

} # 'streamflowQC_fluctuation' END


.streamflowQC_gev_cdf <- function(x, location, scale, shape) {

  # Evaluate the Gumbel limit separately for numerical stability.
  standardized <- (x - location) / scale
  if (abs(shape) <= 1e-6)
    return(exp(-exp(-standardized)))

  support <- 1 + shape * standardized
  out <- rep(NA_real_, length(x))
  inside <- is.finite(x) & support > 0
  out[inside] <- exp(-support[inside]^(-1 / shape))

  # Values outside the finite GEV support lie at a distribution boundary.
  if (shape > 0)
    out[is.finite(x) & support <= 0] <- 0
  else
    out[is.finite(x) & support <= 0] <- 1
  out

} # '.streamflowQC_gev_cdf' END


.streamflowQC_gev_fit <- function(x) {

  # At least three distinct annual maxima are needed for three parameters.
  x <- x[is.finite(x)]
  if (length(x) < 3L || length(unique(x)) < 3L)
    return(NULL)

  # Start near the Gumbel moment estimates and optimize unconstrained scale.
  start.scale <- stats::sd(x) * sqrt(6) / pi
  if (!is.finite(start.scale) || start.scale <= 0)
    start.scale <- max(stats::IQR(x) / 1.349, sqrt(.Machine$double.eps))
  start <- c(location=mean(x) - 0.5772156649 * start.scale,
             log.scale=log(start.scale), shape=0)

  objective <- function(parameters) {
    location <- parameters[1L]
    scale <- exp(parameters[2L])
    shape <- parameters[3L]
    standardized <- (x - location) / scale
    if (abs(shape) <= 1e-6) {
      log.density <- -log(scale) - standardized - exp(-standardized)
    } else {
      support <- 1 + shape * standardized
      if (any(!is.finite(support)) || any(support <= 0)) return(1e100)
      log.density <- -log(scale) - (1 / shape + 1) * log(support) -
                     support^(-1 / shape)
    }
    value <- -sum(log.density)
    if (is.finite(value)) value else 1e100
  } # 'objective' END

  fit <- try(stats::optim(start, objective, method="Nelder-Mead",
                          control=list(maxit=5000L)), silent=TRUE)
  if (inherits(fit, "try-error") || fit$convergence != 0L ||
      !is.finite(fit$value))
    return(NULL)

  c(location=unname(fit$par[1L]), scale=exp(unname(fit$par[2L])),
    shape=unname(fit$par[3L]))

} # '.streamflowQC_gev_fit' END


streamflowQC_highflow <- function(
  x, log.z=6, amax.ratio=2, return.period=1000,
  min.years=10L, offset=0.01) {

  # Validate high-flow screening thresholds from the published framework.
  values <- .precipQC_matrix(x)
  min.years <- .precipQC_check_positive_integer(min.years, "min.years")
  for (name in c("log.z", "amax.ratio", "return.period", "offset")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value <= 0)
      stop("Invalid argument: '", name, "' must be a positive finite number !")
  }
  if (amax.ratio <= 1)
    stop("Invalid argument: 'amax.ratio' must be larger than one !")

  # Store correlated high-flow diagnostics under one evidence family.
  dates <- as.Date(zoo::index(x))
  years <- format(dates, "%Y")
  log.flags <- ratio.flags <- gev.flags <-
    matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
           dimnames=dimnames(values))
  return.values <- matrix(NA_real_, nrow=NROW(values), ncol=NCOL(values),
                          dimnames=dimnames(values))
  diagnostics <- data.frame(
    station=colnames(values), n.years=integer(NCOL(values)),
    location=NA_real_, scale=NA_real_, shape=NA_real_,
    fitted=FALSE, stringsAsFactors=FALSE
  )

  for (j in seq_len(NCOL(values))) {
    valid <- is.finite(values[, j]) & values[, j] >= 0
    transformed <- log(values[valid, j] + offset)
    if (length(transformed) >= 2L) {
      centre <- mean(transformed)
      spread <- stats::sd(transformed)
      if (is.finite(spread) && spread > sqrt(.Machine$double.eps))
        log.flags[valid, j] <- transformed > centre + log.z * spread
    }

    # Calculate one annual maximum for every year with finite data.
    annual <- tapply(values[, j], years, function(z) {
      z <- z[is.finite(z)]
      if (length(z) == 0L) NA_real_ else max(z)
    })
    annual <- annual[is.finite(annual)]
    diagnostics$n.years[j] <- length(annual)
    if (length(annual) >= 2L) {
      ranked <- sort(annual, decreasing=TRUE)
      ratio <- if (ranked[2L] <= 0) Inf else ranked[1L] / ranked[2L]
      if (ratio > amax.ratio)
        ratio.flags[, j] <- valid & values[, j] == ranked[1L]
    }

    # GEV fitting is diagnostic until a minimum annual record is available.
    if (length(annual) >= min.years) {
      fit <- .streamflowQC_gev_fit(as.numeric(annual))
      if (!is.null(fit)) {
        cdf <- .streamflowQC_gev_cdf(values[, j], fit["location"],
                                     fit["scale"], fit["shape"])
        return.values[, j] <- 1 / pmax(1 - cdf, .Machine$double.xmin)
        gev.flags[, j] <- valid & return.values[, j] > return.period
        diagnostics[j, c("location", "scale", "shape")] <- fit
        diagnostics$fitted[j] <- TRUE
      }
    }
  }

  out <- list(
    flags=.precipQC_zoo(log.flags | ratio.flags | gev.flags, x),
    components=list(log=.precipQC_zoo(log.flags, x),
                    amax.ratio=.precipQC_zoo(ratio.flags, x),
                    gev=.precipQC_zoo(gev.flags, x)),
    return.period=.precipQC_zoo(return.values, x),
    diagnostics=diagnostics
  )
  class(out) <- c("streamflowQC_highflow", "list")
  out

} # 'streamflowQC_highflow' END


.streamflowQC_weighted_median <- function(x, weights) {

  # A weighted median is resistant to a single faulty neighbour.
  order.x <- order(x)
  x <- x[order.x]
  weights <- weights[order.x] / sum(weights[order.x])
  x[which(cumsum(weights) >= 0.5)[1L]]

} # '.streamflowQC_weighted_median' END


streamflowQC_spatial <- function(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  area=NULL, elevation=NULL, elevation.scale=500,
  n.neighbours=5L, max.distance=400, min.neighbours=2L,
  min.support=1L, min.overlap=90L, min.correlation=0.5,
  window=3L, high.prob=0.99, support.prob=0.95, f=3) {

  # Validate neighbour and event-corroboration controls.
  values <- .precipQC_matrix(x)
  min.neighbours <- .precipQC_check_positive_integer(min.neighbours,
                                                      "min.neighbours")
  min.support <- .precipQC_check_positive_integer(min.support,
                                                   "min.support")
  window <- .precipQC_check_positive_integer(window, "window")
  high.prob <- .precipQC_check_probability(high.prob, "high.prob")
  support.prob <- .precipQC_check_probability(support.prob,
                                               "support.prob")
  if (!is.numeric(f) || length(f) != 1L || is.na(f) ||
      !is.finite(f) || f <= 0)
    stop("Invalid argument: 'f' must be a positive finite number !")

  # Match metadata exactly as in the precipitation and temperature QC APIs.
  meta <- .precipQC_metadata(colnames(values), metadata, station.id,
                             coords, elevation)
  areas <- rep(1, NCOL(values))
  if (!is.null(area)) {
    if (!is.character(area) || length(area) != 1L || is.na(area) ||
        !nzchar(area) || !(area %in% names(meta$metadata)))
      stop("Invalid argument: 'area' must name a catchment-area metadata column or be NULL !")
    areas <- meta$metadata[[area]]
    if (!is.numeric(areas) || any(!is.finite(areas)) || any(areas <= 0))
      stop("Invalid argument: the catchment-area metadata column must contain positive finite numeric values !")
  }

  # Rank candidates with geographic, elevation, and correlation information.
  neighbours <- .precipQC_neighbours(
    values=values, metadata=metadata, station.id=station.id,
    coords=coords, n.neighbours=n.neighbours,
    max.distance=max.distance, min.overlap=min.overlap,
    min.correlation=min.correlation, elevation=elevation,
    elevation.scale=elevation.scale
  )
  transformed <- log1p(pmax(sweep(values, 2L, areas, "/"), 0))
  estimate.t <- score <- matrix(NA_real_, nrow=NROW(values),
                                ncol=NCOL(values), dimnames=dimnames(values))
  support.count <- matrix(NA_integer_, nrow=NROW(values),
                          ncol=NCOL(values), dimnames=dimnames(values))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  selected <- vector("list", NCOL(values))
  names(selected) <- colnames(values)

  for (j in seq_len(NCOL(values))) {
    candidates <- neighbours$index[[j]]
    if (length(candidates) < min.neighbours) next
    selected[[j]] <- colnames(values)[candidates]

    # Pairwise log-flow regressions allow different basin magnitudes.
    predictions <- matrix(NA_real_, nrow=NROW(values),
                          ncol=length(candidates))
    errors <- rep(NA_real_, length(candidates))
    for (k in seq_along(candidates)) {
      neighbour <- candidates[k]
      ok <- is.finite(transformed[, j]) &
            is.finite(transformed[, neighbour])
      if (sum(ok) < min.overlap) next
      design <- cbind(1, transformed[ok, neighbour])
      fit <- stats::lm.fit(design, transformed[ok, j])
      if (any(!is.finite(fit$coefficients))) next
      predictions[, k] <- fit$coefficients[1L] +
                           fit$coefficients[2L] * transformed[, neighbour]
      errors[k] <- stats::mad(fit$residuals, constant=1.4826)
      if (!is.finite(errors[k]) || errors[k] <= sqrt(.Machine$double.eps))
        errors[k] <- stats::IQR(fit$residuals) / 1.349
    }

    # Combine simultaneous predictions using inverse residual error.
    for (i in seq_len(NROW(values))) {
      ok <- is.finite(predictions[i, ]) & is.finite(errors) & errors > 0
      if (sum(ok) < min.neighbours) next
      estimate.t[i, j] <- .streamflowQC_weighted_median(
        predictions[i, ok], 1 / errors[ok]^2
      )
    }

    residual <- transformed[, j] - estimate.t[, j]
    scale <- stats::mad(residual, na.rm=TRUE, constant=1.4826)
    if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps))
      scale <- stats::IQR(residual, na.rm=TRUE) / 1.349
    if (is.finite(scale) && scale > sqrt(.Machine$double.eps))
      score[, j] <- abs(residual) / scale

    # A rare target event needs at least one high-flow neighbour in the window.
    target.threshold <- as.numeric(stats::quantile(
      values[is.finite(values[, j]), j], high.prob,
      names=FALSE, type=8, na.rm=TRUE
    ))
    neighbour.threshold <- vapply(candidates, function(k) {
      as.numeric(stats::quantile(values[is.finite(values[, k]), k],
                                 support.prob, names=FALSE, type=8,
                                 na.rm=TRUE))
    }, numeric(1))
    events <- which(is.finite(values[, j]) &
                    values[, j] > target.threshold)
    for (i in events) {
      rows <- max(1L, i - window):min(NROW(values), i + window)
      supported <- vapply(seq_along(candidates), function(k) {
        any(values[rows, candidates[k]] > neighbour.threshold[k],
            na.rm=TRUE)
      }, logical(1))
      support.count[i, j] <- sum(supported)
      flags[i, j] <- support.count[i, j] < min.support &&
                     is.finite(score[i, j]) && score[i, j] >= f
    }
  }

  # Convert the specific-flow estimate back to each target station's units.
  estimate <- sweep(expm1(estimate.t), 2L, areas, "*")
  out <- list(flags=.precipQC_zoo(flags, x),
              scores=.precipQC_zoo(score, x),
              estimate=.precipQC_zoo(estimate, x),
              support.count=.precipQC_zoo(support.count, x),
              neighbours=selected)
  class(out) <- c("streamflowQC_spatial", "list")
  out

} # 'streamflowQC_spatial' END


streamflowQC_breakpoint <- function(
  x, min.years=10L, alpha=0.01, min.relative.change=0.5,
  min.completeness=0.8,
  indicators=c("mean", "sd", "minimum", "maximum")) {

  # Validate annual-indicator and station-homogeneity controls.
  values <- .precipQC_matrix(x)
  min.years <- .precipQC_check_positive_integer(min.years, "min.years")
  alpha <- .precipQC_check_probability(alpha, "alpha")
  if (!is.numeric(min.relative.change) ||
      length(min.relative.change) != 1L || is.na(min.relative.change) ||
      !is.finite(min.relative.change) || min.relative.change < 0)
    stop("Invalid argument: 'min.relative.change' must be non-negative !")
  if (!is.numeric(min.completeness) || length(min.completeness) != 1L ||
      is.na(min.completeness) || !is.finite(min.completeness) ||
      min.completeness <= 0 || min.completeness > 1)
    stop("Invalid argument: 'min.completeness' must be in (0, 1] !")
  allowed <- c("mean", "sd", "minimum", "maximum")
  if (!is.character(indicators) || length(indicators) == 0L ||
      anyNA(indicators) || any(!indicators %in% allowed) ||
      anyDuplicated(indicators))
    stop("Invalid argument: 'indicators' must be a unique subset of mean, sd, minimum, maximum !")

  # Aggregate sub-daily input to daily means before annual assessment.
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
  expected <- vapply(year.levels, function(year) {
    as.integer(as.Date(paste0(year + 1L, "-01-01")) -
               as.Date(paste0(year, "-01-01")))
  }, integer(1))
  out <- data.frame(station=colnames(values), breakpoint.year=NA_integer_,
                    indicator=NA_character_, p.value=NA_real_,
                    relative.change=NA_real_, n.indicators=0L,
                    flagged=FALSE, stringsAsFactors=FALSE)

  for (j in seq_len(NCOL(values))) {
    observed <- vapply(year.levels, function(year) {
      sum(is.finite(daily[years == year, j]))
    }, integer(1))
    complete <- observed / expected >= min.completeness

    # Pettitt tests are run independently on several annual flow properties.
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
      relative <- if (abs(before) <= sqrt(.Machine$double.eps)) {
        if (abs(after) <= sqrt(.Machine$double.eps)) 0 else Inf
      } else {
        abs(after - before) / abs(before)
      }
      list(year=as.integer(names(z)[split]),
           p.value=unname(test["p.value"]), relative.change=relative)
    })
    valid <- !vapply(diagnostics, is.null, logical(1))
    if (!any(valid)) next

    diagnostics <- diagnostics[valid]
    adjusted <- stats::p.adjust(vapply(diagnostics, `[[`, numeric(1),
                                       "p.value"), method="holm")
    relative <- vapply(diagnostics, `[[`, numeric(1), "relative.change")
    flagged <- adjusted < alpha & relative >= min.relative.change
    candidates <- if (any(flagged)) which(flagged) else seq_along(adjusted)
    selected <- candidates[which.min(adjusted[candidates])]
    out$breakpoint.year[j] <- diagnostics[[selected]]$year
    out$indicator[j] <- names(diagnostics)[selected]
    out$p.value[j] <- adjusted[selected]
    out$relative.change[j] <- relative[selected]
    out$n.indicators[j] <- sum(flagged)
    out$flagged[j] <- any(flagged)
  }

  out

} # 'streamflowQC_breakpoint' END
