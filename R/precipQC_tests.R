# File precipQC_tests.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Individual quality-control tests for precipitation time series               #
################################################################################

.precipQC_matrix <- function(x, allow.logical=FALSE) {

  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a 'zoo' object !")

  values <- zoo::coredata(x)
  if (!is.numeric(values) && !(allow.logical && is.logical(values)))
    stop("Invalid argument: the values in 'x' must be numeric !")

  if (is.null(dim(values)))
    values <- matrix(values, ncol=1L)

  if (is.null(colnames(values)))
    colnames(values) <- paste0("station", seq_len(NCOL(values)))

  values

} # '.precipQC_matrix' END


.precipQC_zoo <- function(values, x) {

  zoo::zoo(values, zoo::index(x))

} # '.precipQC_zoo' END


.precipQC_runs <- function(x) {

  runs <- rle(x)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  list(values=runs$values, lengths=runs$lengths,
       starts=starts, ends=ends)

} # '.precipQC_runs' END


.precipQC_check_positive_integer <- function(x, name) {

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || abs(x - round(x)) > sqrt(.Machine$double.eps))
    stop("Invalid argument: '", name, "' must be a positive integer !")

  as.integer(round(x))

} # '.precipQC_check_positive_integer' END


.precipQC_check_probability <- function(x, name) {

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x <= 0 || x >= 1)
    stop("Invalid argument: '", name, "' must be in (0, 1) !")

  x

} # '.precipQC_check_probability' END


.precipQC_upper_threshold <- function(x, prob, z, min.samples) {

  work <- log1p(x[is.finite(x) & x >= 0])
  if (length(work) < min.samples)
    return(NA_real_)

  repeat {
    centre <- stats::median(work)
    spread <- stats::mad(work, center=centre, constant=1.4826)
    if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
      spread <- stats::IQR(work) / 1.349
    if (!is.finite(spread) || spread <= sqrt(.Machine$double.eps))
      spread <- sqrt(.Machine$double.eps)

    qvalue <- as.numeric(stats::quantile(work, probs=prob, names=FALSE,
                                         type=8, na.rm=TRUE))
    threshold <- max(qvalue, centre + z * spread)
    largest <- which.max(work)

    if (work[largest] <= threshold || length(work) - 1L < min.samples)
      break

    work <- work[-largest]
  }

  expm1(threshold)

} # '.precipQC_upper_threshold' END


.precipQC_haversine <- function(lon, lat) {

  n <- length(lon)
  out <- matrix(NA_real_, nrow=n, ncol=n)
  rad <- pi / 180

  for (i in seq_len(n)) {
    if (!is.finite(lon[i]) || !is.finite(lat[i])) next
    ok <- is.finite(lon) & is.finite(lat) & seq_len(n) != i
    dlon <- (lon[ok] - lon[i]) * rad
    dlat <- (lat[ok] - lat[i]) * rad
    a <- sin(dlat / 2)^2 + cos(lat[i] * rad) *
         cos(lat[ok] * rad) * sin(dlon / 2)^2
    out[i, ok] <- 6371 * 2 * asin(pmin(1, sqrt(a)))
  }

  diag(out) <- 0
  out

} # '.precipQC_haversine' END


.precipQC_metadata <- function(stations, metadata, station.id, coords,
                               elevation=NULL) {

  supplied <- !is.null(metadata)
  if (is.null(metadata)) {
    metadata <- data.frame(station=stations, stringsAsFactors=FALSE)
    station.id <- "station"
    elevation <- NULL
  }

  if (!is.data.frame(metadata))
    stop("Invalid argument: 'metadata' must be a data.frame or NULL !")

  if (!is.character(station.id) || length(station.id) != 1L ||
      !(station.id %in% names(metadata)))
    stop("Invalid argument: 'station.id' must name a column in 'metadata' !")

  ids <- as.character(metadata[[station.id]])
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))
    stop("Invalid argument: station identifiers in 'metadata' must be unique and non-missing !")

  rows <- match(stations, ids)
  if (anyNA(rows))
    stop("Invalid argument: 'metadata' does not contain every station in 'x' !")

  metadata <- metadata[rows, , drop=FALSE]
  rownames(metadata) <- NULL

  has.coords <- FALSE
  has.elevation <- FALSE
  coordinate.available <- rep(FALSE, length(stations))
  if (supplied) {
    if (!is.character(coords) || length(coords) != 2L || anyNA(coords) ||
        any(!nzchar(coords)) || anyDuplicated(coords) ||
        !all(coords %in% names(metadata)))
      stop("Invalid argument: 'coords' must name the longitude and latitude columns in 'metadata' !")
    if (!all(vapply(metadata[coords], is.numeric, logical(1))))
      stop("Invalid argument: longitude and latitude metadata columns must be numeric !")

    lon <- metadata[[coords[1L]]]
    lat <- metadata[[coords[2L]]]
    invalid.lon <- !is.na(lon) &
                   (!is.finite(lon) | lon < -180 | lon > 180)
    invalid.lat <- !is.na(lat) &
                   (!is.finite(lat) | lat < -90 | lat > 90)
    if (any(invalid.lon))
      stop("Invalid argument: non-missing metadata longitudes must be finite and in [-180, 180] !")
    if (any(invalid.lat))
      stop("Invalid argument: non-missing metadata latitudes must be finite and in [-90, 90] !")
    coordinate.available <- is.finite(lon) & is.finite(lat)
    has.coords <- any(coordinate.available)

    if (!is.null(elevation)) {
      if (!is.character(elevation) || length(elevation) != 1L ||
          is.na(elevation) || !nzchar(elevation) ||
          !(elevation %in% names(metadata)))
        stop("Invalid argument: 'elevation' must name an elevation column in 'metadata' or be NULL !")
      if (!is.numeric(metadata[[elevation]]) ||
          any(!is.finite(metadata[[elevation]])))
        stop("Invalid argument: the elevation metadata column must contain finite numeric values !")
      has.elevation <- TRUE
    }
  }

  list(metadata=metadata, station.id=station.id, coords=coords,
       elevation=elevation, has.coords=has.coords,
       has.elevation=has.elevation,
       coordinate.available=coordinate.available)

} # '.precipQC_metadata' END


.precipQC_neighbours <- function(values, metadata=NULL, station.id="station",
                                  coords=c("lon", "lat"), n.neighbours=10L,
                                  max.distance=Inf, min.overlap=30L,
                                  min.correlation=0, elevation=NULL,
                                  elevation.scale=500) {

  stations <- colnames(values)
  nstations <- NCOL(values)
  n.neighbours <- .precipQC_check_positive_integer(n.neighbours,
                                                    "n.neighbours")
  min.overlap <- .precipQC_check_positive_integer(min.overlap,
                                                   "min.overlap")

  if (!is.numeric(max.distance) || length(max.distance) != 1L ||
      is.na(max.distance) || max.distance <= 0)
    stop("Invalid argument: 'max.distance' must be a positive number !")

  if (!is.numeric(min.correlation) || length(min.correlation) != 1L ||
      is.na(min.correlation) || min.correlation < -1 || min.correlation > 1)
    stop("Invalid argument: 'min.correlation' must be in [-1, 1] !")
  if (!is.numeric(elevation.scale) || length(elevation.scale) != 1L ||
      is.na(elevation.scale) || !is.finite(elevation.scale) ||
      elevation.scale <= 0)
    stop("Invalid argument: 'elevation.scale' must be a positive finite number !")

  meta <- .precipQC_metadata(stations, metadata, station.id, coords,
                             elevation)
  distances <- matrix(1, nrow=nstations, ncol=nstations,
                      dimnames=list(stations, stations))

  if (meta$has.coords) {
    distances <- .precipQC_haversine(
      as.numeric(meta$metadata[[coords[1L]]]),
      as.numeric(meta$metadata[[coords[2L]]])
    )
    dimnames(distances) <- list(stations, stations)
  } else {
    diag(distances) <- 0
  }
  elevation.similarity <- matrix(1, nrow=nstations, ncol=nstations,
                                 dimnames=list(stations, stations))
  if (meta$has.elevation) {
    heights <- as.numeric(meta$metadata[[meta$elevation]])
    height.difference <- abs(outer(heights, heights, "-"))
    elevation.similarity <- exp(-height.difference / elevation.scale)
  }
  effective.distance <- pmax(distances, 0.1) /
                        pmax(elevation.similarity,
                             .Machine$double.xmin)
  diag(effective.distance) <- 0

  out <- vector("list", nstations)
  names(out) <- stations

  for (j in seq_len(nstations)) {
    if (meta$has.coords) {
      candidates <- which(seq_len(nstations) != j &
                          (!is.finite(distances[j, ]) |
                           distances[j, ] <= max.distance))
    } else {
      candidates <- which(seq_len(nstations) != j)
    }
    if (length(candidates) == 0L) {
      out[[j]] <- integer()
      next
    }

    correlations <- rep(NA_real_, length(candidates))
    overlaps <- integer(length(candidates))
    for (k in seq_along(candidates)) {
      z <- candidates[k]
      ok <- is.finite(values[, j]) & is.finite(values[, z])
      overlaps[k] <- sum(ok)
      if (overlaps[k] >= min.overlap)
        correlations[k] <- suppressWarnings(stats::cor(
          log1p(pmax(values[ok, j], 0)), log1p(pmax(values[ok, z], 0)),
          method="spearman"
        ))
    }

    eligible <- overlaps >= min.overlap & is.finite(correlations)
    preferred <- eligible & correlations >= min.correlation

    if (meta$coordinate.available[j]) {
      known.distance <- is.finite(distances[j, candidates])
      rank.distance <- ifelse(known.distance,
                              effective.distance[j, candidates], Inf)
      ordered <- order(!known.distance, rank.distance, -correlations,
                       na.last=TRUE)
    } else {
      ordered <- order(-correlations,
                       -elevation.similarity[j, candidates], na.last=TRUE)
    }

    selected <- candidates[ordered][preferred[ordered]]
    if (length(selected) < 2L)
      selected <- candidates[ordered][eligible[ordered]]

    out[[j]] <- utils::head(selected, n.neighbours)
  }

  list(index=out, distance=distances,
       elevation.similarity=elevation.similarity,
       metadata=meta$metadata, has.coords=meta$has.coords,
       has.elevation=meta$has.elevation,
       coordinate.available=meta$coordinate.available)

} # '.precipQC_neighbours' END


.precipQC_wet_day_mean <- function(x, wet.threshold=0.1) {

  values <- .precipQC_matrix(x)
  dates <- as.Date(zoo::index(x))
  out <- rep(NA_real_, NCOL(values))

  for (j in seq_len(NCOL(values))) {
    totals <- tapply(values[, j], dates, function(z) {
      if (all(is.na(z))) NA_real_ else sum(z, na.rm=TRUE)
    })
    wet <- totals[is.finite(totals) & totals >= wet.threshold]
    if (length(wet) > 0L)
      out[j] <- mean(wet)
  }

  names(out) <- colnames(values)
  out

} # '.precipQC_wet_day_mean' END


precipQC_range <- function(x, lower=0, upper=Inf) {

  values <- .precipQC_matrix(x)

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

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values)))
    flags[, j] <- is.finite(values[, j]) &
                  (values[, j] < lower[j] | values[, j] > upper[j])

  .precipQC_zoo(flags, x)

} # 'precipQC_range' END


precipQC_persistence <- function(x, high.threshold=10, high.run=5L,
                                 long.run=Inf, wet.threshold=0.1) {

  values <- .precipQC_matrix(x)
  high.run <- .precipQC_check_positive_integer(high.run, "high.run")

  if (!is.numeric(high.threshold) ||
      !(length(high.threshold) %in% c(1L, NCOL(values))) ||
      anyNA(high.threshold) || any(high.threshold < 0))
    stop("Invalid argument: 'high.threshold' must contain non-negative values !")
  high.threshold <- rep(high.threshold, length.out=NCOL(values))

  if (!is.numeric(long.run) || length(long.run) != 1L || is.na(long.run) ||
      long.run < 1)
    stop("Invalid argument: 'long.run' must be a positive number !")
  if (is.finite(long.run))
    long.run <- .precipQC_check_positive_integer(long.run, "long.run")

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  for (j in seq_len(NCOL(values))) {
    encoded <- ifelse(is.na(values[, j]), NA_character_,
                      format(values[, j], digits=15, trim=TRUE,
                             scientific=FALSE))
    runs <- .precipQC_runs(encoded)
    for (k in seq_along(runs$lengths)) {
      if (is.na(runs$values[k])) next
      value <- values[runs$starts[k], j]
      high <- runs$lengths[k] >= high.run &&
              is.finite(value) && value >= high.threshold[j]
      long <- is.finite(long.run) && runs$lengths[k] >= long.run &&
              is.finite(value) && value >= wet.threshold
      if (high || long)
        flags[runs$starts[k]:runs$ends[k], j] <- TRUE
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_persistence' END


.QC_duplicate_blocks <- function(x, min.month.values=20L,
                                 min.year.values=300L,
                                 min.nonzero=0L, zero.threshold=0) {

  values <- .precipQC_matrix(x)
  dates <- as.Date(zoo::index(x))
  if (anyDuplicated(dates))
    stop("Invalid argument: 'x' must contain at most one observation per day !")

  min.month.values <- .precipQC_check_positive_integer(
    min.month.values, "min.month.values")
  min.year.values <- .precipQC_check_positive_integer(
    min.year.values, "min.year.values")
  if (!is.numeric(min.nonzero) || length(min.nonzero) != 1L ||
      is.na(min.nonzero) || !is.finite(min.nonzero) || min.nonzero < 0 ||
      abs(min.nonzero - round(min.nonzero)) > sqrt(.Machine$double.eps))
    stop("Invalid argument: 'min.nonzero' must be a non-negative integer !")
  min.nonzero <- as.integer(round(min.nonzero))
  if (!is.numeric(zero.threshold) || length(zero.threshold) != 1L ||
      is.na(zero.threshold) || !is.finite(zero.threshold))
    stop("Invalid argument: 'zero.threshold' must be a finite number !")

  years <- format(dates, "%Y")
  months <- format(dates, "%Y-%m")
  month.number <- format(dates, "%m")
  day <- format(dates, "%d")
  month.groups <- split(seq_along(dates), months)
  year.groups <- split(seq_along(dates), years)
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  compare <- function(a, b, keys, minimum, column) {
    common <- intersect(keys[a], keys[b])
    if (length(common) < minimum) return(FALSE)
    ia <- a[match(common, keys[a])]
    ib <- b[match(common, keys[b])]
    za <- values[ia, column]
    zb <- values[ib, column]
    if (any(!is.finite(za)) || any(!is.finite(zb)) ||
        !all(za == zb)) return(FALSE)
    if (min.nonzero > 0L &&
        (sum(za > zero.threshold) < min.nonzero ||
         sum(zb > zero.threshold) < min.nonzero)) return(FALSE)
    TRUE
  }

  for (j in seq_len(NCOL(values))) {
    if (length(year.groups) > 1L) {
      pairs <- utils::combn(seq_along(year.groups), 2L)
      for (k in seq_len(NCOL(pairs))) {
        a <- year.groups[[pairs[1L, k]]]
        b <- year.groups[[pairs[2L, k]]]
        if (compare(a, b, format(dates, "%m-%d"), min.year.values, j))
          flags[c(a, b), j] <- is.finite(values[c(a, b), j])
      }
    }

    if (length(month.groups) > 1L) {
      pairs <- utils::combn(seq_along(month.groups), 2L)
      for (k in seq_len(NCOL(pairs))) {
        a <- month.groups[[pairs[1L, k]]]
        b <- month.groups[[pairs[2L, k]]]
        same.year <- years[a[1L]] == years[b[1L]]
        same.month <- month.number[a[1L]] == month.number[b[1L]]
        if (!(same.year || same.month)) next
        if (compare(a, b, day, min.month.values, j))
          flags[c(a, b), j] <- is.finite(values[c(a, b), j])
      }
    }
  }

  .precipQC_zoo(flags, x)

} # '.QC_duplicate_blocks' END


precipQC_duplicate <- function(x, min.month.values=20L,
                               min.year.values=300L,
                               min.nonzero=3L, wet.threshold=0.1) {

  .QC_duplicate_blocks(
    x, min.month.values=min.month.values,
    min.year.values=min.year.values, min.nonzero=min.nonzero,
    zero.threshold=wet.threshold
  )

} # 'precipQC_duplicate' END


precipQC_gap <- function(x, gap=300, min.samples=30L,
                         wet.threshold=0.1) {

  values <- .precipQC_matrix(x)
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  if (!is.numeric(gap) || length(gap) != 1L || is.na(gap) ||
      !is.finite(gap) || gap <= 0)
    stop("Invalid argument: 'gap' must be a positive number !")

  months <- as.integer(format(as.Date(zoo::index(x)), "%m"))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    for (month in seq_len(12L)) {
      rows <- which(months == month & is.finite(values[, j]) &
                    values[, j] >= wet.threshold)
      if (length(rows) < min.samples) next
      ordered <- sort(values[rows, j])
      gaps <- which(diff(ordered) >= gap)
      if (length(gaps) > 0L) {
        threshold <- ordered[gaps[1L] + 1L]
        flags[rows[values[rows, j] >= threshold], j] <- TRUE
      }
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_gap' END


precipQC_frequency <- function(x, window=10L,
                               counts=c(9L, 8L, 7L, 5L),
                               probs=c(0.3, 0.5, 0.7, 0.9),
                               min.samples=20L, wet.threshold=0.1) {

  values <- .precipQC_matrix(x)
  window <- .precipQC_check_positive_integer(window, "window")
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")
  if (!is.numeric(counts) || !is.numeric(probs) ||
      length(counts) != length(probs) || length(counts) == 0L ||
      anyNA(counts) || anyNA(probs) || any(counts < 2) ||
      any(counts > window) || any(probs <= 0 | probs >= 1))
    stop("Invalid arguments: 'counts' and 'probs' must define valid repetition rules !")
  counts <- as.integer(round(counts))
  order.rules <- order(counts, decreasing=TRUE)
  counts <- counts[order.rules]
  probs <- probs[order.rules]

  months <- as.integer(format(as.Date(zoo::index(x)), "%m"))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values))) {
    wet <- which(is.finite(values[, j]) & values[, j] >= wet.threshold)
    if (length(wet) < window) next
    for (end in seq.int(window, length(wet))) {
      rows <- wet[(end - window + 1L):end]
      tab <- table(format(values[rows, j], digits=15, trim=TRUE,
                          scientific=FALSE))
      repeated <- names(tab)[tab >= min(counts)]
      if (length(repeated) == 0L) next
      for (label in repeated) {
        count <- unname(tab[label])
        eligible <- which(count >= counts)
        if (length(eligible) == 0L) next
        probability <- probs[eligible[1L]]
        candidate <- rows[format(values[rows, j], digits=15, trim=TRUE,
                                 scientific=FALSE) == label]
        thresholds <- vapply(candidate, function(i) {
          reference <- values[months == months[i], j]
          reference <- reference[is.finite(reference) &
                                 reference >= wet.threshold]
          if (length(reference) < min.samples) return(NA_real_)
          as.numeric(stats::quantile(reference, probability,
                                     names=FALSE, type=8))
        }, numeric(1))
        if (all(is.finite(thresholds)) &&
            all(values[candidate, j] >= thresholds))
          flags[candidate, j] <- TRUE
      }
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_frequency' END


precipQC_weekday <- function(x, wet.threshold=0.1, min.wet.days=20L,
                             alpha=0.001, underreporting.ratio=0.5) {

  values <- .precipQC_matrix(x)
  min.wet.days <- .precipQC_check_positive_integer(min.wet.days,
                                                    "min.wet.days")
  alpha <- .precipQC_check_probability(alpha, "alpha")
  if (!is.numeric(underreporting.ratio) ||
      length(underreporting.ratio) != 1L ||
      is.na(underreporting.ratio) || !is.finite(underreporting.ratio) ||
      underreporting.ratio <= 0 || underreporting.ratio >= 1)
    stop("Invalid argument: 'underreporting.ratio' must be in (0, 1) !")

  dates <- as.Date(zoo::index(x))
  if (anyDuplicated(dates))
    stop("Invalid argument: 'x' must contain at most one observation per day !")

  years <- format(dates, "%Y")
  weekdays <- as.POSIXlt(dates)$wday + 1L
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  for (j in seq_len(NCOL(values))) {
    for (year in unique(years)) {
      rows <- which(years == year & is.finite(values[, j]))
      if (length(rows) == 0L) next

      exposure <- tabulate(weekdays[rows], nbins=7L)
      wet <- rows[values[rows, j] >= wet.threshold]
      if (length(wet) < min.wet.days || any(exposure == 0L)) next

      observed <- tabulate(weekdays[wet], nbins=7L)
      expected <- length(wet) * exposure / sum(exposure)
      statistic <- sum((observed - expected)^2 / expected)
      p.value <- stats::pchisq(statistic, df=6L, lower.tail=FALSE)
      underreported <- observed / expected <= underreporting.ratio

      if (is.finite(p.value) && p.value < alpha && any(underreported)) {
        suspect <- rows[underreported[weekdays[rows]] &
                        values[rows, j] < wet.threshold]
        flags[suspect, j] <- TRUE
      }
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_weekday' END


precipQC_climatology <- function(x, group=c("dayofyear", "month"),
                                 window=15L, prob=0.999, z=8,
                                 min.samples=30L, wet.threshold=0.1) {

  values <- .precipQC_matrix(x)
  group <- match.arg(group)
  window <- .precipQC_check_positive_integer(window, "window")
  prob <- .precipQC_check_probability(prob, "prob")
  min.samples <- .precipQC_check_positive_integer(min.samples,
                                                   "min.samples")

  if (!is.numeric(z) || length(z) != 1L || is.na(z) || !is.finite(z) ||
      z <= 0)
    stop("Invalid argument: 'z' must be a positive number !")

  dates <- as.Date(zoo::index(x))
  groups <- if (group == "month") {
    as.integer(format(dates, "%m"))
  } else {
    as.integer(format(dates, "%j"))
  }
  ngroups <- if (group == "month") 12L else 366L
  half.window <- floor(window / 2)

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  for (j in seq_len(NCOL(values))) {
    for (g in sort(unique(groups))) {
      target <- which(groups == g & is.finite(values[, j]) &
                      values[, j] >= wet.threshold)
      if (length(target) == 0L) next

      if (group == "month") {
        reference <- which(groups == g)
      } else {
        distance <- abs(groups - g)
        distance <- pmin(distance, ngroups - distance)
        reference <- which(distance <= half.window)
      }

      threshold <- .precipQC_upper_threshold(values[reference, j], prob, z,
                                              min.samples)
      if (is.finite(threshold))
        flags[target, j] <- values[target, j] > threshold
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_climatology' END


precipQC_accumulation <- function(x, interval.hours,
                                  preceding.hours=23,
                                  following.hours=23,
                                  threshold.factor=2,
                                  wet.threshold=0.1,
                                  missing.only=FALSE) {

  values <- .precipQC_matrix(x)
  if (!is.numeric(interval.hours) || length(interval.hours) != 1L ||
      is.na(interval.hours) || !is.finite(interval.hours) ||
      interval.hours <= 0)
    stop("Invalid argument: 'interval.hours' must be a positive number !")

  for (arg in c("preceding.hours", "following.hours", "threshold.factor")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0)
      stop("Invalid argument: '", arg, "' must be a non-negative number !")
  }

  if (!is.logical(missing.only) || length(missing.only) != 1L ||
      is.na(missing.only))
    stop("Invalid argument: 'missing.only' must be a logical value !")

  n.pre <- max(1L, ceiling(preceding.hours / interval.hours))
  n.post <- ceiling(following.hours / interval.hours)
  wet.day.mean <- .precipQC_wet_day_mean(x, wet.threshold)
  thresholds <- threshold.factor * wet.day.mean
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  datetimes <- zoo::index(x)
  gaps <- rep(FALSE, NROW(values))
  if (NROW(values) > 1L) {
    delta <- if (inherits(datetimes, "Date")) {
      as.numeric(diff(datetimes)) * 24
    } else {
      as.numeric(diff(as.POSIXct(datetimes)), units="hours")
    }
    gaps[-1L] <- delta > 1.5 * interval.hours
  }

  for (j in seq_len(NCOL(values))) {
    if (!is.finite(thresholds[j])) next
    candidates <- which(is.finite(values[, j]) &
                        values[, j] > thresholds[j])
    for (i in candidates) {
      if (i <= n.pre) next
      previous <- values[(i - n.pre):(i - 1L), j]
      if (missing.only) {
        previous.ok <- all(is.na(previous)) || gaps[i]
      } else {
        previous.ok <- all(is.na(previous) | previous <= wet.threshold) &&
                       any(is.finite(previous))
      }

      following.ok <- TRUE
      if (n.post > 0L) {
        if (i + n.post > NROW(values)) next
        following <- values[(i + 1L):(i + n.post), j]
        following.ok <- all(is.na(following) |
                            following <= wet.threshold) &&
                        any(is.finite(following))
      }

      if (previous.ok && following.ok)
        flags[i, j] <- TRUE
    }
  }

  .precipQC_zoo(flags, x)

} # 'precipQC_accumulation' END


precipQC_spatial <- function(x, metadata=NULL, station.id="station",
                             coords=c("lon", "lat"), n.neighbours=10L,
                             max.distance=Inf, min.neighbours=2L,
                             min.overlap=30L, min.correlation=0,
                             cr.threshold=3, wet.threshold=0.1,
                             elevation=NULL, elevation.scale=500) {

  values <- .precipQC_matrix(x)
  min.neighbours <- .precipQC_check_positive_integer(min.neighbours,
                                                      "min.neighbours")
  if (!is.numeric(cr.threshold) || length(cr.threshold) != 1L ||
      is.na(cr.threshold) || !is.finite(cr.threshold) || cr.threshold <= 0)
    stop("Invalid argument: 'cr.threshold' must be a positive number !")

  neighbours <- .precipQC_neighbours(
    values=values, metadata=metadata, station.id=station.id, coords=coords,
    n.neighbours=n.neighbours, max.distance=max.distance,
    min.overlap=min.overlap, min.correlation=min.correlation,
    elevation=elevation, elevation.scale=elevation.scale
  )

  transformed <- log1p(pmax(values, 0))
  estimate.t <- matrix(NA_real_, nrow=NROW(values), ncol=NCOL(values),
                       dimnames=dimnames(values))
  local.scale <- estimate.t

  for (j in seq_len(NCOL(values))) {
    nn <- neighbours$index[[j]]
    if (length(nn) < min.neighbours) next

    for (i in seq_len(NROW(values))) {
      ok <- nn[is.finite(transformed[i, nn])]
      if (length(ok) < min.neighbours) next

      y <- transformed[i, ok]
      if (neighbours$coordinate.available[j] ||
          neighbours$has.elevation) {
        d <- neighbours$distance[j, ok]
        known.distance <- is.finite(d)
        if (any(known.distance)) {
          fallback <- if (is.finite(max.distance)) max.distance else
                      2 * max(d[known.distance], 0.1)
          d[!known.distance] <- max(fallback,
                                    max(d[known.distance], 0.1))
        } else {
          d[] <- 1
        }
        w <- neighbours$elevation.similarity[j, ok] /
             pmax(d, 0.1)^2
        ordered <- order(y)
        y.ordered <- y[ordered]
        w.ordered <- w[ordered] / sum(w)
        estimate.t[i, j] <- y.ordered[
          which(cumsum(w.ordered) >= 0.5)[1L]
        ]
      } else {
        estimate.t[i, j] <- stats::median(y)
      }
      local.scale[i, j] <- stats::mad(y, constant=1.4826)
    }
  }

  scores <- matrix(NA_real_, nrow=NROW(values), ncol=NCOL(values),
                   dimnames=dimnames(values))
  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))

  for (j in seq_len(NCOL(values))) {
    residual <- transformed[, j] - estimate.t[, j]
    historical.scale <- stats::mad(residual, na.rm=TRUE, constant=1.4826)
    if (!is.finite(historical.scale) ||
        historical.scale <= sqrt(.Machine$double.eps))
      historical.scale <- stats::IQR(residual, na.rm=TRUE) / 1.349
    if (!is.finite(historical.scale) ||
        historical.scale <= sqrt(.Machine$double.eps))
      historical.scale <- 0.15

    denominator <- pmax(local.scale[, j], historical.scale, 0.15,
                        na.rm=TRUE)
    scores[, j] <- abs(residual) / denominator
    flags[, j] <- is.finite(values[, j]) & is.finite(estimate.t[, j]) &
                   values[, j] >= wet.threshold & residual > 0 &
                   scores[, j] >= cr.threshold
  }

  out <- list(flags=.precipQC_zoo(flags, x),
              scores=.precipQC_zoo(scores, x),
              estimate=.precipQC_zoo(expm1(estimate.t), x),
              neighbours=lapply(neighbours$index,
                                function(i) colnames(values)[i]))
  class(out) <- c("precipQC_spatial", "list")
  out

} # 'precipQC_spatial' END


precipQC_dryspell <- function(x, metadata=NULL, station.id="station",
                              coords=c("lon", "lat"), n.neighbours=10L,
                              max.distance=Inf, window.days=15L,
                              neighbour.wet.days=3L,
                              neighbour.fraction=1,
                              wet.threshold=0.1, elevation=NULL,
                              elevation.scale=500) {

  values <- .precipQC_matrix(x)
  window.days <- .precipQC_check_positive_integer(window.days,
                                                   "window.days")
  neighbour.wet.days <- .precipQC_check_positive_integer(
    neighbour.wet.days, "neighbour.wet.days"
  )
  if (!is.numeric(neighbour.fraction) || length(neighbour.fraction) != 1L ||
      is.na(neighbour.fraction) || neighbour.fraction <= 0 ||
      neighbour.fraction > 1)
    stop("Invalid argument: 'neighbour.fraction' must be in (0, 1] !")

  neighbours <- .precipQC_neighbours(
    values=values, metadata=metadata, station.id=station.id, coords=coords,
    n.neighbours=n.neighbours, max.distance=max.distance,
    min.overlap=max(2L, window.days), min.correlation=-1,
    elevation=elevation, elevation.scale=elevation.scale
  )

  dates <- as.Date(zoo::index(x))
  all.dates <- seq(min(dates), max(dates), by="day")
  day.index <- match(dates, all.dates)
  daily.wet <- matrix(FALSE, nrow=length(all.dates), ncol=NCOL(values),
                      dimnames=list(as.character(all.dates), colnames(values)))
  daily.present <- daily.wet

  for (j in seq_len(NCOL(values))) {
    daily.wet[, j] <- vapply(seq_along(all.dates), function(i) {
      z <- values[day.index == i, j]
      any(is.finite(z) & z >= wet.threshold)
    }, logical(1))
    daily.present[, j] <- vapply(seq_along(all.dates), function(i) {
      any(is.finite(values[day.index == i, j]))
    }, logical(1))
  }

  flagged.days <- matrix(FALSE, nrow=length(all.dates), ncol=NCOL(values),
                         dimnames=dimnames(daily.wet))

  for (j in seq_len(NCOL(values))) {
    nn <- neighbours$index[[j]]
    if (length(nn) < 1L) next
    dry <- daily.present[, j] & !daily.wet[, j]
    runs <- .precipQC_runs(dry)
    candidate.runs <- which(runs$values & runs$lengths >= window.days)

    for (k in candidate.runs) {
      starts <- seq.int(runs$starts[k],
                        runs$ends[k] - window.days + 1L)
      for (start in starts) {
        interval <- start:(start + window.days - 1L)
        wet.count <- colSums(daily.wet[interval, nn, drop=FALSE])
        corroborated <- mean(wet.count >= neighbour.wet.days)
        if (is.finite(corroborated) &&
            corroborated >= neighbour.fraction)
          flagged.days[interval, j] <- TRUE
      }
    }
  }

  flags <- matrix(FALSE, nrow=NROW(values), ncol=NCOL(values),
                  dimnames=dimnames(values))
  for (j in seq_len(NCOL(values)))
    flags[, j] <- flagged.days[day.index, j] &
                  is.finite(values[, j]) & values[, j] < wet.threshold

  .precipQC_zoo(flags, x)

} # 'precipQC_dryspell' END


.precipQC_pettitt <- function(x) {

  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2L)
    return(c(split=NA_real_, p.value=NA_real_, relative.change=NA_real_))

  ranks <- rank(x)
  u <- 2 * cumsum(ranks) - seq_len(n) * (n + 1)
  split <- unname(which.max(abs(u[-n])))
  k <- abs(u[split])
  p.value <- min(1, 2 * exp((-6 * k^2) / (n^3 + n^2)))
  before <- stats::median(x[seq_len(split)])
  after <- stats::median(x[(split + 1L):n])
  relative <- if (abs(before) <= sqrt(.Machine$double.eps)) {
    if (abs(after) <= sqrt(.Machine$double.eps)) 0 else Inf
  } else {
    abs(after - before) / abs(before)
  }

  c(split=split, p.value=p.value, relative.change=relative)

} # '.precipQC_pettitt' END


precipQC_breakpoint <- function(
  x, wet.threshold=0.1, min.years=5L, alpha=0.05,
  min.relative.change=0.5, min.completeness=0.8,
  indicators=c("total", "wet.days", "maximum", "extreme.days")) {

  values <- .precipQC_matrix(x)
  min.years <- .precipQC_check_positive_integer(min.years, "min.years")
  alpha <- .precipQC_check_probability(alpha, "alpha")
  if (!is.numeric(min.completeness) || length(min.completeness) != 1L ||
      is.na(min.completeness) || !is.finite(min.completeness) ||
      min.completeness <= 0 || min.completeness > 1)
    stop("Invalid argument: 'min.completeness' must be in (0, 1] !")

  allowed <- c("total", "wet.days", "maximum", "extreme.days")
  if (!is.character(indicators) || length(indicators) == 0L ||
      anyNA(indicators) || any(!indicators %in% allowed) ||
      anyDuplicated(indicators))
    stop("Invalid argument: 'indicators' must contain unique values from ",
         paste(allowed, collapse=", "), " !")

  dates <- as.Date(zoo::index(x))
  daily.dates <- sort(unique(dates))
  daily.values <- matrix(NA_real_, nrow=length(daily.dates),
                         ncol=NCOL(values),
                         dimnames=list(NULL, colnames(values)))
  day <- match(dates, daily.dates)
  for (j in seq_len(NCOL(values))) {
    daily.values[, j] <- vapply(seq_along(daily.dates), function(i) {
      z <- values[day == i, j]
      if (all(!is.finite(z))) NA_real_ else sum(z[is.finite(z)])
    }, numeric(1))
  }
  values <- daily.values
  dates <- daily.dates
  years <- as.integer(format(dates, "%Y"))
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
    observed <- vapply(year.levels, function(year)
      sum(is.finite(values[years == year, j])), integer(1))
    complete <- observed / expected >= min.completeness
    wet.values <- values[is.finite(values[, j]) &
                         values[, j] >= wet.threshold, j]
    p99 <- if (length(wet.values) > 0L) {
      as.numeric(stats::quantile(wet.values, 0.99, names=FALSE, type=8))
    } else {
      NA_real_
    }

    annual <- lapply(indicators, function(indicator) {
      z <- vapply(year.levels, function(year) {
        y <- values[years == year, j]
        y <- y[is.finite(y)]
        if (length(y) == 0L) return(NA_real_)
        switch(indicator,
          total=sum(y),
          wet.days=sum(y >= wet.threshold),
          maximum=max(y),
          extreme.days=if (is.finite(p99)) sum(y > p99) else NA_real_
        )
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
      list(year=as.integer(names(z)[as.integer(test["split"])]),
           p.value=unname(test["p.value"]),
           relative.change=unname(test["relative.change"]))
    })
    valid <- !vapply(diagnostics, is.null, logical(1))
    if (!any(valid)) next

    diagnostics <- diagnostics[valid]
    p.values <- vapply(diagnostics, `[[`, numeric(1), "p.value")
    adjusted <- stats::p.adjust(p.values, method="holm")
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

} # 'precipQC_breakpoint' END
