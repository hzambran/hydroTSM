# File QC_spatial.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Shared scalable helpers for station-based spatial quality control             #
################################################################################

.QC_spatial_limit <- function(x, name, minimum=1L) {

  # Accept infinity as the explicit exhaustive-selection request.
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < minimum ||
      (is.finite(x) && abs(x - round(x)) > sqrt(.Machine$double.eps)))
    stop("Invalid argument: '", name, "' must be an integer >= ",
         minimum, " or Inf !")

  if (is.finite(x)) as.integer(round(x)) else Inf

} # '.QC_spatial_limit' END


.QC_spatial_description <- function(max.candidates, selection.samples) {

  # Describe effective screening bounds in interactive and batch logs.
  max.candidates <- .QC_spatial_limit(max.candidates, "max.candidates")
  selection.samples <- .QC_spatial_limit(selection.samples,
                                          "selection.samples", 2L)
  candidates <- if (is.finite(max.candidates)) max.candidates else "all"
  samples <- if (is.finite(selection.samples)) selection.samples else "all"
  paste0("Spatial screening uses up to ", candidates,
         " candidate(s) and ", samples, " timestamp(s) per target.")

} # '.QC_spatial_description' END


.QC_spatial_rows <- function(n, selection.samples) {

  # Retain all rows for short records and an evenly spaced sample otherwise.
  selection.samples <- .QC_spatial_limit(selection.samples,
                                          "selection.samples", 2L)
  if (!is.finite(selection.samples) || n <= selection.samples)
    return(seq_len(n))

  unique(as.integer(round(seq(1, n, length.out=selection.samples))))

} # '.QC_spatial_rows' END


.QC_spatial_screen <- function(values, method=c("pearson", "spearman"),
                               max.rows=64L) {

  # Build a deterministic low-cost station similarity matrix for screening.
  method <- match.arg(method)
  rows <- .QC_spatial_rows(NROW(values), max.rows)
  work <- values[rows, , drop=FALSE]
  if (method == "spearman") {
    work <- log1p(pmax(work, 0))
    for (j in seq_len(NCOL(work))) {
      ok <- is.finite(work[, j])
      work[, j] <- NA_real_
      if (sum(ok) >= 2L)
        work[ok, j] <- rank(work[ok, j], ties.method="average")
    }
  }

  # Centre, scale, and mean-impute as zero on the standardized scale.
  for (j in seq_len(NCOL(work))) {
    ok <- is.finite(work[, j])
    if (sum(ok) < 2L) {
      work[, j] <- 0
      next
    }
    centre <- mean(work[ok, j])
    work[ok, j] <- work[ok, j] - centre
    scale <- sqrt(sum(work[ok, j]^2))
    if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
      work[, j] <- 0
    } else {
      work[ok, j] <- work[ok, j] / scale
      work[!ok, j] <- 0
    }
  }

  # Cross-products provide a fast approximate correlation ranking.
  out <- crossprod(work)
  diag(out) <- -Inf
  out

} # '.QC_spatial_screen' END


.QC_haversine_target <- function(lon, lat, target) {

  # Calculate one target-to-network distance vector without an n-by-n matrix.
  out <- rep(NA_real_, length(lon))
  if (!is.finite(lon[target]) || !is.finite(lat[target]))
    return(out)

  ok <- is.finite(lon) & is.finite(lat)
  rad <- pi / 180
  dlon <- (lon[ok] - lon[target]) * rad
  dlat <- (lat[ok] - lat[target]) * rad
  a <- sin(dlat / 2)^2 + cos(lat[target] * rad) *
       cos(lat[ok] * rad) * sin(dlon / 2)^2
  out[ok] <- 6371 * 2 * asin(pmin(1, sqrt(a)))
  out[target] <- 0
  out

} # '.QC_haversine_target' END


.QC_spatial_context <- function(values, metadata=NULL, station.id="station",
                                coords=c("lon", "lat"), max.distance=Inf,
                                n.neighbours=10L, max.candidates=200L,
                                elevation=NULL, elevation.scale=500,
                                screen.method=c("pearson", "spearman")) {

  # Validate shared controls before constructing reusable candidate lists.
  screen.method <- match.arg(screen.method)
  n.neighbours <- .precipQC_check_positive_integer(n.neighbours,
                                                    "n.neighbours")
  max.candidates <- .QC_spatial_limit(max.candidates, "max.candidates",
                                      n.neighbours)
  if (!is.numeric(max.distance) || length(max.distance) != 1L ||
      is.na(max.distance) || max.distance <= 0)
    stop("Invalid argument: 'max.distance' must be a positive number !")
  if (!is.numeric(elevation.scale) || length(elevation.scale) != 1L ||
      is.na(elevation.scale) || !is.finite(elevation.scale) ||
      elevation.scale <= 0)
    stop("Invalid argument: 'elevation.scale' must be a positive finite number !")

  # Align metadata once and retain only compact target-specific pair data.
  stations <- colnames(values)
  nstations <- NCOL(values)
  meta <- .precipQC_metadata(stations, metadata, station.id, coords,
                             elevation)
  lon <- lat <- rep(NA_real_, nstations)
  if (meta$has.coords) {
    lon <- as.numeric(meta$metadata[[coords[1L]]])
    lat <- as.numeric(meta$metadata[[coords[2L]]])
  }
  heights <- if (meta$has.elevation)
    as.numeric(meta$metadata[[meta$elevation]]) else rep(0, nstations)

  candidates <- distances <- similarities <- vector("list", nstations)
  needs.screen <- FALSE
  raw <- vector("list", nstations)

  # Apply the exact geographic filter before statistical screening.
  for (j in seq_len(nstations)) {
    distance <- if (meta$has.coords)
      .QC_haversine_target(lon, lat, j) else rep(1, nstations)
    candidate <- which(seq_len(nstations) != j &
                       (!is.finite(distance) | distance <= max.distance))
    raw[[j]] <- list(index=candidate, distance=distance[candidate])
    needs.screen <- needs.screen ||
                    (!meta$coordinate.available[j] &&
                     is.finite(max.candidates) &&
                     length(candidate) > max.candidates)
  }

  # Calculate the approximate similarity matrix only when a pool needs capping.
  screen <- if (needs.screen)
    .QC_spatial_screen(values, screen.method) else NULL

  for (j in seq_len(nstations)) {
    candidate <- raw[[j]]$index
    distance <- raw[[j]]$distance
    similarity <- if (meta$has.elevation)
      exp(-abs(heights[candidate] - heights[j]) / elevation.scale) else
      rep(1, length(candidate))

    if (is.finite(max.candidates) && length(candidate) > max.candidates) {
      if (meta$coordinate.available[j]) {
        known <- is.finite(distance)
        effective <- pmax(distance, 0.1) /
                     pmax(similarity, .Machine$double.xmin)
        order.index <- order(!known, effective, na.last=TRUE)
      } else {
        screen.value <- screen[j, candidate]
        order.index <- order(-screen.value, -similarity, na.last=TRUE)
      }
      keep <- utils::head(order.index, max.candidates)
      candidate <- candidate[keep]
      distance <- distance[keep]
      similarity <- similarity[keep]
    }

    candidates[[j]] <- candidate
    distances[[j]] <- distance
    similarities[[j]] <- similarity
  }
  names(candidates) <- names(distances) <- names(similarities) <- stations

  list(candidates=candidates, distance=distances,
       elevation.similarity=similarities,
       coordinate.available=meta$coordinate.available,
       has.coords=meta$has.coords, has.elevation=meta$has.elevation,
       metadata=meta$metadata, stations=stations,
       max.candidates=max.candidates, screen.method=screen.method)

} # '.QC_spatial_context' END


.QC_selection_overlap <- function(min.overlap, selected.rows, total.rows) {

  # Scale the overlap requirement when neighbour selection uses a row sample.
  min.overlap <- .precipQC_check_positive_integer(min.overlap,
                                                   "min.overlap")
  if (length(selected.rows) == total.rows)
    return(min.overlap)

  max(2L, min(length(selected.rows),
              as.integer(ceiling(min.overlap * length(selected.rows) /
                                 total.rows))))

} # '.QC_selection_overlap' END


.QC_rank_columns <- function(values, log.transform=FALSE) {

  # Rank each station once so Spearman screening is vectorized by target.
  if (log.transform)
    values <- log1p(pmax(values, 0))
  for (j in seq_len(NCOL(values))) {
    ok <- is.finite(values[, j])
    values[, j] <- NA_real_
    if (sum(ok) >= 2L)
      values[ok, j] <- rank(values[ok, j], ties.method="average")
  }
  values

} # '.QC_rank_columns' END


.QC_pairwise_lm_stats <- function(target, predictors, minimum) {

  # Calculate all target-on-predictor fits from pairwise sufficient statistics.
  if (is.null(dim(predictors)))
    predictors <- matrix(predictors, ncol=1L)
  target.ok <- is.finite(target)
  valid <- is.finite(predictors) & target.ok
  n <- colSums(valid)
  x <- predictors
  x[!valid] <- 0
  y <- target
  y[!target.ok] <- 0

  sx <- colSums(x)
  sy <- as.numeric(crossprod(valid * 1, y))
  sxx <- colSums(x * x)
  syy <- as.numeric(crossprod(valid * 1, y * y))
  sxy <- colSums(x * y)
  centred.x <- sxx - sx^2 / pmax(n, 1)
  centred.y <- syy - sy^2 / pmax(n, 1)
  covariance <- sxy - sx * sy / pmax(n, 1)
  correlation <- covariance / sqrt(centred.x * centred.y)
  slope <- covariance / centred.x
  intercept <- (sy - slope * sx) / pmax(n, 1)
  sse <- syy + n * intercept^2 + slope^2 * sxx -
         2 * intercept * sy - 2 * slope * sxy +
         2 * intercept * slope * sx
  rmse <- sqrt(pmax(sse, 0) / pmax(n, 1))

  invalid <- n < minimum | !is.finite(centred.x) |
             centred.x <= sqrt(.Machine$double.eps) |
             !is.finite(centred.y) |
             centred.y <= sqrt(.Machine$double.eps) |
             !is.finite(correlation) | !is.finite(rmse)
  correlation[invalid] <- slope[invalid] <- intercept[invalid] <-
    rmse[invalid] <- NA_real_

  list(n=n, correlation=correlation, intercept=intercept,
       slope=slope, rmse=rmse)

} # '.QC_pairwise_lm_stats' END


.QC_precip_row_stats <- function(x, weights, min.neighbours, weighted) {

  # Use the compiled small-row kernel for weighted medians and local MADs.
  .Call(.qc_precip_row_stats, x, as.numeric(weights),
        as.integer(min.neighbours), as.logical(weighted))

} # '.QC_precip_row_stats' END
