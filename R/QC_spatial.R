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

  # Build a deterministic low-cost station workspace for screening.
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

  # Retain the compact standardized workspace for target-wise screening.
  work

} # '.QC_spatial_screen' END


.QC_spatial_screen_target <- function(work, target, candidates) {

  # Calculate only one target vector instead of a dense station matrix.
  if (length(candidates) == 0L)
    return(numeric())

  as.numeric(crossprod(work[, target], work[, candidates, drop=FALSE]))

} # '.QC_spatial_screen_target' END


.QC_time_blocks <- function(n, block.size=8192L) {

  # Bound temporary station-time matrices while preserving row order.
  block.size <- .precipQC_check_positive_integer(block.size, "block.size")
  starts <- seq.int(1L, n, by=block.size)
  time.block <- function(first) {
    first:min(n, first + block.size - 1L)
  } # 'time.block' END
  lapply(starts, time.block)

} # '.QC_time_blocks' END


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
    candidate.distance <- distance[candidate]
    needs.screen <- needs.screen ||
      (is.finite(max.candidates) && length(candidate) > max.candidates &&
       (!meta$coordinate.available[j] ||
        sum(is.finite(candidate.distance)) < max.candidates))
  }

  # Build an approximate workspace only when a pool needs capping.
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
        known.order <- which(known)[order(effective[known])]
        unknown.order <- which(!known)
        if (length(known.order) < max.candidates &&
            length(unknown.order) > 0L) {
          screen.value <- .QC_spatial_screen_target(
            screen, j, candidate[unknown.order]
          )
          unknown.order <- unknown.order[
            order(-screen.value, -similarity[unknown.order], na.last=TRUE)
          ]
        }
        order.index <- c(known.order, unknown.order)
      } else {
        screen.value <- .QC_spatial_screen_target(screen, j, candidate)
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
  reverse.slope <- covariance / centred.y
  reverse.intercept <- (sx - reverse.slope * sy) / pmax(n, 1)
  reverse.sse <- sxx + n * reverse.intercept^2 +
    reverse.slope^2 * syy - 2 * reverse.intercept * sx -
    2 * reverse.slope * sxy +
    2 * reverse.intercept * reverse.slope * sy
  reverse.rmse <- sqrt(pmax(reverse.sse, 0) / pmax(n, 1))

  invalid <- n < minimum | !is.finite(centred.x) |
             centred.x <= sqrt(.Machine$double.eps) |
             !is.finite(centred.y) |
             centred.y <= sqrt(.Machine$double.eps) |
             !is.finite(correlation) | !is.finite(rmse)
  correlation[invalid] <- slope[invalid] <- intercept[invalid] <-
    rmse[invalid] <- reverse.rmse[invalid] <- NA_real_

  list(n=n, correlation=correlation, intercept=intercept,
       slope=slope, rmse=rmse, reverse.rmse=reverse.rmse)

} # '.QC_pairwise_lm_stats' END


.QC_pair_cache <- function(values, context, minimum,
                           method=c("pearson", "ranked", "spearman"),
                           keep.rmse=FALSE) {

  # Represent every directed candidate edge and its undirected pair once.
  method <- match.arg(method)
  minimum <- .precipQC_check_positive_integer(minimum, "minimum")
  nstations <- NCOL(values)
  lengths.candidates <- lengths(context$candidates)
  targets <- rep.int(seq_len(nstations), lengths.candidates)
  candidates <- unlist(context$candidates, use.names=FALSE)
  lower <- pmin(targets, candidates)
  upper <- pmax(targets, candidates)
  keys <- (lower - 1) * nstations + upper
  unique.keys <- unique(keys)
  pair.index <- match(keys, unique.keys)
  pair.lower <- (unique.keys - 1) %/% nstations + 1
  pair.upper <- (unique.keys - 1) %% nstations + 1
  pair.correlation <- rep(NA_real_, length(unique.keys))
  pair.n <- integer(length(unique.keys))
  forward.rmse <- reverse.rmse <- rep(NA_real_, length(unique.keys))

  # Vectorize all pairs sharing the lower-numbered station.
  for (j in unique(pair.lower)) {
    positions <- which(pair.lower == j)
    neighbours <- pair.upper[positions]
    if (method == "spearman") {
      for (k in seq_along(neighbours)) {
        ok <- is.finite(values[, j]) & is.finite(values[, neighbours[k]])
        pair.n[positions[k]] <- sum(ok)
        if (pair.n[positions[k]] >= minimum)
          pair.correlation[positions[k]] <- suppressWarnings(stats::cor(
            log1p(pmax(values[ok, j], 0)),
            log1p(pmax(values[ok, neighbours[k]], 0)), method="spearman"
          ))
      }
    } else {
      statistics <- .QC_pairwise_lm_stats(
        values[, j], values[, neighbours, drop=FALSE], minimum
      )
      pair.n[positions] <- statistics$n
      pair.correlation[positions] <- statistics$correlation
      if (keep.rmse) {
        forward.rmse[positions] <- statistics$rmse
        reverse.rmse[positions] <- statistics$reverse.rmse
      }
    }
  }

  # Expand compact pair statistics to each target's candidate-list order.
  correlations <- split(pair.correlation[pair.index], targets)
  overlaps <- split(pair.n[pair.index], targets)
  correlations <- correlations[as.character(seq_len(nstations))]
  overlaps <- overlaps[as.character(seq_len(nstations))]
  correlations[lengths.candidates == 0L] <- list(numeric())
  overlaps[lengths.candidates == 0L] <- list(integer())
  names(correlations) <- names(overlaps) <- context$stations
  out <- list(correlation=correlations, n=overlaps,
              npairs=length(unique.keys))
  if (keep.rmse) {
    directed.rmse <- ifelse(targets == lower,
                            forward.rmse[pair.index],
                            reverse.rmse[pair.index])
    errors <- split(directed.rmse, targets)
    errors <- errors[as.character(seq_len(nstations))]
    errors[lengths.candidates == 0L] <- list(numeric())
    names(errors) <- context$stations
    out$rmse <- errors
  }
  out

} # '.QC_pair_cache' END


.QC_precip_row_stats <- function(x, weights, min.neighbours, weighted) {

  # Use the compiled small-row kernel for weighted medians and local MADs.
  .Call(.qc_precip_row_stats, x, as.numeric(weights),
        as.integer(min.neighbours), as.logical(weighted))

} # '.QC_precip_row_stats' END
