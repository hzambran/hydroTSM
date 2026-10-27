library(hydroTSM)

set.seed(789)

################################################################################
# Daily streamflow QC                                                         #
################################################################################

# Build correlated daily stations with known physical, persistence, and gaps.
daily.dates <- seq(as.Date("2019-01-01"), by="day", length.out=730)
daily.base <- 20 + 8 * sin(2 * pi * seq_along(daily.dates) / 365.25) +
              stats::rnorm(length(daily.dates), 0, 0.3)
daily.values <- cbind(
  S1=daily.base,
  S2=1.1 * daily.base + stats::rnorm(length(daily.base), 0, 0.2),
  S3=0.9 * daily.base + stats::rnorm(length(daily.base), 0, 0.2),
  S4=daily.base
)
daily.values[10, "S1"] <- -1
daily.values[100:110, "S2"] <- 15
daily.values[300, "S3"] <- 250
daily.values[1:250, "S4"] <- NA_real_
daily <- zoo::zoo(daily.values, daily.dates)

# Metadata order differs from the time-series order to test identifier matching.
metadata <- data.frame(
  gauge=rev(colnames(daily)), lon=rev(c(-71.3, -71.2, -71.1, -71.0)),
  lat=rev(c(-33.3, -33.2, -33.1, -33.0)),
  area_km2=rev(c(100, 120, 90, 110)),
  elevation=rev(c(200, 250, 180, 220)), stringsAsFactors=FALSE
)

# Run the complete daily workflow while retaining flagged stations for inspection.
daily.qc <- streamflowQC_daily(
  daily, metadata=metadata, station.id="gauge",
  coords=c("lon", "lat"), area="area_km2", elevation="elevation",
  min.years=0, max.missing=0.2, max.review=1, max.suspicious=1,
  min.overlap=30L, min.correlation=-1, max.distance=1000,
  correction="set_na", breakpoint.min.years=2L, verbose=FALSE
)

# Confirm the public result contract and known injected errors.
required.components <- c(
  "accepted.metadata", "discarded.metadata", "accepted.data",
  "discarded.data", "accepted.corrected", "discarded.corrected",
  "suspicious", "corrections", "flags", "flag.count", "rejected",
  "station.summary", "breakpoint", "spatial.estimate", "spatial.score",
  "spatial.support", "neighbours", "highflow", "return.period", "settings"
)
stopifnot(
  inherits(daily.qc, "streamflowQC"),
  all(required.components %in% names(daily.qc)),
  zoo::is.zoo(daily.qc$accepted.data),
  zoo::is.zoo(daily.qc$discarded.data),
  "S4" %in% daily.qc$discarded.metadata$gauge,
  "S4" %in% colnames(daily.qc$discarded.data),
  isTRUE(daily.qc$flags$range[10, "S1"]),
  all(daily.qc$flags$flatline[100:110, "S2"]),
  isTRUE(daily.qc$flags$spike[300, "S3"]),
  is.na(daily.qc$accepted.corrected[10, "S1"]),
  inherits(daily.qc$highflow, "streamflowQC_highflow"),
  identical(daily.qc$settings$area, "area_km2"),
  isTRUE(daily.qc$settings$checks["spatial"])
)

# Every test can be disabled with one named logical vector.
daily.checks.off <- c(
  range=FALSE, duplicate=FALSE, gap=FALSE, climatology=FALSE,
  flatline=FALSE, spike=FALSE, rate=FALSE, highflow=FALSE,
  spatial=FALSE, breakpoint=FALSE
)
daily.none <- streamflowQC_daily(
  daily[, 1:2], checks=daily.checks.off, min.years=0,
  max.missing=1, max.review=1, max.suspicious=1, verbose=FALSE
)
stopifnot(length(daily.none$flags) == 0L,
          is.null(daily.none$highflow),
          NCOL(daily.none$discarded.data) == 0L)

# Date bounds subset before QC and retain the resolved period in settings.
daily.window <- streamflowQC_daily(
  daily[, 1:2], checks=daily.checks.off,
  from=as.Date("2019-02-01"), to=as.Date("2019-03-31"),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(
  identical(zoo::index(daily.window$accepted.data),
            daily.dates[daily.dates >= as.Date("2019-02-01") &
                        daily.dates <= as.Date("2019-03-31")]),
  identical(as.character(daily.window$settings$from), "2019-02-01"),
  identical(as.character(daily.window$settings$to), "2019-03-31")
)

# Spatial QC defaults to off without metadata when not explicitly supplied.
daily.nonspatial.off <- daily.checks.off[names(daily.checks.off) != "spatial"]
daily.no.metadata <- streamflowQC_daily(
  daily[, 1:3], checks=daily.nonspatial.off,
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(identical(unname(daily.no.metadata$settings$checks["spatial"]),
                    FALSE),
          is.null(daily.no.metadata$spatial.estimate))

# Explicit spatial choices remain authoritative with or without metadata.
daily.spatial.on <- daily.checks.off
daily.spatial.on["spatial"] <- TRUE
daily.explicit.spatial <- streamflowQC_daily(
  daily[1:120, 1:3], checks=daily.spatial.on,
  min.overlap=30L, min.correlation=-1,
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
daily.explicit.no.spatial <- streamflowQC_daily(
  daily[1:120, 1:3], metadata=metadata[metadata$gauge != "S4", ],
  station.id="gauge", checks=daily.checks.off,
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(isTRUE(daily.explicit.spatial$settings$checks["spatial"]),
          "spatial" %in% names(daily.explicit.spatial$flags),
          identical(unname(
            daily.explicit.no.spatial$settings$checks["spatial"]), FALSE),
          is.null(daily.explicit.no.spatial$spatial.estimate))

# Invalid date windows fail before any QC calculation is started.
invalid.window <- try(
  streamflowQC_daily(
    daily[, 1:2], checks=daily.checks.off,
    from=as.Date("2019-03-01"), to=as.Date("2019-02-01"),
    verbose=FALSE
  ), silent=TRUE
)
stopifnot(inherits(invalid.window, "try-error"),
          grepl("'from > to'", invalid.window, fixed=TRUE))

# Exact copied daily calendar blocks are independently auditable.
duplicate.dates <- seq(as.Date("2020-01-01"),
                       as.Date("2020-02-29"), by="day")
duplicate.values <- seq_along(duplicate.dates)
duplicate.values[32:60] <- duplicate.values[1:29]
duplicate.flags <- streamflowQC_duplicate(
  zoo::zoo(duplicate.values, duplicate.dates)
)
stopifnot(all(duplicate.flags[c(1:29, 32:60), 1]))

# Spatial output includes selected neighbours, estimates, scores, and support.
spatial <- streamflowQC_spatial(
  daily[, 1:3], metadata=metadata[metadata$gauge != "S4", ],
  station.id="gauge", coords=c("lon", "lat"), area="area_km2",
  elevation="elevation", max.distance=1000, min.overlap=30,
  min.correlation=-1
)
stopifnot(inherits(spatial, "streamflowQC_spatial"),
          zoo::is.zoo(spatial$flags), zoo::is.zoo(spatial$estimate),
          zoo::is.zoo(spatial$support.count),
          length(spatial$neighbours$S1) == 2L)

# A multi-year high-flow test fits GEV diagnostics without extra dependencies.
high.dates <- as.Date(paste0(2000:2011, "-07-01"))
high.values <- zoo::zoo(cbind(S1=seq(10, 21), S2=seq(15, 26)), high.dates)
high <- streamflowQC_highflow(high.values, min.years=10L)
stopifnot(inherits(high, "streamflowQC_highflow"),
          zoo::is.zoo(high$return.period),
          all(high$diagnostics$n.years == 12L))

# The wrapper reports dispatch, enabled stages, and the final summary.
daily.verbose.checks <- daily.checks.off
daily.verbose.checks["range"] <- TRUE
daily.messages <- capture.output(
  daily.wrapper <- streamflowQC(
    daily[, 1:2], checks=daily.verbose.checks, min.years=0,
    max.missing=1, max.review=1, max.suspicious=1, verbose=TRUE
  ),
  type="message"
)
daily.silent <- capture.output(
  invisible(streamflowQC(
    daily[, 1:2], checks=daily.verbose.checks, min.years=0,
    max.missing=1, max.review=1, max.suspicious=1, verbose=FALSE
  )),
  type="message"
)
invalid.verbose <- try(
  streamflowQC(daily[, 1:2], verbose=NA), silent=TRUE
)
stopifnot(
  identical(daily.wrapper$settings$resolution, "daily"),
  identical(daily.wrapper$settings$verbose, TRUE),
  any(grepl("Detected daily data", daily.messages, fixed=TRUE)),
  any(grepl("Running physical-range check", daily.messages, fixed=TRUE)),
  any(grepl("Completed daily QC", daily.messages, fixed=TRUE)),
  length(daily.silent) == 0L,
  inherits(invalid.verbose, "try-error"),
  grepl("'verbose' must be a logical value", invalid.verbose, fixed=TRUE)
)

# The wrapper applies its date window before detecting the input frequency.
daily.wrapper.window <- streamflowQC(
  daily[, 1:2], checks=daily.checks.off,
  from=as.Date("2019-04-01"), to=as.Date("2019-04-30"),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(identical(zoo::index(daily.wrapper.window$accepted.data),
                    daily.dates[91:120]))

################################################################################
# Sub-daily streamflow QC                                                     #
################################################################################

# Use hourly data so duration arguments can be checked directly.
subdaily.times <- seq(
  as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
  by="hour", length.out=24 * 60
)
subdaily.base <- 10 + 2 * sin(seq_along(subdaily.times) / 40) +
                 stats::rnorm(length(subdaily.times), 0, 0.05)
subdaily.values <- cbind(
  S1=subdaily.base,
  S2=1.1 * subdaily.base,
  S3=0.9 * subdaily.base,
  S4=subdaily.base
)
subdaily.values[20, "S1"] <- -1
subdaily.values[200:203, "S2"] <- 100
subdaily.values[500:519, "S3"] <- rep(c(8, 12), 10)
subdaily.values[1:500, "S4"] <- NA_real_
subdaily <- zoo::zoo(subdaily.values, subdaily.times)

# Run all sub-daily point checks except the long-record breakpoint diagnostic.
subdaily.qc <- streamflowQC_subdaily(
  subdaily, metadata=metadata, station.id="gauge",
  coords=c("lon", "lat"), area="area_km2", elevation="elevation",
  checks=c(breakpoint=FALSE), flatline.hours=3,
  high.flatline.hours=2, climatology.min.samples=20L,
  rate.min.samples=20L, min.overlap=30L, min.correlation=-1,
  max.distance=1000, min.years=0, max.missing=0.2,
  max.review=1, max.suspicious=1, correction="set_na", verbose=FALSE
)
stopifnot(
  inherits(subdaily.qc, "streamflowQC"),
  identical(subdaily.qc$settings$resolution, "subdaily"),
  identical(subdaily.qc$settings$interval.hours, 1),
  isTRUE(subdaily.qc$flags$range[20, "S1"]),
  all(subdaily.qc$flags$flatline[200:203, "S2"]),
  any(subdaily.qc$flags$fluctuation[500:519, "S3"]),
  "S4" %in% colnames(subdaily.qc$discarded.data),
  is.na(subdaily.qc$accepted.corrected[20, "S1"]),
  inherits(zoo::index(subdaily.qc$accepted.data), "POSIXt")
)

# Disable all checks for focused sub-daily API tests below.
subdaily.checks.off <- c(
  range=FALSE, gap=FALSE, climatology=FALSE, flatline=FALSE,
  spike=FALSE, rate=FALSE, fluctuation=FALSE, highflow=FALSE,
  spatial=FALSE, breakpoint=FALSE
)

# POSIXct bounds retain the sub-daily index class and exact endpoints.
subdaily.from <- subdaily.times[25L]
subdaily.to <- subdaily.times[72L]
subdaily.window <- streamflowQC_subdaily(
  subdaily[, 1:2], checks=subdaily.checks.off,
  from=subdaily.from, to=subdaily.to,
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(
  identical(zoo::index(subdaily.window$accepted.data),
            subdaily.times[25:72]),
  identical(subdaily.window$settings$from, subdaily.from),
  identical(subdaily.window$settings$to, subdaily.to)
)

# The frequency-aware wrapper also reports sub-daily dispatch and progress.
subdaily.verbose.checks <- subdaily.checks.off
subdaily.verbose.checks["range"] <- TRUE
subdaily.messages <- capture.output(
  subdaily.wrapper <- streamflowQC(
    subdaily[, 1:2], checks=subdaily.verbose.checks, min.years=0,
    max.missing=1, max.review=1, max.suspicious=1, verbose=TRUE
  ),
  type="message"
)
stopifnot(
  identical(subdaily.wrapper$settings$resolution, "subdaily"),
  identical(subdaily.wrapper$settings$verbose, TRUE),
  any(grepl("Detected hourly data", subdaily.messages, fixed=TRUE)),
  any(grepl("Starting sub-daily QC", subdaily.messages, fixed=TRUE)),
  any(grepl("Running physical-range check", subdaily.messages, fixed=TRUE)),
  any(grepl("Completed sub-daily QC", subdaily.messages, fixed=TRUE))
)

# The sub-daily station worker must preserve serial flags and diagnostics.
subdaily.parallel.checks <- subdaily.checks.off
subdaily.parallel.checks[c("gap", "flatline", "rate", "fluctuation",
                           "highflow")] <- TRUE
subdaily.serial <- streamflowQC_subdaily(
  subdaily[1:300, 1:2], checks=subdaily.parallel.checks,
  flatline.hours=3, high.flatline.hours=2,
  rate.max.rise=c(Inf, Inf), rate.max.fall=c(Inf, Inf),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
subdaily.socket <- streamflowQC_subdaily(
  subdaily[1:300, 1:2], checks=subdaily.parallel.checks,
  flatline.hours=3, high.flatline.hours=2,
  rate.max.rise=c(Inf, Inf), rate.max.fall=c(Inf, Inf),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE, ncores=2L, parallel.type="socket"
)
stopifnot(
  identical(subdaily.serial$flags, subdaily.socket$flags),
  identical(subdaily.serial$station.summary,
            subdaily.socket$station.summary),
  identical(subdaily.serial$highflow, subdaily.socket$highflow)
)

# PSOCK execution must preserve daily results and report effective resources.
parallel.checks <- c(
  range=TRUE, duplicate=FALSE, gap=TRUE, climatology=FALSE,
  flatline=TRUE, spike=TRUE, rate=TRUE, highflow=TRUE,
  spatial=TRUE, breakpoint=FALSE
)
parallel.serial <- streamflowQC_daily(
  daily[1:120, ], metadata=metadata, station.id="gauge",
  coords=c("lon", "lat"), area="area_km2", elevation="elevation",
  checks=parallel.checks, rate.max.rise=rep(Inf, 4),
  rate.max.fall=rep(Inf, 4), min.overlap=30, min.correlation=-1,
  max.distance=1000, min.years=0, max.missing=1,
  max.review=1, max.suspicious=1, verbose=FALSE
)
parallel.messages <- capture.output(
  parallel.socket <- streamflowQC_daily(
    daily[1:120, ], metadata=metadata, station.id="gauge",
    coords=c("lon", "lat"), area="area_km2", elevation="elevation",
    checks=parallel.checks, rate.max.rise=rep(Inf, 4),
    rate.max.fall=rep(Inf, 4), min.overlap=30, min.correlation=-1,
    max.distance=1000, min.years=0, max.missing=1,
    max.review=1, max.suspicious=1, verbose=TRUE,
    ncores=2L, parallel.type="socket", cluster.nodes="localhost"
  ), type="message"
)
stopifnot(
  identical(parallel.serial$flags, parallel.socket$flags),
  identical(parallel.serial$station.summary,
            parallel.socket$station.summary),
  identical(parallel.serial$neighbours, parallel.socket$neighbours),
  identical(parallel.serial$highflow, parallel.socket$highflow),
  identical(parallel.socket$settings$ncores, 2L),
  identical(parallel.socket$settings$parallel.type, "socket"),
  any(grepl("Using socket backend with 2 worker(s) across 1 node(s)",
            parallel.messages, fixed=TRUE))
)

# Invalid backend controls fail before QC calculations begin.
invalid.cores <- try(streamflowQC_daily(daily, ncores=0), silent=TRUE)
invalid.nodes <- try(
  streamflowQC_daily(daily, ncores=2, parallel.type="fork",
                     cluster.nodes="localhost"), silent=TRUE
)
stopifnot(inherits(invalid.cores, "try-error"),
          inherits(invalid.nodes, "try-error"))

# The class help page has a syntactically valid topic for '?'.
class.help <- utils::help("streamflowQC_class", package="hydroTSM")
legacy.class.help <- utils::help("streamflowQC-class", package="hydroTSM")
stopifnot(length(class.help) == 1L, length(legacy.class.help) == 1L)

# Summary plotting writes only to the caller-supplied temporary graphics device.
plot.file <- tempfile(fileext=".pdf")
grDevices::pdf(plot.file)
plot(subdaily.qc)
grDevices::dev.off()
stopifnot(file.exists(plot.file), file.info(plot.file)$size > 0)
