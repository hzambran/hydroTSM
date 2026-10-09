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
  correction="set_na", breakpoint.min.years=2L
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
  identical(daily.qc$settings$area, "area_km2")
)

# Every test can be disabled with one named logical vector.
daily.checks.off <- c(
  range=FALSE, duplicate=FALSE, gap=FALSE, climatology=FALSE,
  flatline=FALSE, spike=FALSE, rate=FALSE, highflow=FALSE,
  spatial=FALSE, breakpoint=FALSE
)
daily.none <- streamflowQC_daily(
  daily[, 1:2], checks=daily.checks.off, min.years=0,
  max.missing=1, max.review=1, max.suspicious=1
)
stopifnot(length(daily.none$flags) == 0L,
          is.null(daily.none$highflow),
          NCOL(daily.none$discarded.data) == 0L)

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

# The frequency-aware wrapper selects the daily workflow.
daily.wrapper <- streamflowQC(
  daily[, 1:2], checks=daily.checks.off, min.years=0,
  max.missing=1, max.review=1, max.suspicious=1
)
stopifnot(identical(daily.wrapper$settings$resolution, "daily"))

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
  max.review=1, max.suspicious=1, correction="set_na"
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

# The frequency-aware wrapper also selects the sub-daily workflow.
subdaily.checks.off <- c(
  range=FALSE, gap=FALSE, climatology=FALSE, flatline=FALSE,
  spike=FALSE, rate=FALSE, fluctuation=FALSE, highflow=FALSE,
  spatial=FALSE, breakpoint=FALSE
)
subdaily.wrapper <- streamflowQC(
  subdaily[, 1:2], checks=subdaily.checks.off, min.years=0,
  max.missing=1, max.review=1, max.suspicious=1
)
stopifnot(identical(subdaily.wrapper$settings$resolution, "subdaily"))

# Summary plotting writes only to the caller-supplied temporary graphics device.
plot.file <- tempfile(fileext=".pdf")
grDevices::pdf(plot.file)
plot(subdaily.qc)
grDevices::dev.off()
stopifnot(file.exists(plot.file), file.info(plot.file)$size > 0)
