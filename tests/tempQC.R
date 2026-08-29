library(hydroTSM)

set.seed(456)

################################################################################
# Daily air-temperature QC                                                    #
################################################################################

daily.dates <- seq(as.Date("2019-01-01"), by="day", length.out=730)
season <- 15 + 10 * sin(2 * pi * seq_along(daily.dates) / 365.25)
daily.values <- cbind(
  S1=season + stats::rnorm(length(season), 0, 0.5),
  S2=season + stats::rnorm(length(season), 0, 0.5),
  S3=season + stats::rnorm(length(season), 0, 0.5),
  S4=season + stats::rnorm(length(season), 0, 0.5)
)
daily.values[10, "S1"] <- 100
daily.values[100:106, "S2"] <- 5
daily.values[300, "S3"] <- daily.values[299, "S3"] + 30
daily.values[1:200, "S4"] <- NA_real_
daily <- zoo::zoo(daily.values, daily.dates)

metadata <- data.frame(
  station=colnames(daily),
  lon=c(-71.3, -71.2, -71.1, -71.0),
  lat=c(-33.3, -33.2, -33.1, -33.0),
  elevation=c(100, 150, 200, 250),
  stringsAsFactors=FALSE
)

daily.qc <- tempQC_daily(
  daily, metadata=metadata, min.years=0, max.suspicious=1,
  max.distance=1000, correction="set_na",
  breakpoint.min.years=2L, elevation="elevation"
)

required.components <- c(
  "accepted.metadata", "discarded.metadata", "accepted.data",
  "accepted.corrected", "suspicious", "corrections", "flags",
  "flag.count", "rejected", "station.summary", "breakpoint",
  "spatial.estimate", "spatial.score", "neighbours", "settings"
)

stopifnot(
  inherits(daily.qc, "tempQC"),
  identical(names(daily.qc), required.components),
  zoo::is.zoo(daily.qc$accepted.data),
  zoo::is.zoo(daily.qc$accepted.corrected),
  "S4" %in% daily.qc$discarded.metadata$station,
  isTRUE(daily.qc$flags$range[10, "S1"]),
  all(daily.qc$flags$persistence[100:106, "S2"]),
  isTRUE(daily.qc$flags$spike[300, "S3"]),
  is.na(daily.qc$accepted.corrected[10, "S1"]),
  all(c("indicator", "n.indicators") %in% names(daily.qc$breakpoint))
)

no.checks <- c(
  range=FALSE, duplicate=FALSE, climatology=FALSE,
  persistence=FALSE, step=FALSE, spike=FALSE,
  spatial=FALSE, breakpoint=FALSE
)
daily.none <- tempQC_daily(
  daily[, 1:2], metadata=metadata[1:2, ], checks=no.checks,
  min.years=0, max.missing=1, max.suspicious=1
)
stopifnot(length(daily.none$flags) == 0L)

duplicate.dates <- seq(as.Date("2020-01-01"),
                       as.Date("2020-02-29"), by="day")
duplicate.values <- stats::rnorm(length(duplicate.dates))
duplicate.values[32:60] <- duplicate.values[1:29]
duplicate.flags <- tempQC_duplicate(
  zoo::zoo(duplicate.values, duplicate.dates)
)
stopifnot(all(duplicate.flags[c(1:29, 32:60), 1]))

tmin <- zoo::zoo(cbind(S1=c(5, 20, 6), S2=c(1, 2, 3)),
                  as.Date("2020-01-01") + 0:2)
tmax <- zoo::zoo(cbind(S1=c(10, 10, 11), S2=c(5, 6, 7)),
                  as.Date("2020-01-01") + 0:2)
internal <- tempQC_internal(tmin, tmax)
stopifnot(isTRUE(internal$tmin[2, "S1"]),
          isTRUE(internal$tmax[2, "S1"]))

spatial <- tempQC_spatial(
  daily[, 1:3], metadata=metadata[1:3, ], max.distance=1000,
  min.overlap=30, min.group.overlap=10, min.correlation=-1
)
stopifnot(inherits(spatial, "tempQC_spatial"),
          zoo::is.zoo(spatial$flags), zoo::is.zoo(spatial$estimate))

custom.metadata <- data.frame(
  gauge_id=rev(colnames(daily)[1:3]),
  longitude=rev(metadata$lon[1:3]),
  latitude=rev(metadata$lat[1:3]),
  height_m=rev(metadata$elevation[1:3]),
  stringsAsFactors=FALSE
)
daily.custom <- tempQC_daily(
  daily[, 1:3], metadata=custom.metadata, station.id="gauge_id",
  coords=c("longitude", "latitude"), elevation="height_m",
  checks=no.checks, min.years=0, max.missing=1, max.suspicious=1
)
daily.nometa <- tempQC_daily(
  daily[, 1:2], checks=no.checks, min.years=0,
  max.missing=1, max.suspicious=1
)
stopifnot(
  identical(daily.custom$accepted.metadata$gauge_id,
            colnames(daily)[1:3]),
  identical(daily.custom$settings$elevation, "height_m"),
  identical(daily.nometa$accepted.metadata$station,
            colnames(daily)[1:2])
)

rank.dates <- seq(as.Date("2020-01-01"), by="day", length.out=120)
rank.base <- 12 + sin(seq_along(rank.dates) / 8)
rank.noise <- rep(c(-0.3, 0.1, 0.2, 0), length.out=120)
rank.temp <- zoo::zoo(cbind(
  S1=rank.base,
  S2=rank.base + rank.noise,
  S3=rank.base + c(rank.noise[-1], rank.noise[1]),
  S4=rank.base + c(rank.noise[-c(1, 2)], rank.noise[1:2])
), rank.dates)
rank.metadata <- data.frame(
  code=colnames(rank.temp), longitude=c(0, 0.01, 0.02, 0.03),
  latitude=rep(0, 4), height_m=c(100, 2100, 100, 100)
)
rank.spatial <- tempQC_spatial(
  rank.temp, metadata=rank.metadata, station.id="code",
  coords=c("longitude", "latitude"), elevation="height_m",
  elevation.scale=100, n.neighbours=2, max.distance=10,
  min.overlap=30, min.group.overlap=10, min.correlation=-1
)
stopifnot(identical(rank.spatial$neighbours$S1, c("S3", "S4")))

daily.wrapper <- tempQC(
  daily[, 1:2], metadata=metadata[1:2, ], checks=no.checks,
  min.years=0, max.missing=1, max.suspicious=1
)
stopifnot(identical(daily.wrapper$settings$resolution, "daily"))

################################################################################
# Sub-daily air-temperature QC                                                #
################################################################################

subdaily.times <- seq(
  as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
  by="hour", length.out=24 * 60
)
hour <- as.integer(format(subdaily.times, "%H"))
subdaily.base <- 15 + 6 * sin(2 * pi * (hour - 7) / 24)
subdaily.values <- cbind(
  S1=subdaily.base + stats::rnorm(length(hour), 0, 0.2),
  S2=subdaily.base + stats::rnorm(length(hour), 0, 0.2),
  S3=subdaily.base + stats::rnorm(length(hour), 0, 0.2),
  S4=subdaily.base + stats::rnorm(length(hour), 0, 0.2)
)
subdaily.values[20, "S1"] <- 100
subdaily.values[200:203, "S2"] <- 10
subdaily.values[500, "S3"] <- subdaily.values[499, "S3"] + 20
subdaily <- zoo::zoo(subdaily.values, subdaily.times)

subdaily.qc <- tempQC_subdaily(
  subdaily, metadata=metadata, min.years=0, max.suspicious=1,
  max.distance=1000, checks=c(breakpoint=FALSE),
  correction="set_na", elevation="elevation"
)
stopifnot(
  inherits(subdaily.qc, "tempQC"),
  identical(subdaily.qc$settings$resolution, "subdaily"),
  identical(subdaily.qc$settings$interval.hours, 1),
  isTRUE(subdaily.qc$flags$range[20, "S1"]),
  all(subdaily.qc$flags$persistence[200:203, "S2"]),
  isTRUE(subdaily.qc$flags$spike[500, "S3"]),
  is.na(subdaily.qc$accepted.corrected[20, "S1"]),
  inherits(zoo::index(subdaily.qc$accepted.data), "POSIXt")
)

subdaily.no.checks <- c(
  range=FALSE, climatology=FALSE, persistence=FALSE,
  step=FALSE, spike=FALSE, spatial=FALSE, breakpoint=FALSE
)
subdaily.wrapper <- tempQC(
  subdaily[, 1:2], metadata=metadata[1:2, ],
  checks=subdaily.no.checks, min.years=0,
  max.missing=1, max.suspicious=1
)
stopifnot(identical(subdaily.wrapper$settings$resolution, "subdaily"))

plot.file <- tempfile(fileext=".pdf")
grDevices::pdf(plot.file)
plot(subdaily.qc)
grDevices::dev.off()
stopifnot(file.exists(plot.file), file.info(plot.file)$size > 0)
