library(hydroTSM)

set.seed(123)

################################################################################
# Daily precipitation QC                                                      #
################################################################################

daily.dates <- seq(as.Date("2019-01-01"), by="day", length.out=730)
daily.base <- stats::rgamma(length(daily.dates), shape=0.6, rate=0.5)
daily.base[daily.base < 1] <- 0
daily.values <- cbind(
  S1=daily.base,
  S2=pmax(0, daily.base + stats::rnorm(length(daily.base), 0, 0.1)),
  S3=pmax(0, daily.base + stats::rnorm(length(daily.base), 0, 0.1)),
  S4=daily.base
)
daily.values[10, "S1"] <- -1
daily.values[100:104, "S2"] <- 20
daily.values[1:200, "S4"] <- NA_real_
daily <- zoo::zoo(daily.values, daily.dates)

metadata <- data.frame(
  station=colnames(daily),
  lon=c(-71.3, -71.2, -71.1, -71.0),
  lat=c(-33.3, -33.2, -33.1, -33.0),
  elevation=c(100, 150, 200, 250),
  stringsAsFactors=FALSE
)

daily.qc <- precipQC_daily(
  daily, metadata=metadata, min.years=0, max.suspicious=1,
  max.distance=1000, correction="set_na", elevation="elevation"
)

required.components <- c(
  "accepted.metadata", "discarded.metadata", "accepted.data",
  "accepted.corrected", "suspicious", "corrections", "flags",
  "flag.count", "rejected", "station.summary", "breakpoint",
  "spatial.estimate", "spatial.score", "neighbours", "settings"
)

stopifnot(
  inherits(daily.qc, "precipQC"),
  identical(names(daily.qc), required.components),
  is.data.frame(daily.qc$accepted.metadata),
  is.data.frame(daily.qc$discarded.metadata),
  zoo::is.zoo(daily.qc$accepted.data),
  zoo::is.zoo(daily.qc$accepted.corrected),
  zoo::is.zoo(daily.qc$flag.count),
  zoo::is.zoo(daily.qc$rejected),
  "S4" %in% daily.qc$discarded.metadata$station,
  all(c("S1", "S2", "S3") %in% daily.qc$accepted.metadata$station),
  daily.qc$accepted.data[10, "S1"] == -1,
  is.na(daily.qc$accepted.corrected[10, "S1"]),
  isTRUE(daily.qc$flags$range[10, "S1"]),
  all(daily.qc$flags$persistence[100:104, "S2"]),
  any(daily.qc$suspicious$station == "S1" &
      daily.qc$suspicious$original == -1 &
      daily.qc$suspicious$action == "reject"),
  any(daily.qc$corrections$station == "S1")
)

no.checks <- c(
  range=FALSE, duplicate=FALSE, frequency=FALSE, gap=FALSE,
  climatology=FALSE, persistence=FALSE,
  accumulation=FALSE, weekday=FALSE, spatial=FALSE, dryspell=FALSE,
  breakpoint=FALSE
)
daily.none <- precipQC_daily(
  daily[, 1:2], metadata=metadata[1:2, ], checks=no.checks,
  min.years=0, max.missing=1, max.suspicious=1
)
stopifnot(
  length(daily.none$flags) == 0L,
  NROW(daily.none$suspicious) == 0L,
  all(daily.none$station.summary$recommendation == "accept")
)

custom.metadata <- data.frame(
  gauge_id=rev(colnames(daily)[1:3]),
  longitude=rev(metadata$lon[1:3]),
  latitude=rev(metadata$lat[1:3]),
  height_m=rev(metadata$elevation[1:3]),
  stringsAsFactors=FALSE
)
daily.custom <- precipQC_daily(
  daily[, 1:3], metadata=custom.metadata, station.id="gauge_id",
  coords=c("longitude", "latitude"), elevation="height_m",
  checks=no.checks, min.years=0, max.missing=1, max.suspicious=1
)
daily.nometa <- precipQC_daily(
  daily[, 1:2], checks=no.checks, min.years=0,
  max.missing=1, max.suspicious=1
)
invalid.coords <- try(precipQC_daily(
  daily[, 1:2], metadata=custom.metadata[2:3, ],
  station.id="gauge_id", coords=c("x", "latitude"), checks=no.checks
), silent=TRUE)
stopifnot(
  identical(daily.custom$accepted.metadata$gauge_id,
            colnames(daily)[1:3]),
  identical(daily.custom$settings$station.id, "gauge_id"),
  identical(daily.custom$settings$coords,
            c("longitude", "latitude")),
  identical(daily.custom$settings$elevation, "height_m"),
  identical(daily.nometa$accepted.metadata$station,
            colnames(daily)[1:2]),
  inherits(invalid.coords, "try-error")
)

range.flags <- precipQC_range(daily, upper=1825)
persistence.flags <- precipQC_persistence(
  daily, high.threshold=10, high.run=5
)
climatology.flags <- precipQC_climatology(
  daily, min.samples=20, z=6
)
weekday.dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"),
                     by="day")
weekday.values <- cbind(A=rep(1, length(weekday.dates)),
                        B=rep(1, length(weekday.dates)))
mondays <- as.POSIXlt(weekday.dates)$wday == 1L
weekday.values[mondays, "A"] <- 0
weekday.data <- zoo::zoo(weekday.values, weekday.dates)
weekday.flags <- precipQC_weekday(weekday.data)
spatial.flags <- precipQC_spatial(
  daily, metadata=metadata, max.distance=1000,
  min.overlap=30, min.correlation=-1, elevation="elevation"
)
dry.flags <- precipQC_dryspell(
  daily, metadata=metadata, max.distance=1000,
  window.days=15, neighbour.fraction=0.5
)
breakpoints <- precipQC_breakpoint(daily, min.years=2)

duplicate.dates <- seq(as.Date("2020-01-01"),
                       as.Date("2020-02-29"), by="day")
duplicate.values <- seq_along(duplicate.dates)
duplicate.values[32:60] <- duplicate.values[1:29]
duplicate.data <- zoo::zoo(duplicate.values, duplicate.dates)
duplicate.flags <- precipQC_duplicate(duplicate.data)

gap.values <- c(seq_len(30), 700)
gap.data <- zoo::zoo(gap.values,
                     seq(as.Date("2020-01-01"), by="day", length.out=31))
gap.flags <- precipQC_gap(gap.data)

frequency.values <- seq_len(31)
frequency.values[1:5] <- 100
frequency.data <- zoo::zoo(
  frequency.values,
  seq(as.Date("2020-01-01"), by="day", length.out=31)
)
frequency.flags <- precipQC_frequency(frequency.data)

stopifnot(
  zoo::is.zoo(range.flags),
  zoo::is.zoo(persistence.flags),
  zoo::is.zoo(climatology.flags),
  all(duplicate.flags[c(1:29, 32:60), 1]),
  isTRUE(gap.flags[31, 1]),
  all(frequency.flags[1:5, 1]),
  zoo::is.zoo(weekday.flags),
  all(weekday.flags[mondays, "A"]),
  !any(weekday.flags[!mondays, "A"]),
  !any(weekday.flags[, "B"]),
  inherits(spatial.flags, "precipQC_spatial"),
  zoo::is.zoo(spatial.flags$flags),
  zoo::is.zoo(spatial.flags$estimate),
  zoo::is.zoo(dry.flags),
  is.data.frame(breakpoints),
  all(c("indicator", "n.indicators") %in% names(breakpoints)),
  NROW(breakpoints) == NCOL(daily)
)

rank.dates <- seq(as.Date("2020-01-01"), by="day", length.out=90)
rank.base <- 2 + sin(seq_along(rank.dates) / 6)
rank.precip <- zoo::zoo(
  cbind(S1=rank.base, S2=rank.base, S3=rank.base, S4=rank.base),
  rank.dates
)
rank.metadata <- data.frame(
  code=colnames(rank.precip), longitude=c(0, 0.01, 0.02, 0.03),
  latitude=rep(0, 4), height_m=c(100, 2100, 100, 100)
)
rank.horizontal <- precipQC_spatial(
  rank.precip, metadata=rank.metadata, station.id="code",
  coords=c("longitude", "latitude"), n.neighbours=2,
  max.distance=10, min.overlap=30, min.correlation=-1
)
rank.elevation <- precipQC_spatial(
  rank.precip, metadata=rank.metadata, station.id="code",
  coords=c("longitude", "latitude"), n.neighbours=2,
  max.distance=10, min.overlap=30, min.correlation=-1,
  elevation="height_m", elevation.scale=100
)
stopifnot(
  identical(rank.horizontal$neighbours$S1, c("S2", "S3")),
  identical(rank.elevation$neighbours$S1, c("S3", "S4"))
)

daily.wrapper <- precipQC(
  daily[, 1:2], metadata=metadata[1:2, ], checks=no.checks,
  min.years=0, max.missing=1, max.suspicious=1
)
stopifnot(identical(daily.wrapper$settings$resolution, "daily"))

################################################################################
# Sub-daily precipitation QC                                                  #
################################################################################

subdaily.times <- seq(
  as.POSIXct("2021-01-01 00:00:00", tz="UTC"),
  by="hour", length.out=24 * 30
)
subdaily.base <- stats::rexp(length(subdaily.times), rate=1)
subdaily.base[subdaily.base < 1.5] <- 0
subdaily.values <- cbind(
  S1=subdaily.base,
  S2=pmax(0, subdaily.base + stats::rnorm(length(subdaily.base), 0, 0.05)),
  S3=pmax(0, subdaily.base + stats::rnorm(length(subdaily.base), 0, 0.05)),
  S4=subdaily.base
)
subdaily.values[20, "S1"] <- -1
subdaily.values[200:201, "S2"] <- 30
subdaily <- zoo::zoo(subdaily.values, subdaily.times)

subdaily.qc <- precipQC_subdaily(
  subdaily, metadata=metadata, min.years=0, max.suspicious=1,
  max.distance=1000, min.overlap=30, spatial.hours=c(0, 3, 6),
  checks=c(monthly.accumulation=FALSE, breakpoint=FALSE),
  elevation="elevation"
)

stopifnot(
  inherits(subdaily.qc, "precipQC"),
  identical(subdaily.qc$settings$resolution, "subdaily"),
  identical(subdaily.qc$settings$interval.hours, 1),
  isTRUE(subdaily.qc$flags$range[20, "S1"]),
  all(subdaily.qc$flags$persistence[200:201, "S2"]),
  all(subdaily.qc$station.summary$recommendation == "accept"),
  inherits(zoo::index(subdaily.qc$accepted.data), "POSIXt")
)

weekday.subdaily.times <- seq(
  as.POSIXct("2020-01-01 00:00:00", tz="UTC"),
  as.POSIXct("2020-12-31 23:00:00", tz="UTC"), by="hour"
)
weekday.subdaily.dates <- as.Date(weekday.subdaily.times)
weekday.subdaily.mondays <- as.POSIXlt(weekday.subdaily.dates)$wday == 1L
weekday.subdaily.noon <- format(weekday.subdaily.times, "%H") == "12"
weekday.subdaily.values <- matrix(
  0, nrow=length(weekday.subdaily.times), ncol=2,
  dimnames=list(NULL, c("S1", "S2"))
)
weekday.subdaily.values[
  weekday.subdaily.noon & !weekday.subdaily.mondays, "S1"
] <- 1
weekday.subdaily.values[weekday.subdaily.noon, "S2"] <- 1
weekday.subdaily <- zoo::zoo(weekday.subdaily.values,
                             weekday.subdaily.times)
weekday.only <- c(
  range=FALSE, climatology=FALSE, persistence=FALSE,
  daily.accumulation=FALSE, monthly.accumulation=FALSE,
  weekday=TRUE, spatial=FALSE, dryspell=FALSE, breakpoint=FALSE
)
weekday.subdaily.qc <- precipQC_subdaily(
  weekday.subdaily, metadata=metadata[1:2, ], checks=weekday.only,
  min.years=0, max.missing=1, max.suspicious=1
)
stopifnot(
  all(weekday.subdaily.qc$flags$weekday[
    weekday.subdaily.mondays, "S1"
  ]),
  !any(weekday.subdaily.qc$flags$weekday[
    !weekday.subdaily.mondays, "S1"
  ]),
  !any(weekday.subdaily.qc$flags$weekday[, "S2"]),
  all(weekday.subdaily.qc$suspicious$action == "review")
)

subdaily.no.checks <- c(
  range=FALSE, climatology=FALSE, persistence=FALSE,
  daily.accumulation=FALSE, monthly.accumulation=FALSE,
  weekday=FALSE, spatial=FALSE, dryspell=FALSE, breakpoint=FALSE
)
subdaily.wrapper <- precipQC(
  subdaily[, 1:2], metadata=metadata[1:2, ],
  checks=subdaily.no.checks, min.years=0,
  max.missing=1, max.suspicious=1
)
stopifnot(identical(subdaily.wrapper$settings$resolution, "subdaily"))

monthly <- zoo::zoo(matrix(seq_len(12), ncol=1,
                           dimnames=list(NULL, "S1")),
                    zoo::as.yearmon(seq(as.Date("2020-01-01"),
                                        by="month", length.out=12)))
invalid.monthly <- try(precipQC(monthly), silent=TRUE)
stopifnot(inherits(invalid.monthly, "try-error"))

invalid.subdaily <- try(
  precipQC_subdaily(daily[, 1:2], metadata=metadata[1:2, ]),
  silent=TRUE
)
invalid.daily <- try(
  precipQC_daily(subdaily[, 1:2], metadata=metadata[1:2, ]),
  silent=TRUE
)
stopifnot(inherits(invalid.subdaily, "try-error"),
          inherits(invalid.daily, "try-error"))

plot.file <- tempfile(fileext=".pdf")
grDevices::pdf(plot.file)
plot(daily.qc)
grDevices::dev.off()
stopifnot(file.exists(plot.file), file.info(plot.file)$size > 0)
