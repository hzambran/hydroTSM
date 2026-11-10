library(hydroTSM)

# Exercise the bounded spatial path at the requested network size.
set.seed(20261110)
nstations <- 2000L
nrows <- 48L
stations <- sprintf("S%04d", seq_len(nstations))
dates <- seq(as.Date("2020-01-01"), by="day", length.out=nrows)
metadata <- data.frame(
  id=stations, lon=runif(nstations, -75, -70),
  lat=runif(nstations, -40, -35), stringsAsFactors=FALSE
)
metadata$lon[seq_len(10L)] <- NA_real_
metadata$lat[11:20] <- NA_real_

precipitation <- matrix(rexp(nrows * nstations, rate=1 / 3),
                        nrow=nrows, ncol=nstations,
                        dimnames=list(NULL, stations))
precipitation[matrix(runif(length(precipitation)) < 0.7,
                     nrow=nrows)] <- 0
precipitation <- zoo(precipitation, dates)
precipitation.qc <- precipQC_spatial(
  precipitation, metadata=metadata, station.id="id",
  n.neighbours=5L, max.distance=400, min.overlap=20L,
  max.candidates=32L, selection.samples=32L
)

# Temperature uses the same bounded geometry with vectorized regressions.
temperature <- matrix(rnorm(nrows * nstations, mean=12, sd=4),
                      nrow=nrows, ncol=nstations,
                      dimnames=list(NULL, stations))
temperature <- zoo(temperature, dates)
temperature.qc <- tempQC_spatial(
  temperature, metadata=metadata, station.id="id",
  n.neighbours=5L, max.distance=400, min.overlap=20L,
  min.group.overlap=5L, min.correlation=-1,
  max.candidates=32L, selection.samples=32L
)

stopifnot(
  NCOL(precipitation.qc$flags) == nstations,
  NCOL(temperature.qc$flags) == nstations,
  length(precipitation.qc$neighbours) == nstations,
  length(temperature.qc$neighbours) == nstations,
  max(lengths(precipitation.qc$neighbours)) <= 5L,
  max(lengths(temperature.qc$neighbours)) <= 5L
)

# Infinity retains exhaustive selection and agrees on small complete inputs.
small.precipitation <- precipitation[, seq_len(4L)]
bounded <- precipQC_spatial(small.precipitation, n.neighbours=3L,
                            min.overlap=20L)
exhaustive <- precipQC_spatial(
  small.precipitation, n.neighbours=3L, min.overlap=20L,
  max.candidates=Inf, selection.samples=Inf
)
stopifnot(identical(bounded$flags, exhaustive$flags),
          identical(bounded$neighbours, exhaustive$neighbours))

# Confirm the frequency-aware wrappers forward and record both controls.
precipitation.workflow <- precipQC(
  small.precipitation,
  checks=c(range=FALSE, duplicate=FALSE, frequency=FALSE, gap=FALSE,
           climatology=FALSE, persistence=FALSE, accumulation=FALSE,
           weekday=FALSE, spatial=TRUE, dryspell=FALSE, breakpoint=FALSE),
  spatial.days=1L, n.neighbours=3L, min.overlap=20L,
  max.candidates=3L, selection.samples=20L,
  min.years=0, verbose=FALSE
)
small.temperature <- temperature[, seq_len(4L)]
temperature.workflow <- tempQC(
  small.temperature,
  checks=c(range=FALSE, duplicate=FALSE, climatology=FALSE,
           persistence=FALSE, step=FALSE, spike=FALSE,
           spatial=TRUE, breakpoint=FALSE),
  n.neighbours=3L, min.overlap=20L, min.group.overlap=5L,
  min.correlation=-1, max.candidates=3L, selection.samples=20L,
  min.years=0, verbose=FALSE
)
stopifnot(identical(precipitation.workflow$settings$max.candidates, 3L),
          identical(precipitation.workflow$settings$selection.samples, 20L),
          identical(temperature.workflow$settings$max.candidates, 3L),
          identical(temperature.workflow$settings$selection.samples, 20L))

invalid.candidates <- try(precipQC_spatial(
  small.precipitation, n.neighbours=3L, max.candidates=2L
), silent=TRUE)
stopifnot(inherits(invalid.candidates, "try-error"))
