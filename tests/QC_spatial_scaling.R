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

# Streamflow reuses the same compact geometry and blocked compiled summaries.
streamflow <- matrix(rexp(nrows * nstations, rate=1 / 15),
                     nrow=nrows, ncol=nstations,
                     dimnames=list(NULL, stations))
streamflow <- zoo(streamflow, dates)
streamflow.qc <- streamflowQC_spatial(
  streamflow, metadata=metadata, station.id="id",
  n.neighbours=5L, max.distance=400, min.overlap=20L,
  min.correlation=-1, max.candidates=32L, selection.samples=32L
)

stopifnot(
  NCOL(precipitation.qc$flags) == nstations,
  NCOL(temperature.qc$flags) == nstations,
  NCOL(streamflow.qc$flags) == nstations,
  length(precipitation.qc$neighbours) == nstations,
  length(temperature.qc$neighbours) == nstations,
  length(streamflow.qc$neighbours) == nstations,
  max(lengths(precipitation.qc$neighbours)) <= 5L,
  max(lengths(temperature.qc$neighbours)) <= 5L,
  max(lengths(streamflow.qc$neighbours)) <= 5L,
  identical(colnames(streamflow.qc$flags), stations)
)

# Confirm that incomplete coordinates retain compact reusable candidate lists.
context <- hydroTSM:::.QC_spatial_context(
  as.matrix(zoo::coredata(precipitation)), metadata=metadata,
  station.id="id", max.distance=400, n.neighbours=5L,
  max.candidates=32L, screen.method="spearman"
)
stopifnot(is.list(context$distance),
          is.list(context$elevation.similarity),
          !is.matrix(context$distance),
          max(lengths(context$candidates)) <= 32L,
          all(names(context$candidates) == stations))

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

small.streamflow <- streamflow[, seq_len(4L)]
bounded.streamflow <- streamflowQC_spatial(
  small.streamflow, n.neighbours=3L, min.overlap=20L,
  min.correlation=-1
)
exhaustive.streamflow <- streamflowQC_spatial(
  small.streamflow, n.neighbours=3L, min.overlap=20L,
  min.correlation=-1, max.candidates=Inf, selection.samples=Inf
)
stopifnot(identical(bounded.streamflow$flags,
                    exhaustive.streamflow$flags),
          identical(bounded.streamflow$neighbours,
                    exhaustive.streamflow$neighbours))

# Each reciprocal correlation is calculated once and reused in both directions.
small.context <- hydroTSM:::.QC_spatial_context(
  as.matrix(zoo::coredata(small.precipitation)),
  n.neighbours=3L, max.candidates=Inf, screen.method="spearman"
)
pair.cache <- hydroTSM:::.QC_pair_cache(
  as.matrix(zoo::coredata(small.precipitation)), small.context,
  minimum=20L, method="spearman"
)
cor.12 <- pair.cache$correlation[[1L]][
  match(2L, small.context$candidates[[1L]])]
cor.21 <- pair.cache$correlation[[2L]][
  match(1L, small.context$candidates[[2L]])]
stopifnot(pair.cache$npairs == choose(4L, 2L),
          isTRUE(all.equal(unname(cor.12), unname(cor.21))))

# Exercise row blocking beyond the native 8,192-row block size.
long.rows <- 8205L
long.index <- seq(as.POSIXct("2020-01-01", tz="UTC"), by="hour",
                  length.out=long.rows)
long.signal <- sin(seq_len(long.rows) / 48)
long.precipitation <- zoo::zoo(
  cbind(A=pmax(long.signal, 0), B=pmax(long.signal + 0.01, 0),
        C=pmax(long.signal + 0.02, 0), D=pmax(long.signal + 0.03, 0)),
  long.index
)
long.temperature <- zoo::zoo(
  cbind(A=10 + long.signal, B=11 + long.signal,
        C=12 + long.signal, D=13 + long.signal), long.index
)
long.streamflow <- zoo::zoo(
  cbind(A=10 + long.signal, B=11 + long.signal,
        C=12 + long.signal, D=13 + long.signal), long.index
)
long.precipitation.qc <- precipQC_spatial(
  long.precipitation, n.neighbours=3L, min.overlap=30L,
  min.correlation=-1, max.candidates=3L, selection.samples=256L
)
long.temperature.qc <- tempQC_spatial(
  long.temperature, group="month_hour", n.neighbours=3L,
  min.overlap=30L, min.group.overlap=5L, min.correlation=-1,
  max.candidates=3L, selection.samples=256L
)
long.streamflow.qc <- streamflowQC_spatial(
  long.streamflow, n.neighbours=3L, min.overlap=30L,
  min.correlation=-1, max.candidates=3L, selection.samples=256L
)
stopifnot(inherits(long.precipitation.qc$flags, "zoo"),
          inherits(long.temperature.qc$flags, "zoo"),
          inherits(long.streamflow.qc$flags, "zoo"),
          identical(dim(long.precipitation.qc$flags), c(long.rows, 4L)),
          identical(dim(long.temperature.qc$flags), c(long.rows, 4L)),
          identical(dim(long.streamflow.qc$flags), c(long.rows, 4L)),
          identical(colnames(long.precipitation.qc$flags), LETTERS[1:4]),
          identical(colnames(long.temperature.qc$flags), LETTERS[1:4]),
          identical(colnames(long.streamflow.qc$flags), LETTERS[1:4]))

# Coarser precipitation scales retain flags without unused diagnostics.
backend <- hydroTSM:::.QC_parallel_setup(1L, "serial", NULL, 4L)
flag.only <- hydroTSM:::.precipQC_spatial_core(
  small.precipitation, n.neighbours=3L, min.overlap=20L,
  min.correlation=-1, max.candidates=3L, selection.samples=20L,
  keep.diagnostics=FALSE, backend=backend
)
hydroTSM:::.QC_parallel_stop(backend)
stopifnot(identical(names(flag.only), "flags"),
          inherits(flag.only$flags, "zoo"))

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
streamflow.workflow <- streamflowQC(
  small.streamflow,
  checks=c(range=FALSE, duplicate=FALSE, gap=FALSE,
           climatology=FALSE, flatline=FALSE, spike=FALSE,
           rate=FALSE, highflow=FALSE, spatial=TRUE,
           breakpoint=FALSE),
  n.neighbours=3L, min.overlap=20L, min.correlation=-1,
  max.candidates=3L, selection.samples=20L,
  min.years=0, max.missing=1, max.review=1, max.suspicious=1,
  verbose=FALSE
)
stopifnot(identical(precipitation.workflow$settings$max.candidates, 3L),
          identical(precipitation.workflow$settings$selection.samples, 20L),
          identical(temperature.workflow$settings$max.candidates, 3L),
          identical(temperature.workflow$settings$selection.samples, 20L),
          identical(streamflow.workflow$settings$max.candidates, 3L),
          identical(streamflow.workflow$settings$selection.samples, 20L))

invalid.candidates <- try(precipQC_spatial(
  small.precipitation, n.neighbours=3L, max.candidates=2L
), silent=TRUE)
stopifnot(inherits(invalid.candidates, "try-error"))

invalid.streamflow.candidates <- try(streamflowQC_spatial(
  small.streamflow, n.neighbours=3L, max.candidates=2L
), silent=TRUE)
stopifnot(inherits(invalid.streamflow.candidates, "try-error"))
