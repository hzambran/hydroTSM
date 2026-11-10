# Benchmark the bounded spatial engines with a reproducible 2,000-station case.
library(hydroTSM)

arguments <- commandArgs(trailingOnly=TRUE)
ncores <- if (length(arguments) >= 1L) as.integer(arguments[1L]) else 1L
parallel.type <- if (length(arguments) >= 2L) arguments[2L] else
                 if (ncores > 1L) "socket" else "serial"

set.seed(20261110)
nstations <- 2000L
nrows <- 365L
stations <- sprintf("S%04d", seq_len(nstations))
dates <- seq(as.Date("2020-01-01"), by="day", length.out=nrows)
metadata <- data.frame(
  id=stations, lon=runif(nstations, -75, -70),
  lat=runif(nstations, -40, -35), stringsAsFactors=FALSE
)

# Generate correlated-enough synthetic precipitation and temperature fields.
precipitation <- matrix(rexp(nrows * nstations, rate=1 / 3),
                        nrow=nrows, ncol=nstations,
                        dimnames=list(NULL, stations))
precipitation[matrix(runif(length(precipitation)) < 0.7,
                     nrow=nrows)] <- 0
season <- 12 + 10 * sin(2 * pi * seq_len(nrows) / 365)
temperature <- outer(season, rep(1, nstations)) +
               matrix(rnorm(nrows * nstations, sd=2), nrow=nrows)
colnames(temperature) <- stations

precipitation.time <- system.time(precipitation.qc <- precipQC_spatial(
  zoo(precipitation, dates), metadata=metadata, station.id="id",
  ncores=ncores, parallel.type=parallel.type
))
temperature.time <- system.time(temperature.qc <- tempQC_spatial(
  zoo(temperature, dates), metadata=metadata, station.id="id",
  min.group.overlap=10L, ncores=ncores, parallel.type=parallel.type
))

stopifnot(NCOL(precipitation.qc$flags) == nstations,
          NCOL(temperature.qc$flags) == nstations)
print(rbind(precipitation=precipitation.time,
            temperature=temperature.time))
