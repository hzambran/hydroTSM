%%File precipQC.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{precipQC}
\Rdversion{1.1}
\alias{precipQC}

\title{Frequency-aware quality control of precipitation time series}

\description{
Detects the sampling frequency of a precipitation \code{zoo} object with
\code{\link{sfreq}} and dispatches to \code{\link{precipQC_daily}} or
\code{\link{precipQC_subdaily}}. No files are written.
}

\usage{
precipQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  elevation=NULL, elevation.scale=500, verbose=TRUE, ncores=1L,
  parallel.type=c("socket", "fork", "serial"), cluster.nodes=NULL,
  from=start(x), to=end(x)
)
}

\arguments{
  \item{x}{
A univariate or multivariate numeric \code{zoo} object. Each column is one
precipitation station and must have a unique name.
}
  \item{metadata, station.id, coords, elevation, elevation.scale}{
Optional station metadata, field-name selectors, and elevation-decay control
passed to the selected workflow. See \code{\link{precipQC_daily}}.
}
  \item{...}{
Arguments passed unchanged to the selected QC function. Because the daily and
sub-daily functions have resolution-specific arguments, only arguments
accepted by the detected workflow should be supplied.
}
  \item{verbose}{
Logical. If \code{TRUE}, informative messages report the detected frequency,
the active QC stages, parallel workers/nodes, spatial scales, and the final QC summary. If
\code{FALSE}, these informational messages are suppressed; warnings and errors
are unaffected.
}
  \item{ncores, parallel.type, cluster.nodes}{
Parallel worker count, backend, and optional PSOCK host names passed to the
selected workflow. See \code{\link{precipQC_daily}}.
}
  \item{from, to}{
Starting and ending index bounds used to subset \code{x} before frequency
detection and QC. They default to the complete record. See
\code{\link{precipQC_daily}}.
}
}

\details{
\code{sfreq(x)} returning \code{"daily"} selects
\code{precipQC_daily(x, ...)}. Values \code{"minute"} and \code{"hourly"}
select \code{precipQC_subdaily(x, ...)}. Weekly, monthly, quarterly, annual,
or other frequencies are rejected with an informative error because the
implemented thresholds and tests are not valid at those resolutions. Progress
is emitted with \code{message()}, so it can be captured or suppressed without
redirecting returned values.
The default \code{ncores=1L} is serial and preserves the historical behaviour.
When \code{checks} is not supplied through \code{...}, spatial QC is enabled
by default only when \code{metadata} is supplied.
}

\value{
An object of class \code{"precipQC"}; see \code{\link{precipQC-class}}.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{sfreq}}, \code{\link{precipQC_daily}},
\code{\link{precipQC_subdaily}}, \code{\link{plot.precipQC}}
}

\examples{
dates <- seq(as.Date("2020-01-01"), by="day", length.out=40)
x <- zoo(cbind(A=rep(0, 40), B=rep(0, 40)), dates)

qc <- precipQC(
  x, min.years=0,
  checks=c(climatology=FALSE, spatial=FALSE, dryspell=FALSE,
           breakpoint=FALSE)
)
qc$settings$resolution
}

\keyword{ts}
\keyword{manip}
