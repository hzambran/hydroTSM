%%File tempQC.Rd
%% Part of the hydroTSM R package

\name{tempQC}
\Rdversion{1.1}
\alias{tempQC}

\title{Frequency-aware quality control of air-temperature time series}

\description{
Detects the sampling frequency of a numeric \code{zoo} object with
\code{\link{sfreq}} and dispatches it to \code{\link{tempQC_daily}} or
\code{\link{tempQC_subdaily}}. No files are written.
}

\usage{
tempQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  elevation=NULL, elevation.scale=500,
  max.candidates=200L, selection.samples=2048L,
  verbose=TRUE, ncores=1L,
  parallel.type=c("socket", "fork", "serial"), cluster.nodes=NULL,
  from=start(x), to=end(x)
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object. Each column is one station.}
  \item{metadata, station.id, coords, elevation, elevation.scale}{Optional
station metadata, field-name selectors, and elevation-decay control passed to
the selected workflow. See \code{\link{tempQC_daily}}.}
  \item{...}{Arguments passed unchanged to the selected workflow. Because the
daily and sub-daily functions have resolution-specific arguments, only
arguments accepted by the detected workflow should be supplied. This includes
\code{max.suspicious}, the maximum proportion of finite observations that may
be confirmed as suspicious before a station is recommended for discard; see
\code{\link{tempQC_daily}} and \code{\link{tempQC_subdaily}}.}
  \item{max.candidates, selection.samples}{Candidate and time-sample
bounds passed to the selected workflow. See \code{\link{tempQC_daily}}.}
  \item{verbose}{Logical. If \code{TRUE}, informative messages report the
detected frequency, parallel workers/nodes, active QC stages, and final QC summary. If \code{FALSE},
these informational messages are suppressed; warnings and errors are
unaffected.}
  \item{ncores}{Positive integer giving the total requested number of worker
R processes, across all machines rather than per machine. The effective number
is capped at the number of station columns. \code{ncores=1L} uses the serial
path and does not create a cluster. See \code{\link{tempQC_daily}}.}
  \item{parallel.type}{Parallel backend passed to the selected workflow:
\code{"socket"}, \code{"fork"}, or \code{"serial"}. Network hosts can be
used only with \code{"socket"}. See \code{\link{tempQC_daily}}.}
  \item{cluster.nodes}{\code{NULL} to place all socket workers on the local
machine, or a character vector of host names on which PSOCK workers will be
started. This argument selects worker locations; it does not set their number.
Host names are recycled to the effective \code{ncores}. See
\code{\link{tempQC_daily}}.}
  \item{from, to}{Starting and ending index bounds used to subset \code{x}
before frequency detection and QC. See \code{\link{tempQC_daily}}.}
}

\details{
Daily input is sent to \code{tempQC_daily}; minute or hourly input is sent to
\code{tempQC_subdaily}. Other frequencies are rejected explicitly. Automatic
dispatch changes only the workflow: test activation, thresholds, metadata,
correction policy, and station recommendation criteria remain under user
control. Progress is reported with \code{message()}, so it can be captured or
suppressed independently of the returned result. The staged design follows the
gross-error, tolerance, temporal,
inter-variable, and inter-station QC categories described by Brunet et al.
(2020).
The default \code{ncores=1L} is serial and preserves the historical behaviour.
When \code{checks} is not supplied through \code{...}, spatial QC is enabled
by default only when \code{metadata} is supplied.
Spatial QC caches compact geographic/elevation candidate lists and symmetric
pair statistics, uses vectorized sufficient-statistic regressions, and
processes station-time prediction calculations in bounded row blocks. With a
PSOCK backend, large read-only data are broadcast once per worker. Setting
\code{max.candidates=Inf} or \code{selection.samples=Inf} requests the
corresponding exhaustive legacy-style selection.
}

\value{
An object of class \code{"tempQC"}; see \code{\link{tempQC-class}}.
}

\references{
Brunet, M. et al. (2020). \emph{Best Practice Guidelines for Climate Data
and Metadata Formatting, Quality Control and Submission}. Copernicus Climate
Change Service.
}

\seealso{
\code{\link{tempQC_daily}}, \code{\link{tempQC_subdaily}},
\code{\link{tempQC_tests}}, \code{\link{precipQC}}
}

\examples{
dates <- as.Date("2020-01-01") + 0:29
x <- zoo::zoo(cbind(A=10 + sin(seq_along(dates)),
                    B=11 + sin(seq_along(dates))), dates)
qc <- tempQC(x, checks=c(range=TRUE, duplicate=FALSE,
                          climatology=FALSE, persistence=FALSE,
                          step=FALSE, spike=FALSE, spatial=FALSE,
                          breakpoint=FALSE), min.years=0)
qc$station.summary
}
