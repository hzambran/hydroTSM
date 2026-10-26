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
  elevation=NULL, elevation.scale=500, verbose=TRUE, ncores=1L,
  parallel.type=c("socket", "fork", "serial"), cluster.nodes=NULL
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object. Each column is one station.}
  \item{metadata, station.id, coords, elevation, elevation.scale}{Optional
station metadata, field-name selectors, and elevation-decay control passed to
the selected workflow. See \code{\link{tempQC_daily}}.}
  \item{...}{Arguments passed unchanged to the selected workflow.}
  \item{verbose}{Logical. If \code{TRUE}, informative messages report the
detected frequency, parallel workers/nodes, active QC stages, and final QC summary. If \code{FALSE},
these informational messages are suppressed; warnings and errors are
unaffected.}
  \item{ncores, parallel.type, cluster.nodes}{Requested worker count,
parallel backend, and optional PSOCK host names passed to the selected
workflow. See \code{\link{tempQC_daily}}.}
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
