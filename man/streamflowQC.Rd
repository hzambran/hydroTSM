%%File streamflowQC.Rd
%% Part of the hydroTSM R package

\name{streamflowQC}
\Rdversion{1.1}
\alias{streamflowQC}

\title{Frequency-aware quality control of streamflow time series}

\description{
Detects the sampling frequency of a numeric \code{zoo} object and dispatches
daily data to \code{\link{streamflowQC_daily}} or minute/hourly data to
\code{\link{streamflowQC_subdaily}}. No files are written.
}

\usage{
streamflowQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  area=NULL, elevation=NULL, elevation.scale=500
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object with one streamflow station per
column.}
  \item{metadata}{\code{NULL}, or a data.frame with one row per station.}
  \item{station.id}{Name of the station-identifier field in
\code{metadata}.}
  \item{coords}{Names of longitude and latitude fields in \code{metadata}, in
that order.}
  \item{...}{Arguments passed unchanged to the selected workflow.}
  \item{area}{\code{NULL}, or the name of a positive numeric catchment-area
field. Units may be chosen by the user but must be consistent among stations.}
  \item{elevation}{\code{NULL}, or the name of a numeric station-elevation
field.}
  \item{elevation.scale}{Positive elevation-decay scale used in spatial
neighbour ranking.}
}

\details{
The wrapper changes only the resolution-specific workflow. Test activation,
thresholds, station metadata, evidence requirements, correction policy, and
station recommendations remain explicit user controls. Daily and sub-daily
observations must be in consistent discharge units. Duplicate timestamps are
rejected because selecting among contradictory measurements would require
provenance or agency-quality information not contained in a numeric
\code{zoo} object.

The design combines the point plausibility and homogeneity principles of
Gudmundsson et al. (2018), the real-time hierarchy of U.S. IOOS (2018), and
the sub-daily artefact and event-corroboration framework of Fileni et al.
(2026).
}

\value{
An object of class \code{"streamflowQC"}; see
\code{\link{streamflowQC-class}}.
}

\references{
Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. \emph{EGUsphere preprint}.
doi:10.5194/egusphere-2026-277.

Gudmundsson, L., Do, H. X., Leonard, M. and Westra, S. (2018). The Global
Streamflow Indices and Metadata Archive (GSIM) - Part 2: quality control,
time-series indices and homogeneity assessment. \emph{Earth System Science
Data}, 10, 787--804. doi:10.5194/essd-10-787-2018.

U.S. Integrated Ocean Observing System (2018). \emph{Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0}. doi:10.25923/gszc-ha43.
}

\seealso{
\code{\link{streamflowQC_daily}}, \code{\link{streamflowQC_subdaily}},
\code{\link{streamflowQC_tests}}, \code{\link{precipQC}},
\code{\link{tempQC}}
}

\examples{
dates <- as.Date("2020-01-01") + 0:29
x <- zoo::zoo(cbind(A=10 + sin(seq_along(dates)),
                    B=12 + sin(seq_along(dates))), dates)
qc <- streamflowQC(
  x, checks=c(range=TRUE, duplicate=FALSE, gap=FALSE,
              climatology=FALSE, flatline=FALSE, spike=FALSE,
              rate=FALSE, highflow=FALSE, spatial=FALSE,
              breakpoint=FALSE),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1
)
qc$station.summary
}
