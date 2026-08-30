%%File tempQC_subdaily.Rd
%% Part of the hydroTSM R package

\name{tempQC_subdaily}
\Rdversion{1.1}
\alias{tempQC_subdaily}

\title{Quality control of sub-daily air-temperature time series}

\description{
Applies physical, climatological, temporal, spatial, and homogeneity checks to
minute or hourly air-temperature station series in a \code{zoo} object. The
method preserves the native time index and writes no files.
}

\usage{
tempQC_subdaily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, climatology=TRUE, persistence=TRUE,
           step=TRUE, spike=TRUE, spatial=TRUE, breakpoint=TRUE),
  lower=-89.4, upper=57.7,
  climatology.z=6, climatology.min.samples=30L,
  persistence.hours=3, persistence.tolerance=0.05,
  step.z=5, step.min.samples=30L,
  step.max.rise=Inf, step.max.fall=Inf,
  spike.threshold=12,
  n.neighbours=5L, max.distance=50, min.neighbours=2L,
  min.overlap=100L, min.group.overlap=20L,
  min.correlation=0.7, spatial.f=4, spatial.min.difference=5,
  breakpoint.min.years=10L, breakpoint.alpha=0.05,
  breakpoint.min.shift=1, breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object with a strictly increasing POSIXt index
at minute or hourly resolution. Each column is a station in degrees Celsius.}
  \item{metadata}{\code{NULL}, or station metadata with one row per column.
When supplied, it must include the selected identifier, longitude, and
latitude fields.}
  \item{station.id}{Name of the station identifier column in \code{metadata}.}
  \item{coords}{Names of longitude and latitude metadata columns in decimal
degrees, in that order. Individual entries may be \code{NA}; non-missing
values are range-checked.}
  \item{elevation}{\code{NULL}, or the name of an optional numeric elevation
column in metres.}
  \item{elevation.scale}{Positive height-decay scale in metres used in
neighbour selection and weighting when \code{elevation} is supplied.}
  \item{checks}{Named logical vector that overrides any of \code{"range"},
\code{"climatology"}, \code{"persistence"}, \code{"step"},
\code{"spike"}, \code{"spatial"}, and \code{"breakpoint"}.}
  \item{lower, upper}{Physical screening limits, scalar or station-specific.}
  \item{climatology.z}{Absolute robust anomaly threshold within each
calendar-month and hour-of-day group.}
  \item{climatology.min.samples}{Minimum group sample size.}
  \item{persistence.hours}{Duration of an almost-flat period. The corresponding
number of observations is derived from the modal sampling interval.}
  \item{persistence.tolerance}{Maximum temperature range within that period.}
  \item{step.z}{Robust standardized threshold for consecutive changes within
each month and hour.}
  \item{step.min.samples}{Minimum first-difference sample per group.}
  \item{step.max.rise, step.max.fall}{Optional absolute consecutive rise and
fall limits; use regional, interval-specific values when available.}
  \item{spike.threshold}{Minimum difference from both adjacent observations
for an isolated spike or dip.}
  \item{n.neighbours, max.distance, min.neighbours}{Maximum neighbour count,
maximum distance in km, and minimum simultaneous estimates.}
  \item{min.overlap, min.group.overlap}{Minimum overall and month-hour paired
samples for neighbour selection and regression fitting.}
  \item{min.correlation}{Minimum eligible target-neighbour Pearson
correlation.}
  \item{spatial.f}{Minimum standardized spatial residual.}
  \item{spatial.min.difference}{Minimum absolute spatial residual in degrees
Celsius. Both spatial thresholds must be exceeded.}
  \item{breakpoint.min.years, breakpoint.alpha, breakpoint.min.shift,
breakpoint.min.completeness}{Minimum annual sample size, Holm-adjusted
significance level, minimum absolute median shift, and annual completeness for
the homogeneity diagnostic.}
  \item{correction}{\code{"none"}, \code{"set_na"}, or \code{"spatial"}; see
\code{\link{tempQC_daily}}.}
  \item{min.evidence}{Number of coincident non-hard tests needed to reject a
point. Range, persistence, and isolated-spike flags are hard evidence.}
  \item{max.missing, max.suspicious, min.years}{Station acceptance limits.}
  \item{discard.breakpoint}{Whether a flagged breakpoint should cause a
station discard recommendation.}
}

\details{
The sub-daily workflow uses the same evidence and audit model as
\code{\link{tempQC_daily}}, but its climatological, first-difference, and
spatial-regression reference groups are conditioned jointly on calendar month
and hour of day. Cheng et al. (2016) showed that hourly reference-station
relations can change sharply by hour and season, especially across complex
terrain.

\enumerate{
  \item \emph{Range}: global physical limits screen gross coding and unit
  errors. Local limits may be supplied; elevation-adjusted regional limits are
  preferable where established (Cheng et al., 2016).

  \item \emph{Climatology}: robust median/MAD limits within month-hour groups
  preserve the diurnal and seasonal cycle (Cheng et al., 2016; Brunet et al.,
  2020).

  \item \emph{Persistence}: the default identifies a temperature range no
  greater than 0.05 degrees across three hours, adapting the 10-minute flat-line
  test of Beele et al. (2022) to the detected sampling interval.

  \item \emph{Steps and isolated spikes}: month-hour first differences are
  standardized robustly. Optional absolute rise and fall limits support local
  real-time criteria. Cheng et al. (2016) demonstrate why weather- and
  hour-dependent limits are safer than a universal hourly threshold; Beele et
  al. (2022) provide an operational 10-minute example.

  \item \emph{Spatial regression}: correlated neighbours are chosen by
  proximity-adjusted regression error and combined with proximity and
  inverse-error-variance weights. Geographic distance and, when supplied,
  elevation difference define proximity. A target missing either coordinate
  uses the correlation/regression fallback, while a located target prefers
  eligible located neighbours. Models
  are fitted separately by month and hour. This implements the SRT family
  evaluated by Hubbard et al. (2007), Cheng et al. (2016), and Estevez et al.
  (2018), while the correlation screen addresses the terrain sensitivity
  discussed by Xiong et al. (2022, 2024).

  \item \emph{Homogeneity}: native values are converted to daily means before
  annual mean and variability breakpoint diagnostics. The result is not a
  point flag and does not cause discard unless explicitly requested.
}
}

\value{
An object of class \code{"tempQC"}; see \code{\link{tempQC-class}}.
}

\references{
Beele, E., Reyniers, M., Aerts, R. and Somers, B. (2022). Quality control and
correction method for air temperature data from a citizen science weather
station network in Leuven, Belgium. \emph{Earth System Science Data}, 14,
4681--4717.

Brunet, M. et al. (2020). \emph{Best Practice Guidelines for Climate Data
and Metadata Formatting, Quality Control and Submission}. Copernicus Climate
Change Service.

Cheng, A. R., Lee, T. H., Ku, H. I. and Chen, Y. W. (2016). Quality control
program for real-time hourly temperature observation in Taiwan.
\emph{Journal of Atmospheric and Oceanic Technology}, 33, 953--965.

Estevez, J., Gavilan, P. and Garcia-Marin, A. P. (2018). Spatial regression
test for ensuring temperature data quality in southern Spain.
\emph{Theoretical and Applied Climatology}, 131, 309--318.

Hubbard, K. G., Guttman, N. B., You, J. and Chen, Z. (2007). An improved QC
process for temperature in the daily cooperative weather observations.
\emph{Journal of Atmospheric and Oceanic Technology}, 24, 206--213.

Xiong, X. et al. (2022). Research on quality control methods for surface
temperature observations via spatial correlation analysis.
\emph{International Journal of Climatology}. doi:10.1002/joc.7897.

Xiong, X. et al. (2024). Research on quality control method of surface
temperature observations for complex physical geography.
\emph{Journal of Atmospheric and Oceanic Technology}, 41, 803--815.
}

\seealso{
\code{\link{tempQC}}, \code{\link{tempQC_daily}},
\code{\link{tempQC_tests}}, \code{\link{plot.tempQC}}
}

\examples{
times <- seq(as.POSIXct("2020-01-01", tz="UTC"), by="hour", length.out=72)
h <- as.integer(format(times, "\%H"))
v <- 12 + 5 * sin(2 * pi * h / 24)
x <- zoo::zoo(cbind(A=v, B=v + 0.2), times)
qc <- tempQC_subdaily(
  x, checks=c(range=TRUE, climatology=FALSE, persistence=TRUE,
              step=FALSE, spike=TRUE, spatial=FALSE,
              breakpoint=FALSE),
  min.years=0, max.missing=1
)
qc$station.summary
}
