%%File streamflowQC_subdaily.Rd
%% Part of the hydroTSM R package

\name{streamflowQC_subdaily}
\Rdversion{1.1}
\alias{streamflowQC_subdaily}

\title{Quality control of sub-daily streamflow time series}

\description{
Applies individually selectable physical, temporal, high-flow, spatial, and
homogeneity tests to one or more regular minute/hourly streamflow series. It
returns point flags and station acceptance recommendations without writing
files.
}

\usage{
streamflowQC_subdaily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, gap=TRUE, climatology=TRUE, flatline=TRUE,
           spike=TRUE, rate=TRUE, fluctuation=TRUE, highflow=TRUE,
           spatial=TRUE, breakpoint=TRUE),
  lower=0, upper=Inf,
  climatology.z=6, climatology.min.samples=100L, log.offset=0.01,
  flatline.hours=168, high.flatline.hours=24,
  flatline.high.prob=0.99, flatline.tolerance=0, flag.zero=FALSE,
  spike.factor=5, spike.prob=0.99,
  rate.z=6, rate.min.samples=100L,
  rate.max.rise=Inf, rate.max.fall=Inf, rate.drop.factor=5,
  fluctuation.window=16L, fluctuation.minimum=8L,
  fluctuation.low.diff.prob=0.05,
  highflow.log.z=6, highflow.amax.ratio=2,
  highflow.return.period=1000, highflow.min.years=10L,
  spatial.window.hours=72, spatial.high.prob=0.99,
  spatial.support.prob=0.95, spatial.f=3,
  n.neighbours=5L, max.distance=200, min.neighbours=2L,
  min.support=1L, min.overlap=100L, min.correlation=0.5,
  breakpoint.min.years=10L, breakpoint.alpha=0.01,
  breakpoint.min.relative.change=0.5,
  breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=3L, max.missing=0.2, max.review=0.2,
  max.suspicious=0.05, min.years=1, discard.breakpoint=FALSE,
  area=NULL, elevation=NULL, elevation.scale=500
)
}

\arguments{
  \item{x}{A numeric regular sub-daily \code{zoo} object with a
\code{POSIXt} index and one station per column.}
  \item{metadata}{\code{NULL}, or a data.frame containing every station in
\code{x}. Extra fields are retained.}
  \item{station.id}{Name of the unique metadata station-identifier field.}
  \item{coords}{Names of longitude and latitude fields, in that order.
Individual coordinates may be missing.}
  \item{checks}{Named logical vector. Omitted names retain their defaults;
all sub-daily tests are enabled by default.}
  \item{lower, upper}{Physical limits, scalar or station-specific. The
default rejects negative flow without imposing a region-specific maximum.}
  \item{climatology.z}{Standard-deviation multiplier for monthly log-flow.}
  \item{climatology.min.samples}{Minimum monthly reference observations.}
  \item{log.offset}{Positive constant added before taking logarithms.}
  \item{flatline.hours}{Duration in hours of a low/ordinary-flow flat line.}
  \item{high.flatline.hours}{Shorter flat-line duration applied above the
station high-flow quantile.}
  \item{flatline.high.prob}{High-flow quantile for the shorter flat-line
rule.}
  \item{flatline.tolerance}{Maximum consecutive numerical difference within
a flat line.}
  \item{flag.zero}{Whether repeated zero-flow runs may be flagged.}
  \item{spike.factor}{Multiplicative isolated peak/dip threshold.}
  \item{spike.prob}{Station quantile used as the absolute isolated-spike
threshold.}
  \item{rate.z}{Robust standardized first-difference threshold.}
  \item{rate.min.samples}{Minimum monthly first-difference reference sample.}
  \item{rate.max.rise, rate.max.fall}{Optional absolute consecutive rise and
fall limits, scalar or station-specific.}
  \item{rate.drop.factor}{Multiplicative abrupt-fall threshold.}
  \item{fluctuation.window}{Number of observations in the moving
sign-reversal window.}
  \item{fluctuation.minimum}{Number of derivative sign reversals that the
window must exceed.}
  \item{fluctuation.low.diff.prob}{Quantile of absolute changes treated as
measurement noise before counting reversals.}
  \item{highflow.log.z}{Standard-deviation multiplier for unusually high
global log-flow.}
  \item{highflow.amax.ratio}{Ratio of the largest annual maximum to the
second largest that generates a flag.}
  \item{highflow.return.period}{GEV return-period threshold in years.}
  \item{highflow.min.years}{Minimum annual maxima required for GEV fitting.}
  \item{spatial.window.hours}{Hours before and after a rare event searched
for neighbouring support.}
  \item{spatial.high.prob, spatial.support.prob}{Target and neighbour
high-flow quantiles.}
  \item{spatial.f}{Minimum standardized spatial-regression residual.}
  \item{n.neighbours}{Maximum selected neighbours per station.}
  \item{max.distance}{Maximum target-neighbour distance in km.}
  \item{min.neighbours}{Minimum simultaneous spatial predictions.}
  \item{min.support}{Minimum neighbouring high-flow events needed for
corroboration.}
  \item{min.overlap}{Minimum paired observations for neighbour selection and
regression.}
  \item{min.correlation}{Minimum preferred Spearman log-flow correlation.}
  \item{breakpoint.min.years}{Minimum complete annual indicators for a
Pettitt test.}
  \item{breakpoint.alpha}{Holm-adjusted breakpoint significance level.}
  \item{breakpoint.min.relative.change}{Minimum relative pre/post median
change.}
  \item{breakpoint.min.completeness}{Minimum daily completeness for an annual
indicator.}
  \item{correction}{\code{"none"} preserves values, \code{"set_na"} removes
confirmed points, and \code{"spatial"} uses an available spatial estimate and
otherwise \code{NA}.}
  \item{min.evidence}{Independent flag families needed to confirm a
non-hard-failure point.}
  \item{max.missing, max.review, max.suspicious}{Maximum station fractions of
missing expected values, any flagged observations, and confirmed suspicious
observations.}
  \item{min.years}{Minimum record duration for station acceptance.}
  \item{discard.breakpoint}{Whether a significant large breakpoint discards
a station.}
  \item{area}{\code{NULL}, or the name of a positive numeric catchment-area
field used to compare specific discharge spatially.}
  \item{elevation}{\code{NULL}, or an optional numeric elevation field.}
  \item{elevation.scale}{Positive elevation-decay scale used in neighbour
ranking.}
}

\details{
The defaults generalize the 15-minute UK-Flow15-QC rules to the actual modal
interval. Thus, ordinary flat lines use 168 hours and high-flow flat lines use
24 hours rather than fixed counts of 672 and 96. Relative spikes and abrupt
falls use a factor of five; absolute spikes use the station 0.99 quantile; and
rapid fluctuations use more than eight sign reversals over 16 observations.

High-flow diagnostics include unusually large log-flow, an annual-maximum
ratio, and a base-R maximum-likelihood GEV fit after at least 10 years. These
correlated diagnostics contribute one evidence family. A spatial flag
requires a rare target event, a large leave-one-station-out regression
residual, and insufficient high-flow support among selected neighbours within
the default 72-hour window. Catchment area, coordinates, and elevation are
optional; correlation-based spatial comparison remains available without
metadata.

Only physical range failure is hard evidence by default. The three-family
default reduces the risk of rejecting genuine flash floods, reservoir
operations, tidal behaviour, intermittent zeros, or high-baseflow plateaus.
The review fraction can still recommend discarding a station with pervasive
single-test artefacts.
}

\value{
An object of class \code{"streamflowQC"}; see
\code{\link{streamflowQC-class}}.
}

\references{
Fileni, F., Fowler, H. J., Lewis, E., McLay, F. and Yang, L. (2023). A
quality-control framework for sub-daily flow and level data for hydrological
modelling in Great Britain. \emph{Hydrology Research}, 54, 1357--1367.
doi:10.2166/nh.2023.045.

Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. \emph{EGUsphere preprint}.
doi:10.5194/egusphere-2026-277.

U.S. Integrated Ocean Observing System (2018). \emph{Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0}. doi:10.25923/gszc-ha43.

Zhao, Q. et al. (2018). Research on the Data-Driven Quality Control Method
of Hydrological Time Series Data. \emph{Water}, 10, 1712.
doi:10.3390/w10121712.
}

\seealso{
\code{\link{streamflowQC_daily}}, \code{\link{streamflowQC}},
\code{\link{streamflowQC_tests}}
}

\examples{
times <- seq(as.POSIXct("2020-01-01", tz="UTC"),
             by="hour", length.out=72)
x <- zoo::zoo(cbind(A=10 + sin(seq_along(times) / 5),
                    B=12 + sin(seq_along(times) / 5)), times)
x[20, "A"] <- -1
qc <- streamflowQC_subdaily(
  x, checks=c(climatology=FALSE, flatline=FALSE, spike=FALSE,
              rate=FALSE, fluctuation=FALSE, highflow=FALSE,
              spatial=FALSE, breakpoint=FALSE),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1
)
qc$suspicious
}

