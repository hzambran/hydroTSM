%%File streamflowQC_tests.Rd
%% Part of the hydroTSM R package

\name{streamflowQC_tests}
\Rdversion{1.1}
\alias{streamflowQC_tests}
\alias{streamflowQC_range}
\alias{streamflowQC_duplicate}
\alias{streamflowQC_gap}
\alias{streamflowQC_climatology}
\alias{streamflowQC_flatline}
\alias{streamflowQC_spike}
\alias{streamflowQC_rate}
\alias{streamflowQC_fluctuation}
\alias{streamflowQC_highflow}
\alias{streamflowQC_spatial}
\alias{streamflowQC_breakpoint}

\title{Individual quality-control tests for streamflow}

\description{
Individual tests called by \code{\link{streamflowQC_daily}} and
\code{\link{streamflowQC_subdaily}}. They are exported so each source of
evidence can be run, inspected, deactivated, and tuned independently.
}

\usage{
streamflowQC_range(x, lower=0, upper=Inf)

streamflowQC_duplicate(
  x, min.month.values=20L, min.year.values=300L, min.nonzero=1L
)

streamflowQC_gap(x, interval=NULL)

streamflowQC_climatology(
  x, group=c("dayofyear", "month", "month_slot"), window=5L,
  z=6, min.samples=30L, offset=0.01
)

streamflowQC_flatline(
  x, run=11L, high.run=Inf, high.prob=0.99,
  tolerance=0, flag.zero=FALSE
)

streamflowQC_spike(x, factor=5, prob=0.99)

streamflowQC_rate(
  x, group=c("month", "month_slot"), z=6, min.samples=30L,
  max.rise=Inf, max.fall=Inf, drop.factor=5, offset=0.01
)

streamflowQC_fluctuation(
  x, window=16L, minimum=8L, low.diff.prob=0.05
)

streamflowQC_highflow(
  x, log.z=6, amax.ratio=2, return.period=1000,
  min.years=10L, offset=0.01
)

streamflowQC_spatial(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  area=NULL, elevation=NULL, elevation.scale=500,
  n.neighbours=5L, max.distance=400, min.neighbours=2L,
  min.support=1L, min.overlap=90L, min.correlation=0.5,
  window=3L, high.prob=0.99, support.prob=0.95, f=3
)

streamflowQC_breakpoint(
  x, min.years=10L, alpha=0.01, min.relative.change=0.5,
  min.completeness=0.8,
  indicators=c("mean", "sd", "minimum", "maximum")
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object with one station per column.}
  \item{lower, upper}{Physical limits, scalar or station-specific.}
  \item{min.month.values, min.year.values}{Minimum paired calendar positions
for exact copied-month and copied-year comparisons.}
  \item{min.nonzero}{Minimum positive observations in a candidate copied
block.}
  \item{interval}{Expected interval: days for a \code{Date} index or seconds
for a \code{POSIXt} index. \code{NULL} uses the modal difference.}
  \item{group}{Seasonal grouping. \code{"month_slot"} preserves the calendar
month and hour:minute slot.}
  \item{window}{For climatology, circular calendar-day window width; for
fluctuation, number of observations; for spatial QC, observations before and
after the target event.}
  \item{z}{Standardized climatology or first-difference threshold.}
  \item{min.samples}{Minimum climatology or first-difference reference
sample.}
  \item{offset}{Positive constant added before taking logarithms.}
  \item{run, high.run}{Minimum ordinary and high-flow flat-line lengths in
observations.}
  \item{high.prob}{Target high-flow quantile.}
  \item{tolerance}{Maximum consecutive difference inside a flat line.}
  \item{flag.zero}{Whether repeated zero-flow runs may be flagged.}
  \item{factor}{Multiplicative isolated peak/dip threshold.}
  \item{prob}{Quantile used as the absolute spike threshold.}
  \item{max.rise, max.fall}{Optional absolute consecutive-change limits,
scalar or station-specific.}
  \item{drop.factor}{Multiplicative abrupt-fall threshold.}
  \item{minimum}{Number of sign reversals that a fluctuation window must
exceed.}
  \item{low.diff.prob}{Quantile of absolute changes treated as noise.}
  \item{log.z}{Global log-flow standard-deviation threshold.}
  \item{amax.ratio}{Largest/second-largest annual-maximum ratio threshold.}
  \item{return.period}{GEV return-period threshold in years.}
  \item{min.years}{Minimum annual values required for GEV or breakpoint
analysis.}
  \item{metadata}{\code{NULL}, or station metadata containing all stations.}
  \item{station.id}{Name of the unique station identifier field.}
  \item{coords}{Names of longitude and latitude fields, in that order.}
  \item{area}{\code{NULL}, or the name of a positive numeric catchment-area
field.}
  \item{elevation}{\code{NULL}, or the name of an optional numeric elevation
field.}
  \item{elevation.scale}{Positive elevation-decay scale used in neighbour
ranking.}
  \item{n.neighbours}{Maximum selected neighbours per station.}
  \item{max.distance}{Maximum geographic distance in km.}
  \item{min.neighbours}{Minimum simultaneous spatial predictions.}
  \item{min.support}{Minimum neighbours with a supporting high-flow event.}
  \item{min.overlap}{Minimum paired observations for selection and
regression.}
  \item{min.correlation}{Minimum preferred Spearman log-flow correlation.}
  \item{support.prob}{Neighbour high-flow quantile.}
  \item{f}{Minimum standardized spatial residual.}
  \item{alpha}{Holm-adjusted Pettitt-test significance level.}
  \item{min.relative.change}{Minimum relative pre/post median change.}
  \item{min.completeness}{Minimum daily completeness of an annual indicator.}
  \item{indicators}{Unique subset of \code{"mean"}, \code{"sd"},
\code{"minimum"}, and \code{"maximum"}.}
}

\details{
\code{streamflowQC_range} treats negative discharge and optional
station-specific upper violations as physical errors. Missing values are
handled by the workflow completeness calculation. Non-finite non-missing
values fail the range check.

\code{streamflowQC_duplicate} finds exactly copied calendar months or years.
It is intended for daily data; requiring a positive value prevents ordinary
all-zero intermittent-flow blocks from being flagged by default.

\code{streamflowQC_gap} marks the first finite observation after a missing
value or missing interval. It complements, rather than replaces, station
completeness. A numeric \code{zoo} cannot retain transmission syntax, so the
QARTOD syntax test is enforced structurally by the workflow validators.

\code{streamflowQC_climatology} applies the GSIM transformation
\code{log(Q + 0.01)} and mean plus/minus six standard deviations. Daily
references use a circular calendar-day window; sub-daily references may use
month or month-slot groups.

\code{streamflowQC_flatline} flags every member of an extended near-constant
run. Daily defaults implement more than 10 equal positive values from GSIM.
The sub-daily workflow converts the UK-Flow15-QC seven-day and one-day
high-flow rules from hours to input observations.

\code{streamflowQC_spike} requires an isolated centre value to change and
reverse by a factor of five or by more than the station 0.99 quantile.
\code{streamflowQC_rate} combines robust monthly log-difference screening,
optional absolute limits, and the five-fold rapid-drop rule.
\code{streamflowQC_fluctuation} flags windows with recurrent first-derivative
sign reversals after suppressing the smallest changes.

\code{streamflowQC_highflow} returns one combined flag plus separate
six-standard-deviation, AMAX-ratio, and GEV components. The GEV is fitted to
annual maxima by base-R maximum likelihood. The combined flag deliberately
counts as one evidence family because all three statistics describe the same
extreme event.

\code{streamflowQC_spatial} selects neighbours using correlation and optional
geographic/elevation proximity, fits target-on-neighbour regressions in
log-specific-discharge space when catchment area is available, and combines
predictions using inverse-error weighted medians. A rare target event is
flagged only when its residual score exceeds \code{f} and fewer than
\code{min.support} neighbours exceed their support quantile in the search
window.

\code{streamflowQC_breakpoint} aggregates sub-daily input to daily means,
calculates complete annual indicators, applies Pettitt rank tests, and
Holm-adjusts p-values. Its station flag is separate from point QC because an
inhomogeneity may represent real regulation or catchment change.
}

\value{
Range, duplicate, gap, climatology, flatline, spike, rate, and fluctuation
tests return a logical \code{zoo} matching \code{x}.

\code{streamflowQC_highflow} returns a list with combined \code{flags},
component flag objects, \code{return.period}, and station fit diagnostics.

\code{streamflowQC_spatial} returns a list with \code{flags}, standardized
\code{scores}, flow \code{estimate}, neighbour-event \code{support.count},
and selected \code{neighbours}.

\code{streamflowQC_breakpoint} returns one data.frame row per station.
}

\references{
Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. \emph{EGUsphere preprint}.
doi:10.5194/egusphere-2026-277.

Gudmundsson, L. et al. (2018). The Global Streamflow Indices and Metadata
Archive (GSIM) - Part 2: quality control, time-series indices and homogeneity
assessment. \emph{Earth System Science Data}, 10, 787--804.
doi:10.5194/essd-10-787-2018.

U.S. Integrated Ocean Observing System (2018). \emph{Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0}. doi:10.25923/gszc-ha43.

Zhao, Q. et al. (2018). Research on the Data-Driven Quality Control Method
of Hydrological Time Series Data. \emph{Water}, 10, 1712.
doi:10.3390/w10121712.
}

\seealso{
\code{\link{streamflowQC_daily}}, \code{\link{streamflowQC_subdaily}}
}

\examples{
dates <- as.Date("2020-01-01") + 0:19
x <- zoo::zoo(cbind(A=c(rep(10, 11), 11:19),
                    B=seq(5, 24)), dates)
streamflowQC_flatline(x)
streamflowQC_range(x)
}

