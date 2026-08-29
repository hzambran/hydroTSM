%%File tempQC_tests.Rd
%% Part of the hydroTSM R package

\name{tempQC_tests}
\Rdversion{1.1}
\alias{tempQC_tests}
\alias{tempQC_range}
\alias{tempQC_duplicate}
\alias{tempQC_persistence}
\alias{tempQC_climatology}
\alias{tempQC_step}
\alias{tempQC_spike}
\alias{tempQC_spatial}
\alias{tempQC_internal}
\alias{tempQC_breakpoint}

\title{Individual quality-control tests for air temperature}

\description{
Individual tests called by \code{\link{tempQC_daily}} and
\code{\link{tempQC_subdaily}}. They are exported so each source of evidence
can be run, inspected, and tuned independently.
}

\usage{
tempQC_range(x, lower=-89.4, upper=57.7)

tempQC_duplicate(x, min.month.values=20L, min.year.values=300L)

tempQC_persistence(x, run=7L, tolerance=0)

tempQC_climatology(
  x, group=c("dayofyear", "month", "month_hour"),
  window=15L, z=6, min.samples=30L
)

tempQC_step(
  x, group=c("month", "month_hour"), z=3,
  min.samples=20L, max.rise=Inf, max.fall=Inf
)

tempQC_spike(x, threshold=25)

tempQC_spatial(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  group=c("month", "month_hour"), n.neighbours=5L,
  max.distance=200, min.neighbours=2L, min.overlap=90L,
  min.group.overlap=20L, min.correlation=0.6,
  f=4, min.difference=8, min.se=0.1, elevation=NULL,
  elevation.scale=500
)

tempQC_internal(tmin, tmax, lagged=TRUE, max.range=40)

tempQC_breakpoint(
  x, min.years=10L, alpha=0.05, min.shift=1,
  min.completeness=0.8, indicators=c("mean", "sd")
)
}

\arguments{
  \item{x}{A numeric \code{zoo} object with one station per column.}
  \item{lower, upper}{Physical limits, scalar or station-specific.}
  \item{min.month.values, min.year.values}{Minimum complete paired positions
for copied-month and copied-year comparisons.}
  \item{run}{Minimum flat-line window length in observations.}
  \item{tolerance}{Maximum within-window temperature range.}
  \item{group}{Calendar grouping. \code{"month_hour"} uses month and integer
hour of day.}
  \item{window}{Circular calendar-day window width.}
  \item{z}{Absolute robust standardized climatology or first-difference
threshold.}
  \item{min.samples}{Minimum climatological or first-difference reference
sample.}
  \item{max.rise, max.fall}{Optional absolute consecutive-change limits.}
  \item{threshold}{Absolute difference from both adjacent values required by
the isolated spike/dip test.}
  \item{metadata}{\code{NULL}, or station metadata. When supplied, the
selected identifier, longitude, and latitude fields are required.}
  \item{station.id}{Name of the metadata station identifier column.}
  \item{coords}{Names of numeric longitude and latitude metadata columns in
decimal degrees, in that order.}
  \item{elevation}{\code{NULL}, or the name of an optional numeric metadata
column containing elevation in metres.}
  \item{elevation.scale}{Positive elevation-decay scale in metres used to
penalize neighbours with large height differences.}
  \item{n.neighbours}{Maximum selected spatial-regression neighbours.}
  \item{max.distance}{Maximum target-neighbour distance in km.}
  \item{min.neighbours}{Minimum simultaneous valid regression estimates.}
  \item{min.overlap}{Minimum overall paired observations for neighbour
selection.}
  \item{min.group.overlap}{Minimum paired observations in a model group.}
  \item{min.correlation}{Minimum target-neighbour Pearson correlation.}
  \item{f}{Standardized residual threshold for spatial regression.}
  \item{min.difference}{Absolute spatial residual threshold.}
  \item{min.se}{Positive lower bound on each regression standard error.}
  \item{tmin, tmax}{Numeric \code{zoo} objects with identical time indices and
station columns containing paired minimum and maximum temperatures.}
  \item{lagged}{Whether to apply previous/current/following-day logical tests.}
  \item{max.range}{Maximum allowed lagged Tmin--Tmax separation.}
  \item{min.years}{Minimum complete annual indicators for breakpoint testing.}
  \item{alpha}{Holm-adjusted breakpoint significance level.}
  \item{min.shift}{Minimum absolute pre/post median change in degrees Celsius.}
  \item{min.completeness}{Minimum daily completeness of an annual indicator.}
  \item{indicators}{Unique subset of \code{"mean"}, \code{"sd"},
\code{"minimum"}, and \code{"maximum"}.}
}

\details{
\code{tempQC_range} is a gross-error test using the world-record screening
limits compiled by Durre et al. (2010). Limits are intentionally configurable.

\code{tempQC_duplicate} compares corresponding calendar positions and flags
complete months or years that are copied exactly. Durre et al. (2010) identify
such duplication as a keying, transmission, or processing failure.

\code{tempQC_persistence} moves a window through each station and flags every
observation in windows whose range does not exceed \code{tolerance}. Exact
seven-day flat lines follow Feng et al. (2004); the adjustable tolerance and
duration support sub-daily sensor tests such as Beele et al. (2022).

\code{tempQC_climatology} calculates a resistant median and MAD reference.
Daily tests use a circular day-of-year window; sub-daily tests can preserve the
seasonal and diurnal cycles with month-hour groups. The six-score default is
based on Durre et al. (2010), with the resistant formulation motivated by Feng
et al. (2004).

\code{tempQC_step} robustly standardizes consecutive changes within calendar
groups and can also apply absolute rise/fall limits. This is the seasonal
rate-of-change structure of Hubbard et al. (2007), extended to hour-specific
groups following Cheng et al. (2016).

\code{tempQC_spike} requires the centre value to differ from both neighbours
by at least \code{threshold} in the same direction. Its daily default is the
25-degree spike/dip rule of Durre et al. (2010).

\code{tempQC_spatial} fits target-on-neighbour linear regressions separately
by calendar group, retains the lowest proximity-adjusted-error eligible
neighbours, and combines their predictions with geographic/elevation
proximity and inverse-error-variance weights. The pairwise regression
intercept estimates systematic station offsets, including persistent height
effects, without imposing a fixed lapse rate. A flag requires both
an absolute and a standardized residual. The equations follow the spatial
regression test of Hubbard et al. (2007) and Estevez et al. (2018); grouping
and correlation screening incorporate findings of Cheng et al. (2016) and
Xiong et al. (2022, 2024).

\code{tempQC_internal} flags Tmin above Tmax, possible interchange relative to
the adjacent three-day window, and the 40-degree lagged range conditions in
Durre et al. (2010). It returns separate flags because the erroneous member of
an inconsistent pair is not identifiable automatically.

\code{tempQC_breakpoint} aggregates sub-daily data to daily means, calculates
complete annual indicators, applies Pettitt rank tests, and adjusts p-values
across indicators. Boulanger et al. (2010) and Brunet et al. (2020) motivate
reporting homogeneity separately from point QC.
}

\value{
The range, duplicate, persistence, climatology, step, and spike tests return a
logical \code{zoo} object matching \code{x}. \code{tempQC_spatial} returns a
list with logical \code{flags}, numeric \code{scores}, numeric
\code{estimate}, and selected \code{neighbours}. \code{tempQC_internal}
returns a list of Tmin and Tmax flag objects. \code{tempQC_breakpoint} returns
one data.frame row per station.
}

\references{
Beele, E. et al. (2022). Quality control and correction method for air
temperature data from a citizen science weather station network.
\emph{Earth System Science Data}, 14, 4681--4717.

Boulanger, J.-P. et al. (2010). A procedure for automated quality control and
homogenization of historical daily temperature and precipitation data.
\emph{Climatic Change}, 98, 471--491.

Brunet, M. et al. (2020). \emph{Best Practice Guidelines for Climate Data
and Metadata Formatting, Quality Control and Submission}. Copernicus Climate
Change Service.

Cheng, A. R. et al. (2016). Quality control program for real-time hourly
temperature observation in Taiwan. \emph{Journal of Atmospheric and Oceanic
Technology}, 33, 953--965.

Durre, I. et al. (2010). Comprehensive automated quality assurance of daily
surface observations. \emph{Journal of Applied Meteorology and Climatology},
49, 1615--1633.

Estevez, J. et al. (2018). Spatial regression test for ensuring temperature
data quality in southern Spain. \emph{Theoretical and Applied Climatology},
131, 309--318.

Feng, S., Hu, Q. and Qian, W. (2004). Quality control of daily meteorological
data in China, 1951--2000. \emph{International Journal of Climatology}, 24,
853--870.

Hubbard, K. G. et al. (2007). An improved QC process for temperature in the
daily cooperative weather observations. \emph{Journal of Atmospheric and
Oceanic Technology}, 24, 206--213.

Xiong, X. et al. (2022). Research on quality control methods for surface
temperature observations via spatial correlation analysis.
\emph{International Journal of Climatology}. doi:10.1002/joc.7897.

Xiong, X. et al. (2024). Research on quality control method of surface
temperature observations for complex physical geography.
\emph{Journal of Atmospheric and Oceanic Technology}, 41, 803--815.
}

\seealso{
\code{\link{tempQC_daily}}, \code{\link{tempQC_subdaily}}
}

\examples{
dates <- as.Date("2020-01-01") + 0:9
x <- zoo::zoo(cbind(A=c(rep(10, 7), 11, 12, 13)), dates)
tempQC_persistence(x)
tempQC_range(x)
}
