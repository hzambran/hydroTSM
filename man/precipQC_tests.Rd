%%File precipQC_tests.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{precipQC_tests}
\Rdversion{1.1}
\alias{precipQC_tests}
\alias{precipQC_range}
\alias{precipQC_persistence}
\alias{precipQC_duplicate}
\alias{precipQC_frequency}
\alias{precipQC_gap}
\alias{precipQC_weekday}
\alias{precipQC_climatology}
\alias{precipQC_accumulation}
\alias{precipQC_spatial}
\alias{precipQC_dryspell}
\alias{precipQC_breakpoint}

\title{Individual precipitation quality-control tests}

\description{
Individual tests used by \code{\link{precipQC_daily}} and
\code{\link{precipQC_subdaily}}. They are exported so that each source of QC
evidence can be inspected or tuned independently.
}

\usage{
precipQC_range(x, lower=0, upper=Inf)

precipQC_persistence(
  x, high.threshold=10, high.run=5L,
  long.run=Inf, wet.threshold=0.1
)

precipQC_duplicate(
  x, min.month.values=20L, min.year.values=300L,
  min.nonzero=3L, wet.threshold=0.1
)

precipQC_frequency(
  x, window=10L, counts=c(9L, 8L, 7L, 5L),
  probs=c(0.3, 0.5, 0.7, 0.9),
  min.samples=20L, wet.threshold=0.1
)

precipQC_gap(x, gap=300, min.samples=30L, wet.threshold=0.1)

precipQC_weekday(
  x, wet.threshold=0.1, min.wet.days=20L,
  alpha=0.001, underreporting.ratio=0.5
)

precipQC_climatology(
  x, group=c("dayofyear", "month"), window=15L,
  prob=0.999, z=8, min.samples=30L, wet.threshold=0.1
)

precipQC_accumulation(
  x, interval.hours, preceding.hours=23, following.hours=23,
  threshold.factor=2, wet.threshold=0.1, missing.only=FALSE
)

precipQC_spatial(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  n.neighbours=10L, max.distance=Inf, min.neighbours=2L,
  min.overlap=30L, min.correlation=0, cr.threshold=3,
  wet.threshold=0.1, elevation=NULL, elevation.scale=500
)

precipQC_dryspell(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  n.neighbours=10L, max.distance=Inf, window.days=15L,
  neighbour.wet.days=3L, neighbour.fraction=1,
  wet.threshold=0.1, elevation=NULL, elevation.scale=500
)

precipQC_breakpoint(
  x, wet.threshold=0.1, min.years=5L, alpha=0.05,
  min.relative.change=0.5, min.completeness=0.8,
  indicators=c("total", "wet.days", "maximum", "extreme.days")
)
}

\arguments{
  \item{x}{
A numeric \code{zoo} object. Each column is one station. Spatial and dry-spell
tests require at least two neighbours for the default spatial calculation and
at least one neighbour for the dry-spell calculation.
}
  \item{lower, upper}{
Physical lower and upper precipitation limits. Each may be scalar or contain
one value per station.
}
  \item{high.threshold}{
Scalar or station-specific lower bound for repeated high values.
}
  \item{high.run}{
Minimum number of consecutive identical high values.
}
  \item{long.run}{
Minimum run length for any repeated wet value, or \code{Inf} to disable the
long-run component.
}
  \item{min.month.values, min.year.values}{
Minimum number of complete corresponding calendar positions for copied-month
and copied-year comparisons.
}
  \item{min.nonzero}{
Minimum non-zero values required in both blocks before precipitation
duplication is flagged.
}
  \item{counts, probs}{
Paired repeated-value counts and minimum climatological probabilities for the
clustered frequent-value rules.
}
  \item{gap}{
Minimum separation in mm between adjacent sorted wet values in one calendar
month distribution.
}
  \item{min.wet.days}{
Minimum annual number of wet days required for the weekday test.
}
  \item{underreporting.ratio}{
Maximum observed-to-expected wet-day ratio used to identify an under-recorded
weekday after the overall weekday occurrence test is significant.
}
  \item{wet.threshold}{
Precipitation depth that separates dry and wet observations.
}
  \item{group}{
Climatological grouping: a moving calendar-day window or calendar months.
}
  \item{window}{
Calendar-day window width. It is ignored for monthly grouping.
}
  \item{prob, z, min.samples}{
Empirical upper probability, robust standard-deviation multiplier on the
\code{log1p} scale, and required reference sample size. For frequent-value and
gap checks only \code{min.samples} is used.
}
  \item{interval.hours}{
Native time-step length in hours for the accumulation test.
}
  \item{preceding.hours, following.hours}{
Durations on either side of a possible accumulated total that must be dry or
missing.
}
  \item{threshold.factor}{
Multiplier of station mean wet-day precipitation used as the lower screen for
an accumulated total.
}
  \item{missing.only}{
Logical. If \code{TRUE}, only preceding missing values or an index gap satisfy
the accumulation test; if \code{FALSE}, preceding dry values are required.
}
  \item{metadata}{
\code{NULL}, or a data.frame with one row per station. When supplied, the
selected identifier, longitude, and latitude fields are required.
}
  \item{station.id}{
Name of the metadata station-identifier column.
}
  \item{coords}{
Names of numeric longitude and latitude columns in decimal degrees, in that
order.
}
  \item{elevation}{
\code{NULL}, or the name of an optional numeric elevation column in metres.
}
  \item{elevation.scale}{
Positive elevation-decay scale in metres. Height similarity is
\code{exp(-abs(dz) / elevation.scale)} and influences neighbour ranking and
spatial weights. It is ignored when \code{elevation=NULL}.
}
  \item{n.neighbours}{
Maximum number of neighbours.
}
  \item{max.distance}{
Maximum great-circle distance in km when coordinates are available.
}
  \item{min.neighbours}{
Minimum simultaneous valid neighbours for a spatial estimate.
}
  \item{min.overlap}{
Minimum paired observations for neighbour selection.
}
  \item{min.correlation}{
Preferred minimum Spearman correlation between target and neighbour.
}
  \item{cr.threshold}{
Critical ratio for the absolute transformed residual divided by robust local
and historical dispersion.
}
  \item{window.days}{
Length of target-station dry windows.
}
  \item{neighbour.wet.days}{
Minimum number of wet days during the same window at a neighbour.
}
  \item{neighbour.fraction}{
Fraction of selected neighbours that must meet
\code{neighbour.wet.days}.
}
  \item{min.years}{
Minimum number of annual wet-period medians for the Pettitt test.
}
  \item{alpha}{
Significance level for the weekday test or the Holm-adjusted Pettitt p-value.
}
  \item{min.relative.change}{
Minimum absolute relative difference between pre- and post-break medians.
}
  \item{min.completeness}{
Minimum fraction of expected daily observations required to retain an annual
indicator in breakpoint testing.
}
  \item{indicators}{
Unique subset of \code{"total"}, \code{"wet.days"}, \code{"maximum"}, and
\code{"extreme.days"} to test for breakpoints.
}
}

\details{
\code{precipQC_range} flags values outside physical limits.

\code{precipQC_persistence} run-length encodes each series. Repeated high
values and exceptionally long runs of any wet value are flagged. Exact
repetition is intentional because this test targets copied, stuck, or
artificially disaggregated records.

\code{precipQC_duplicate} compares corresponding positions in entire years,
different months of the same year, and the same calendar month in different
years. Both blocks must be complete over the comparison and contain at least
\code{min.nonzero} wet values. This implements the precipitation duplicate
checks of Durre et al. (2010).

\code{precipQC_frequency} scans consecutive non-zero reports, skipping dry and
missing observations, for the tiered 9/10, 8/10, 7/10, and 5/10 identical-value
rules of Durre et al. (2010). The repeated amount must also exceed its
calendar-month climatological percentile, which reduces false positives from
measurement resolution.

\code{precipQC_gap} sorts wet observations separately by calendar month. When
an adjacent gap is at least \code{gap}, it flags the upper tail. The 300-mm
default follows the daily precipitation distribution-gap test of Durre et al.
(2010).

\code{precipQC_weekday} requires at most one observation per calendar day and
compares station-year wet-day counts among weekdays,
after adjusting the expected counts for the number of available observations
on each weekday. Only when the overall chi-squared test is highly significant
does it flag dry values on strongly under-recorded weekdays. It is intended to
detect false zeros caused by systematic missed manual observations.
\code{precipQC_subdaily} applies it after coverage-screened daily aggregation
and expands the resulting flags back to native intervals.

\code{precipQC_climatology} works on \code{log1p} precipitation. For each
calendar group, it takes the more conservative of an upper empirical quantile
and a robust median-plus-MAD threshold. A largest value above the limit is
temporarily removed and the threshold recomputed until stable. This protects
the reference distribution from gross errors while retaining a low false
positive rate.

\code{precipQC_accumulation} first calculates the mean wet-day total of each
station. A candidate must exceed \code{threshold.factor} times that mean and
have the requested dry/missing flanks. It identifies candidates only; it
cannot prove that a reported depth is a longer-period accumulation.

\code{precipQC_spatial} applies a leave-one-station-out test after
\code{log1p} transformation. Coordinates, overlap, and correspondence select
up to ten neighbours. If elevation is selected, its exponential similarity
factor penalizes neighbours separated by large height differences. The
estimate is a distance- and elevation-weighted median when metadata exist and
an ordinary median otherwise. The critical ratio uses
the larger of the simultaneous neighbour dispersion, historical target
residual dispersion, and a small numerical floor. Only positive residuals are
flagged here; isolated dryness is handled separately.

\code{precipQC_dryspell} aggregates native observations to wet/dry calendar
days, locates long dry runs, and flags target values only where the requested
fraction of neighbours records enough wet days in the same window.

\code{precipQC_breakpoint} first aggregates sub-daily input to daily totals.
It applies the rank-based Pettitt statistic separately to sufficiently
complete annual precipitation totals, wet-day counts, maxima, and counts above
the station wet-day 99th percentile. P-values are Holm-adjusted across the
selected indicators. The strongest breakpoint, its adjusted p-value,
pre/post relative change, and number of significant indicators are returned
because a statistically significant change is not necessarily a measurement
error.
}

\value{
\code{precipQC_range}, \code{precipQC_persistence},
\code{precipQC_duplicate}, \code{precipQC_frequency}, \code{precipQC_gap},
\code{precipQC_weekday}, \code{precipQC_climatology}, \code{precipQC_accumulation}, and
\code{precipQC_dryspell} return logical \code{zoo} objects matching
\code{x}.

\code{precipQC_spatial} returns a list with logical \code{flags}, numeric
\code{scores}, neighbour \code{estimate} series, and selected
\code{neighbours}.

\code{precipQC_breakpoint} returns one data.frame row per station with the
breakpoint year, selected indicator, adjusted p-value, relative change,
number of significant indicators, and logical flag.
}

\references{
Hamada, A., Arakawa, O. and Yatagai, A. (2011). An automated quality
control method for daily rain-gauge data. \emph{Global Environmental
Research}, 15, 183--192.

Scherrer, S. C. et al. (2011). Operational quality control of daily
precipitation using spatio-climatological plausibility testing.
\emph{Meteorologische Zeitschrift}, 20, 397--407.

El Hachem, A. et al. (2022). Space-time statistical quality control of
extreme precipitation observations. \emph{Hydrology and Earth System
Sciences}, 26, 6137--6146.

Durre, I., Menne, M. J., Gleason, B. E., Houston, T. G. and Vose, R. S.
(2010). Comprehensive automated quality assurance of daily surface
observations. \emph{Journal of Applied Meteorology and Climatology}, 49,
1615--1633.

Lewis, E. et al. (2021). Quality control of a global hourly rainfall dataset.
\emph{Environmental Modelling and Software}, 144, 105169.

Stepanek, P., Zahradnicek, P. and Skalak, P. (2009). Data quality control and
homogenization of air temperature and precipitation series in the area of the
Czech Republic in the period 1961--2007. \emph{Advances in Science and
Research}, 3, 23--26.

Vicente-Serrano, S. M., Begueria, S., Lopez-Moreno, J. I., Garcia-Vera, M. A.
and Stepanek, P. (2010). A complete daily precipitation database for northeast
Spain: reconstruction, quality control, and homogeneity. \emph{International
Journal of Climatology}, 30, 1146--1163.

Estevez, J. et al. (2022). A quality control procedure for long-term series of
daily precipitation data in a semiarid environment. \emph{Theoretical and
Applied Climatology}, 149, 1029--1041.

Golkhatmi, N. S. N. and Farzandi, M. (2024). Enhancing rainfall data
consistency and completeness: a spatiotemporal quality control approach and
missing data reconstruction using MICE on large precipitation datasets.
\emph{Water Resources Management}, 38, 815--833.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{precipQC_daily}}, \code{\link{precipQC_subdaily}},
\code{\link{plot.precipQC}}
}

\examples{
dates <- seq(as.Date("2020-01-01"), by="day", length.out=60)
x <- zoo(cbind(A=rep(0, 60), B=rep(0, 60), C=rep(0, 60)), dates)
x[10, "A"] <- -1
x[20:24, "B"] <- 15

precipQC_range(x)
precipQC_persistence(x)
precipQC_weekday(x)
precipQC_breakpoint(x)
}

\keyword{ts}
\keyword{manip}
