%%File precipQC_daily.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{precipQC_daily}
\Rdversion{1.1}
\alias{precipQC_daily}

\title{Quality control of daily precipitation time series}

\description{
Applies a conservative, multi-test quality-control workflow to one or more
daily precipitation series in a \code{zoo} object. Individual observations are
flagged for review or rejection, stations are recommended for acceptance or
discarding, and no files are written.
}

\usage{
precipQC_daily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, frequency=TRUE, gap=TRUE,
           climatology=TRUE, persistence=TRUE, accumulation=TRUE,
           weekday=TRUE, spatial=TRUE, dryspell=TRUE, breakpoint=TRUE),
  lower=0, upper=1825, wet.threshold=0.1,
  duplicate.min.month=20L, duplicate.min.year=300L,
  duplicate.min.nonzero=3L,
  frequency.window=10L, frequency.min.samples=20L,
  gap.threshold=300, gap.min.samples=30L,
  climatology.window=15L, climatology.prob=0.999,
  climatology.z=8, climatology.min.samples=30L,
  persistence.threshold=10, persistence.run=5L,
  weekday.min.wet=20L, weekday.alpha=0.001,
  weekday.ratio=0.5,
  spatial.days=c(1L, 3L, 7L), spatial.cr=3,
  n.neighbours=10L, max.distance=400, min.neighbours=2L,
  min.overlap=30L, min.correlation=0,
  dryspell.days=15L, neighbour.wet.days=3L,
  neighbour.fraction=1,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500
)
}

\arguments{
  \item{x}{
A \code{zoo} object with a daily \code{Date} or \code{POSIXt} index. Each
column is one precipitation station and must have a unique name. Values are
assumed to be precipitation depths in mm per day.
}
  \item{metadata}{
\code{NULL}, or a data.frame with one row per station. Extra columns are
preserved in the accepted and discarded metadata outputs. When supplied, it
must contain the identifier, longitude, and latitude fields selected by
\code{station.id} and \code{coords}. Rows may be in any order and extra
stations are allowed. When \code{NULL}, spatial tests retain the
correlation-only behaviour and generated station metadata are returned.
}
  \item{station.id}{
Character string naming the metadata column that contains the column names of
\code{x}.
}
  \item{coords}{
Two character strings naming longitude and latitude columns in
\code{metadata}, in that order. Both columns must be numeric and expressed
in decimal degrees. Individual entries may be \code{NA}; a station has a
usable location only when both entries are present. Non-missing longitude and
latitude values must be finite and lie in \code{[-180, 180]} and
\code{[-90, 90]}, respectively.
}
  \item{elevation}{
\code{NULL}, or a character string naming an optional numeric metadata column
of station elevations in metres. It is used only when explicitly selected.
}
  \item{elevation.scale}{
Positive elevation-decay scale in metres. With elevation metadata, candidate
neighbours and their spatial weights are penalized by
\code{exp(-abs(dz) / elevation.scale)}. A smaller value gives elevation
difference more influence. The argument has no effect when
\code{elevation=NULL}.
}
  \item{checks}{
Named logical vector. Any subset of \code{"range"}, \code{"climatology"},
\code{"duplicate"}, \code{"frequency"}, \code{"gap"},
\code{"persistence"}, \code{"accumulation"}, \code{"weekday"},
\code{"spatial"}, \code{"dryspell"}, and \code{"breakpoint"} can be supplied
to override the default of running every check.
}
  \item{duplicate.min.month, duplicate.min.year, duplicate.min.nonzero}{
Minimum complete paired month positions, complete paired year positions, and
non-zero totals in both blocks for copied-block detection.
}
  \item{frequency.window, frequency.min.samples}{
Number of consecutive non-zero reports examined for clustered identical values
and minimum calendar-month wet sample used for their percentile screen.
}
  \item{gap.threshold, gap.min.samples}{
Minimum adjacent separation in a sorted calendar-month wet distribution and
minimum wet sample required for that check.
}
  \item{lower, upper}{
Physical lower and upper limits, in mm/day. The default upper limit is the
historical 24-hour world record used by Hamada et al. (2011). Regional limits
should be supplied when authoritative values are available.
}
  \item{wet.threshold}{
Depth in mm separating dry and wet observations.
}
  \item{climatology.window}{
Odd or even positive integer giving the width, in calendar days, of the
moving day-of-year climatological window.
}
  \item{climatology.prob}{
Upper empirical probability used by the climatological test.
}
  \item{climatology.z}{
Number of robust standard deviations above the median on the
\code{log1p}-transformed scale.
}
  \item{climatology.min.samples}{
Minimum number of observations needed to estimate a climatological limit.
}
  \item{persistence.threshold}{
Minimum repeated non-zero value, in mm/day, to which
\code{persistence.run} is applied.
}
  \item{persistence.run}{
Minimum run length for repeated values at or above
\code{persistence.threshold}. The paper-based default flags five or more
identical values of at least 10 mm/day.
}
  \item{weekday.min.wet}{
Minimum number of wet days required in a station-year before the weekday
false-zero test is attempted.
}
  \item{weekday.alpha}{
Significance level for the exposure-adjusted chi-squared comparison of wet-day
occurrence among the seven weekdays.
}
  \item{weekday.ratio}{
Maximum ratio of observed to expected wet days used to identify an
under-recorded weekday after the overall weekday test is significant. It must
be in \code{(0, 1)}.
}
  \item{spatial.days}{
Positive integers giving the rolling accumulation durations, in days, at
which spatial consistency is tested. A flag at an accumulated endpoint is
expanded to every contributing daily value.
}
  \item{spatial.cr}{
Critical ratio for transformed observed-minus-neighbour-estimate residuals.
The default follows the initial value used by El Hachem et al. (2022).
}
  \item{n.neighbours}{
Maximum number of neighbours used for each target station.
}
  \item{max.distance}{
Maximum station-neighbour distance in km when coordinates are available.
}
  \item{min.neighbours}{
Minimum number of simultaneous valid neighbours needed for a spatial
estimate.
}
  \item{min.overlap}{
Minimum number of paired observations required when selecting neighbours.
}
  \item{min.correlation}{
Minimum Spearman correlation preferred when selecting neighbours. When too
few preferred neighbours remain, the best eligible neighbours are used.
}
  \item{dryspell.days}{
Length of a target-station dry window to compare with neighbours.
}
  \item{neighbour.wet.days}{
Number of wet days a neighbour must have during a target-station dry window.
}
  \item{neighbour.fraction}{
Fraction of available neighbours that must corroborate a suspicious dry
window. It must be in \code{(0, 1]}.
}
  \item{correction}{
Correction policy. \code{"none"} (the default) preserves all observations;
\code{"set_na"} replaces confirmed suspicious observations by \code{NA};
\code{"spatial"} uses the native-scale spatial estimate when available and
\code{NA} otherwise.
}
  \item{min.evidence}{
Number of independent non-hard flags required to reject an observation.
Range, copied-block, clustered frequent-value, distribution-gap, persistence,
accumulated-total, and spatially corroborated dry-spell flags are treated as
hard evidence. A value with less evidence remains in the
\code{suspicious} table with action \code{"review"}.
}
  \item{max.missing}{
Maximum missing fraction allowed for a station recommendation of
\code{"accept"}. Missing timestamps inside the record span are included.
}
  \item{max.suspicious}{
Maximum fraction of confirmed suspicious observations allowed for a station
recommendation of \code{"accept"}.
}
  \item{min.years}{
Minimum record length in years required for a station recommendation of
\code{"accept"}. The default is intended for general QC; analyses of extremes
usually require substantially longer records.
}
  \item{discard.breakpoint}{
Logical. If \code{TRUE}, a large significant breakpoint also causes a station
recommendation of \code{"discard"}. It is \code{FALSE} by default because an
apparent change may be climatic rather than instrumental.
}
}

\details{
The workflow combines tests that address different error mechanisms instead
of treating one statistical anomaly as proof of error.

\enumerate{
  \item \emph{Physical range}: negative depths and values above a defensible
  record limit are impossible or require review. Record limits are screening
  limits, not estimates of local return levels. This is the gross-error stage
  used by Hamada et al. (2011) and Estevez et al. (2022).

  \item \emph{Climatological plausibility}: precipitation is transformed with
  \code{log1p} to reduce positive skew. Within a moving 15-day calendar
  window, an upper limit combines an empirical quantile with a robust
  median/MAD limit. The largest exceedance is removed iteratively before the
  limit is recomputed. This is a robust adaptation of the calendar-window
  outlier test of Hamada et al. (2011), consistent with the robust and
  month-aware limits discussed by Stepanek et al. (2009), Estevez et al.
  (2022), and Golkhatmi and Farzandi (2024).

  \item \emph{Persistence}: identical non-zero values repeated for several
  days may indicate a stuck sensor, copied record, or truncated measurement.
  The defaults reproduce the daily criterion in Hamada et al. (2011), while
  Vicente-Serrano et al. (2010) and Estevez et al. (2022) independently use
  repeated-value screening in daily databases.

  \item \emph{Copied blocks, clustered identical values, and distribution
  gaps}: complete identical years or months, tiered frequent identical values
  among ten non-zero reports, and a wet-distribution tail separated by at
  least 300 mm are flagged as hard recording-integrity evidence. These tests
  reproduce the daily precipitation duplicate, frequent-value, and gap checks
  of Durre et al. (2010), with explicit minimum samples to avoid declaring
  sparse blocks identical.

  \item \emph{Accumulated totals}: a large value immediately after missing
  days is flagged as a possible multi-day accumulation. Automatic
  redistribution is not attempted because the period and total must be known
  reliably. Scherrer et al. (2011) redistributed known accumulated totals
  proportionally to spatial estimates.

  \item \emph{Systematic weekday false zeros}: within each station-year, the
  distribution of wet days is compared with the weekday exposure. If the
  distribution is highly inconsistent and a weekday records at most the
  requested fraction of its expected wet days, dry values on that weekday are
  flagged for review. This operationalizes the weekday-zero quality component
  of Estevez et al. (2022), with exposure adjustment to avoid mistaking
  uneven missingness for under-recording.

  \item \emph{Multi-scale spatial consistency}: each station is removed in
  turn, its transformed precipitation is estimated from up to ten nearby
  stations, and the residual is standardized by local and historical robust
  dispersion. The test is repeated for 1-, 3-, and 7-day rolling totals by
  default. Geographic distance limits the candidate set. If elevation is
  supplied, height similarity also affects neighbour ranking and the
  inverse-distance weights, which avoids treating a nearby station across a
  large orographic contrast as equally representative. A station missing
  either coordinate remains testable: its neighbours are selected by temporal
  correlation. For a located target, eligible located neighbours are preferred
  and unlocated neighbours are conservative fallbacks that are not subjected
  to \code{max.distance} because their distance is unknown. This combines the
  neighbour logic of Scherrer et al. (2011) with
  the multi-aggregation critical-ratio idea of El Hachem et al. (2022).
Relative candidate/reference comparisons are also supported by Stepanek et
  al. (2009) and Vicente-Serrano et al. (2010).

  \item \emph{Spatially corroborated dry spells}: a long target-station dry
  period is flagged only when the selected neighbours repeatedly record wet
  days, adapting the GSDR-QC dry-neighbour rule of Lewis et al. (2021) and the
  suspect-zero comparisons of Vicente-Serrano et al. (2010) and Golkhatmi and
  Farzandi (2024).

  \item \emph{Homogeneity}: Pettitt rank tests are applied to sufficiently
  complete annual precipitation totals, wet-day counts, maxima, and counts
  above the station's wet-day 99th percentile. P-values are Holm-adjusted and
  the strongest diagnostic is reported. The multi-indicator design follows
  Vicente-Serrano et al. (2010); the cautious interpretation and use of
  multiple evidence follows Stepanek et al. (2009). The breakpoint is
  diagnostic by default because station moves, equipment changes, and true
  climatic changes can produce similar signals.
}

Convective precipitation can be genuinely local. Therefore a spatial flag by
itself is labelled for review, and default correction is disabled. Confirmed
rejection requires either a hard physical/recording test or at least
\code{min.evidence} coincident flags. Correlation-only results for stations
without coordinates should be interpreted more cautiously because geographic
proximity cannot be verified.
}

\value{
An object of class \code{"precipQC"}, which is a list containing:
\itemize{
  \item \code{accepted.metadata} and \code{discarded.metadata}: station
  metadata plus QC diagnostics and the recommendation reason;
  \item \code{accepted.data}: unmodified daily series for accepted stations;
  \item \code{accepted.corrected}: accepted-station series after the requested
  correction policy;
  \item \code{suspicious}: one row per flagged station-time value, including
  the tests, evidence count, action, spatial estimate, and correction;
  \item \code{corrections}: an audit table of changed values;
  \item \code{flags}: named logical \code{zoo} objects returned by the
  individual tests;
  \item \code{flag.count}, \code{rejected}, \code{station.summary},
  \code{breakpoint}, spatial diagnostics, neighbours, and settings.
}
}

\references{
Durre, I., Menne, M. J., Gleason, B. E., Houston, T. G. and Vose, R. S.
(2010). Comprehensive automated quality assurance of daily surface
observations. \emph{Journal of Applied Meteorology and Climatology}, 49,
1615--1633.

Hamada, A., Arakawa, O. and Yatagai, A. (2011). An automated quality
control method for daily rain-gauge data. \emph{Global Environmental
Research}, 15, 183--192.

Scherrer, S. C., Begert, M., Croci-Maspoli, M. and Appenzeller, C. (2011).
Operational quality control of daily precipitation using
spatio-climatological plausibility testing. \emph{Meteorologische
Zeitschrift}, 20, 397--407.

El Hachem, A. et al. (2022). Space-time statistical quality control of
extreme precipitation observations. \emph{Hydrology and Earth System
Sciences}, 26, 6137--6146.

Lewis, E. et al. (2021). Quality control of a global hourly rainfall
dataset. \emph{Environmental Modelling and Software}, 144, 105169.

Stepanek, P., Zahradnicek, P. and Skalak, P. (2009). Data quality control and
homogenization of air temperature and precipitation series in the area of the
Czech Republic in the period 1961--2007. \emph{Advances in Science and
Research}, 3, 23--26.

Vicente-Serrano, S. M., Begueria, S., Lopez-Moreno, J. I., Garcia-Vera, M. A.
and Stepanek, P. (2010). A complete daily precipitation database for northeast
Spain: reconstruction, quality control, and homogeneity. \emph{International
Journal of Climatology}, 30, 1146--1163.

Estevez, J., Llabres-Brustenga, A., Casas-Castillo, M. C., Garcia-Marin, A. P.,
Kirchner, R. and Rodriguez-Sola, R. (2022). A quality control procedure for
long-term series of daily precipitation data in a semiarid environment.
\emph{Theoretical and Applied Climatology}, 149, 1029--1041.

Golkhatmi, N. S. N. and Farzandi, M. (2024). Enhancing rainfall data
consistency and completeness: a spatiotemporal quality control approach and
missing data reconstruction using MICE on large precipitation datasets.
\emph{Water Resources Management}, 38, 815--833.

Serrano-Notivoli, R. and Tejedor, E. (2021). From rain to data: a review of the
creation of monthly and daily station-based gridded precipitation datasets.
\emph{WIREs Water}, 8, e1555.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{precipQC}}, \code{\link{precipQC_subdaily}},
\code{\link{precipQC_range}}, \code{\link{precipQC_weekday}},
\code{\link{precipQC_spatial}}, \code{\link{plot.precipQC}},
\code{\link[zoo]{zoo}}
}

\examples{
set.seed(1)
dates <- seq(as.Date("2018-01-01"), by="day", length.out=400)
values <- matrix(stats::rgamma(length(dates) * 3, 0.4, 1),
                 ncol=3)
values[values < 1] <- 0
values[20, 1] <- -1
colnames(values) <- c("A", "B", "C")
pcp <- zoo(values, dates)

meta <- data.frame(code=colnames(pcp),
                   longitude=c(-71.2, -71.1, -71.0),
                   latitude=c(-33.2, -33.1, -33.0),
                   altitude_m=c(100, 250, 400))

qc <- precipQC_daily(pcp, metadata=meta, station.id="code",
                     coords=c("longitude", "latitude"),
                     elevation="altitude_m", min.years=0,
                     checks=c(breakpoint=FALSE))
qc$station.summary
head(qc$suspicious)
plot(qc)
}

\keyword{ts}
\keyword{manip}
