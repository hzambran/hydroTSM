# Individual precipitation quality-control tests

Individual tests used by
[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md)
and
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md).
They are exported so that each source of QC evidence can be inspected or
tuned independently.

## Usage

``` r
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
```

## Arguments

- x:

  A numeric `zoo` object. Each column is one station. Spatial and
  dry-spell tests require at least two neighbours for the default
  spatial calculation and at least one neighbour for the dry-spell
  calculation.

- lower, upper:

  Physical lower and upper precipitation limits. Each may be scalar or
  contain one value per station.

- high.threshold:

  Scalar or station-specific lower bound for repeated high values.

- high.run:

  Minimum number of consecutive identical high values.

- long.run:

  Minimum run length for any repeated wet value, or `Inf` to disable the
  long-run component.

- min.month.values, min.year.values:

  Minimum number of complete corresponding calendar positions for
  copied-month and copied-year comparisons.

- min.nonzero:

  Minimum non-zero values required in both blocks before precipitation
  duplication is flagged.

- counts, probs:

  Paired repeated-value counts and minimum climatological probabilities
  for the clustered frequent-value rules.

- gap:

  Minimum separation in mm between adjacent sorted wet values in one
  calendar month distribution.

- min.wet.days:

  Minimum annual number of wet days required for the weekday test.

- underreporting.ratio:

  Maximum observed-to-expected wet-day ratio used to identify an
  under-recorded weekday after the overall weekday occurrence test is
  significant.

- wet.threshold:

  Precipitation depth that separates dry and wet observations.

- group:

  Climatological grouping: a moving calendar-day window or calendar
  months.

- window:

  Calendar-day window width. It is ignored for monthly grouping.

- prob, z, min.samples:

  Empirical upper probability, robust standard-deviation multiplier on
  the `log1p` scale, and required reference sample size. For
  frequent-value and gap checks only `min.samples` is used.

- interval.hours:

  Native time-step length in hours for the accumulation test.

- preceding.hours, following.hours:

  Durations on either side of a possible accumulated total that must be
  dry or missing.

- threshold.factor:

  Multiplier of station mean wet-day precipitation used as the lower
  screen for an accumulated total.

- missing.only:

  Logical. If `TRUE`, only preceding missing values or an index gap
  satisfy the accumulation test; if `FALSE`, preceding dry values are
  required.

- metadata:

  `NULL`, or a data.frame with one row per station. When supplied, the
  selected identifier, longitude, and latitude fields are required.

- station.id:

  Name of the metadata station-identifier column.

- coords:

  Names of numeric longitude and latitude columns in decimal degrees, in
  that order. Individual entries may be `NA`; non-missing values are
  range-checked.

- elevation:

  `NULL`, or the name of an optional numeric elevation column in metres.

- elevation.scale:

  Positive elevation-decay scale in metres. Height similarity is
  `exp(-abs(dz) / elevation.scale)` and influences neighbour ranking and
  spatial weights. It is ignored when `elevation=NULL`.

- n.neighbours:

  Maximum number of neighbours.

- max.distance:

  Maximum great-circle distance in km when coordinates are available.
  Pairs for which either station lacks a coordinate are not
  distance-filtered.

- min.neighbours:

  Minimum simultaneous valid neighbours for a spatial estimate.

- min.overlap:

  Minimum paired observations for neighbour selection.

- min.correlation:

  Preferred minimum Spearman correlation between target and neighbour.

- cr.threshold:

  Critical ratio for the absolute transformed residual divided by robust
  local and historical dispersion.

- window.days:

  Length of target-station dry windows.

- neighbour.wet.days:

  Minimum number of wet days during the same window at a neighbour.

- neighbour.fraction:

  Fraction of selected neighbours that must meet `neighbour.wet.days`.

- min.years:

  Minimum number of annual wet-period medians for the Pettitt test.

- alpha:

  Significance level for the weekday test or the Holm-adjusted Pettitt
  p-value.

- min.relative.change:

  Minimum absolute relative difference between pre- and post-break
  medians.

- min.completeness:

  Minimum fraction of expected daily observations required to retain an
  annual indicator in breakpoint testing.

- indicators:

  Unique subset of `"total"`, `"wet.days"`, `"maximum"`, and
  `"extreme.days"` to test for breakpoints.

## Details

`precipQC_range` flags values outside physical limits.

`precipQC_persistence` run-length encodes each series. Repeated high
values and exceptionally long runs of any wet value are flagged. Exact
repetition is intentional because this test targets copied, stuck, or
artificially disaggregated records.

`precipQC_duplicate` compares corresponding positions in entire years,
different months of the same year, and the same calendar month in
different years. Both blocks must be complete over the comparison and
contain at least `min.nonzero` wet values. This implements the
precipitation duplicate checks of Durre et al. (2010).

`precipQC_frequency` scans consecutive non-zero reports, skipping dry
and missing observations, for the tiered 9/10, 8/10, 7/10, and 5/10
identical-value rules of Durre et al. (2010). The repeated amount must
also exceed its calendar-month climatological percentile, which reduces
false positives from measurement resolution.

`precipQC_gap` sorts wet observations separately by calendar month. When
an adjacent gap is at least `gap`, it flags the upper tail. The 300-mm
default follows the daily precipitation distribution-gap test of Durre
et al. (2010).

`precipQC_weekday` requires at most one observation per calendar day and
compares station-year wet-day counts among weekdays, after adjusting the
expected counts for the number of available observations on each
weekday. Only when the overall chi-squared test is highly significant
does it flag dry values on strongly under-recorded weekdays. It is
intended to detect false zeros caused by systematic missed manual
observations. `precipQC_subdaily` applies it after coverage-screened
daily aggregation and expands the resulting flags back to native
intervals.

`precipQC_climatology` works on `log1p` precipitation. For each calendar
group, it takes the more conservative of an upper empirical quantile and
a robust median-plus-MAD threshold. A largest value above the limit is
temporarily removed and the threshold recomputed until stable. This
protects the reference distribution from gross errors while retaining a
low false positive rate.

`precipQC_accumulation` first calculates the mean wet-day total of each
station. A candidate must exceed `threshold.factor` times that mean and
have the requested dry/missing flanks. It identifies candidates only; it
cannot prove that a reported depth is a longer-period accumulation.

`precipQC_spatial` applies a leave-one-station-out test after `log1p`
transformation. Coordinates, overlap, and correspondence select up to
ten neighbours. If elevation is selected, its exponential similarity
factor penalizes neighbours separated by large height differences. The
estimate is a distance- and elevation-weighted median when usable
metadata weights exist and an ordinary median otherwise. A located
target ranks eligible located neighbours first and uses
correlation-qualified unlocated neighbours only as fallbacks. A target
missing either coordinate uses correlation-based selection and remains
fully testable. The critical ratio uses the larger of the simultaneous
neighbour dispersion, historical target residual dispersion, and a small
numerical floor. Only positive residuals are flagged here; isolated
dryness is handled separately.

`precipQC_dryspell` aggregates native observations to wet/dry calendar
days, locates long dry runs, and flags target values only where the
requested fraction of neighbours records enough wet days in the same
window.

`precipQC_breakpoint` first aggregates sub-daily input to daily totals.
It applies the rank-based Pettitt statistic separately to sufficiently
complete annual precipitation totals, wet-day counts, maxima, and counts
above the station wet-day 99th percentile. P-values are Holm-adjusted
across the selected indicators. The strongest breakpoint, its adjusted
p-value, pre/post relative change, and number of significant indicators
are returned because a statistically significant change is not
necessarily a measurement error.

## Value

`precipQC_range`, `precipQC_persistence`, `precipQC_duplicate`,
`precipQC_frequency`, `precipQC_gap`, `precipQC_weekday`,
`precipQC_climatology`, `precipQC_accumulation`, and `precipQC_dryspell`
return logical `zoo` objects matching `x`.

`precipQC_spatial` returns a list with logical `flags`, numeric
`scores`, neighbour `estimate` series, and selected `neighbours`.

`precipQC_breakpoint` returns one data.frame row per station with the
breakpoint year, selected indicator, adjusted p-value, relative change,
number of significant indicators, and logical flag.

## References

Hamada, A., Arakawa, O. and Yatagai, A. (2011). An automated quality
control method for daily rain-gauge data. *Global Environmental
Research*, 15, 183–192.

Scherrer, S. C. et al. (2011). Operational quality control of daily
precipitation using spatio-climatological plausibility testing.
*Meteorologische Zeitschrift*, 20, 397–407.

El Hachem, A. et al. (2022). Space-time statistical quality control of
extreme precipitation observations. *Hydrology and Earth System
Sciences*, 26, 6137–6146.

Durre, I., Menne, M. J., Gleason, B. E., Houston, T. G. and Vose, R. S.
(2010). Comprehensive automated quality assurance of daily surface
observations. *Journal of Applied Meteorology and Climatology*, 49,
1615–1633.

Lewis, E. et al. (2021). Quality control of a global hourly rainfall
dataset. *Environmental Modelling and Software*, 144, 105169.

Stepanek, P., Zahradnicek, P. and Skalak, P. (2009). Data quality
control and homogenization of air temperature and precipitation series
in the area of the Czech Republic in the period 1961–2007. *Advances in
Science and Research*, 3, 23–26.

Vicente-Serrano, S. M., Begueria, S., Lopez-Moreno, J. I., Garcia-Vera,
M. A. and Stepanek, P. (2010). A complete daily precipitation database
for northeast Spain: reconstruction, quality control, and homogeneity.
*International Journal of Climatology*, 30, 1146–1163.

Estevez, J. et al. (2022). A quality control procedure for long-term
series of daily precipitation data in a semiarid environment.
*Theoretical and Applied Climatology*, 149, 1029–1041.

Golkhatmi, N. S. N. and Farzandi, M. (2024). Enhancing rainfall data
consistency and completeness: a spatiotemporal quality control approach
and missing data reconstruction using MICE on large precipitation
datasets. *Water Resources Management*, 38, 815–833.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md),
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md),
[`plot.precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC-class.md)

## Examples

``` r
dates <- seq(as.Date("2020-01-01"), by="day", length.out=60)
x <- zoo(cbind(A=rep(0, 60), B=rep(0, 60), C=rep(0, 60)), dates)
x[10, "A"] <- -1
x[20:24, "B"] <- 15

precipQC_range(x)
#>                A     B     C
#> 2020-01-01 FALSE FALSE FALSE
#> 2020-01-02 FALSE FALSE FALSE
#> 2020-01-03 FALSE FALSE FALSE
#> 2020-01-04 FALSE FALSE FALSE
#> 2020-01-05 FALSE FALSE FALSE
#> 2020-01-06 FALSE FALSE FALSE
#> 2020-01-07 FALSE FALSE FALSE
#> 2020-01-08 FALSE FALSE FALSE
#> 2020-01-09 FALSE FALSE FALSE
#> 2020-01-10  TRUE FALSE FALSE
#> 2020-01-11 FALSE FALSE FALSE
#> 2020-01-12 FALSE FALSE FALSE
#> 2020-01-13 FALSE FALSE FALSE
#> 2020-01-14 FALSE FALSE FALSE
#> 2020-01-15 FALSE FALSE FALSE
#> 2020-01-16 FALSE FALSE FALSE
#> 2020-01-17 FALSE FALSE FALSE
#> 2020-01-18 FALSE FALSE FALSE
#> 2020-01-19 FALSE FALSE FALSE
#> 2020-01-20 FALSE FALSE FALSE
#> 2020-01-21 FALSE FALSE FALSE
#> 2020-01-22 FALSE FALSE FALSE
#> 2020-01-23 FALSE FALSE FALSE
#> 2020-01-24 FALSE FALSE FALSE
#> 2020-01-25 FALSE FALSE FALSE
#> 2020-01-26 FALSE FALSE FALSE
#> 2020-01-27 FALSE FALSE FALSE
#> 2020-01-28 FALSE FALSE FALSE
#> 2020-01-29 FALSE FALSE FALSE
#> 2020-01-30 FALSE FALSE FALSE
#> 2020-01-31 FALSE FALSE FALSE
#> 2020-02-01 FALSE FALSE FALSE
#> 2020-02-02 FALSE FALSE FALSE
#> 2020-02-03 FALSE FALSE FALSE
#> 2020-02-04 FALSE FALSE FALSE
#> 2020-02-05 FALSE FALSE FALSE
#> 2020-02-06 FALSE FALSE FALSE
#> 2020-02-07 FALSE FALSE FALSE
#> 2020-02-08 FALSE FALSE FALSE
#> 2020-02-09 FALSE FALSE FALSE
#> 2020-02-10 FALSE FALSE FALSE
#> 2020-02-11 FALSE FALSE FALSE
#> 2020-02-12 FALSE FALSE FALSE
#> 2020-02-13 FALSE FALSE FALSE
#> 2020-02-14 FALSE FALSE FALSE
#> 2020-02-15 FALSE FALSE FALSE
#> 2020-02-16 FALSE FALSE FALSE
#> 2020-02-17 FALSE FALSE FALSE
#> 2020-02-18 FALSE FALSE FALSE
#> 2020-02-19 FALSE FALSE FALSE
#> 2020-02-20 FALSE FALSE FALSE
#> 2020-02-21 FALSE FALSE FALSE
#> 2020-02-22 FALSE FALSE FALSE
#> 2020-02-23 FALSE FALSE FALSE
#> 2020-02-24 FALSE FALSE FALSE
#> 2020-02-25 FALSE FALSE FALSE
#> 2020-02-26 FALSE FALSE FALSE
#> 2020-02-27 FALSE FALSE FALSE
#> 2020-02-28 FALSE FALSE FALSE
#> 2020-02-29 FALSE FALSE FALSE
precipQC_persistence(x)
#>                A     B     C
#> 2020-01-01 FALSE FALSE FALSE
#> 2020-01-02 FALSE FALSE FALSE
#> 2020-01-03 FALSE FALSE FALSE
#> 2020-01-04 FALSE FALSE FALSE
#> 2020-01-05 FALSE FALSE FALSE
#> 2020-01-06 FALSE FALSE FALSE
#> 2020-01-07 FALSE FALSE FALSE
#> 2020-01-08 FALSE FALSE FALSE
#> 2020-01-09 FALSE FALSE FALSE
#> 2020-01-10 FALSE FALSE FALSE
#> 2020-01-11 FALSE FALSE FALSE
#> 2020-01-12 FALSE FALSE FALSE
#> 2020-01-13 FALSE FALSE FALSE
#> 2020-01-14 FALSE FALSE FALSE
#> 2020-01-15 FALSE FALSE FALSE
#> 2020-01-16 FALSE FALSE FALSE
#> 2020-01-17 FALSE FALSE FALSE
#> 2020-01-18 FALSE FALSE FALSE
#> 2020-01-19 FALSE FALSE FALSE
#> 2020-01-20 FALSE  TRUE FALSE
#> 2020-01-21 FALSE  TRUE FALSE
#> 2020-01-22 FALSE  TRUE FALSE
#> 2020-01-23 FALSE  TRUE FALSE
#> 2020-01-24 FALSE  TRUE FALSE
#> 2020-01-25 FALSE FALSE FALSE
#> 2020-01-26 FALSE FALSE FALSE
#> 2020-01-27 FALSE FALSE FALSE
#> 2020-01-28 FALSE FALSE FALSE
#> 2020-01-29 FALSE FALSE FALSE
#> 2020-01-30 FALSE FALSE FALSE
#> 2020-01-31 FALSE FALSE FALSE
#> 2020-02-01 FALSE FALSE FALSE
#> 2020-02-02 FALSE FALSE FALSE
#> 2020-02-03 FALSE FALSE FALSE
#> 2020-02-04 FALSE FALSE FALSE
#> 2020-02-05 FALSE FALSE FALSE
#> 2020-02-06 FALSE FALSE FALSE
#> 2020-02-07 FALSE FALSE FALSE
#> 2020-02-08 FALSE FALSE FALSE
#> 2020-02-09 FALSE FALSE FALSE
#> 2020-02-10 FALSE FALSE FALSE
#> 2020-02-11 FALSE FALSE FALSE
#> 2020-02-12 FALSE FALSE FALSE
#> 2020-02-13 FALSE FALSE FALSE
#> 2020-02-14 FALSE FALSE FALSE
#> 2020-02-15 FALSE FALSE FALSE
#> 2020-02-16 FALSE FALSE FALSE
#> 2020-02-17 FALSE FALSE FALSE
#> 2020-02-18 FALSE FALSE FALSE
#> 2020-02-19 FALSE FALSE FALSE
#> 2020-02-20 FALSE FALSE FALSE
#> 2020-02-21 FALSE FALSE FALSE
#> 2020-02-22 FALSE FALSE FALSE
#> 2020-02-23 FALSE FALSE FALSE
#> 2020-02-24 FALSE FALSE FALSE
#> 2020-02-25 FALSE FALSE FALSE
#> 2020-02-26 FALSE FALSE FALSE
#> 2020-02-27 FALSE FALSE FALSE
#> 2020-02-28 FALSE FALSE FALSE
#> 2020-02-29 FALSE FALSE FALSE
precipQC_weekday(x)
#>                A     B     C
#> 2020-01-01 FALSE FALSE FALSE
#> 2020-01-02 FALSE FALSE FALSE
#> 2020-01-03 FALSE FALSE FALSE
#> 2020-01-04 FALSE FALSE FALSE
#> 2020-01-05 FALSE FALSE FALSE
#> 2020-01-06 FALSE FALSE FALSE
#> 2020-01-07 FALSE FALSE FALSE
#> 2020-01-08 FALSE FALSE FALSE
#> 2020-01-09 FALSE FALSE FALSE
#> 2020-01-10 FALSE FALSE FALSE
#> 2020-01-11 FALSE FALSE FALSE
#> 2020-01-12 FALSE FALSE FALSE
#> 2020-01-13 FALSE FALSE FALSE
#> 2020-01-14 FALSE FALSE FALSE
#> 2020-01-15 FALSE FALSE FALSE
#> 2020-01-16 FALSE FALSE FALSE
#> 2020-01-17 FALSE FALSE FALSE
#> 2020-01-18 FALSE FALSE FALSE
#> 2020-01-19 FALSE FALSE FALSE
#> 2020-01-20 FALSE FALSE FALSE
#> 2020-01-21 FALSE FALSE FALSE
#> 2020-01-22 FALSE FALSE FALSE
#> 2020-01-23 FALSE FALSE FALSE
#> 2020-01-24 FALSE FALSE FALSE
#> 2020-01-25 FALSE FALSE FALSE
#> 2020-01-26 FALSE FALSE FALSE
#> 2020-01-27 FALSE FALSE FALSE
#> 2020-01-28 FALSE FALSE FALSE
#> 2020-01-29 FALSE FALSE FALSE
#> 2020-01-30 FALSE FALSE FALSE
#> 2020-01-31 FALSE FALSE FALSE
#> 2020-02-01 FALSE FALSE FALSE
#> 2020-02-02 FALSE FALSE FALSE
#> 2020-02-03 FALSE FALSE FALSE
#> 2020-02-04 FALSE FALSE FALSE
#> 2020-02-05 FALSE FALSE FALSE
#> 2020-02-06 FALSE FALSE FALSE
#> 2020-02-07 FALSE FALSE FALSE
#> 2020-02-08 FALSE FALSE FALSE
#> 2020-02-09 FALSE FALSE FALSE
#> 2020-02-10 FALSE FALSE FALSE
#> 2020-02-11 FALSE FALSE FALSE
#> 2020-02-12 FALSE FALSE FALSE
#> 2020-02-13 FALSE FALSE FALSE
#> 2020-02-14 FALSE FALSE FALSE
#> 2020-02-15 FALSE FALSE FALSE
#> 2020-02-16 FALSE FALSE FALSE
#> 2020-02-17 FALSE FALSE FALSE
#> 2020-02-18 FALSE FALSE FALSE
#> 2020-02-19 FALSE FALSE FALSE
#> 2020-02-20 FALSE FALSE FALSE
#> 2020-02-21 FALSE FALSE FALSE
#> 2020-02-22 FALSE FALSE FALSE
#> 2020-02-23 FALSE FALSE FALSE
#> 2020-02-24 FALSE FALSE FALSE
#> 2020-02-25 FALSE FALSE FALSE
#> 2020-02-26 FALSE FALSE FALSE
#> 2020-02-27 FALSE FALSE FALSE
#> 2020-02-28 FALSE FALSE FALSE
#> 2020-02-29 FALSE FALSE FALSE
precipQC_breakpoint(x)
#>   station breakpoint.year indicator p.value relative.change n.indicators
#> 1       A              NA      <NA>      NA              NA            0
#> 2       B              NA      <NA>      NA              NA            0
#> 3       C              NA      <NA>      NA              NA            0
#>   flagged
#> 1   FALSE
#> 2   FALSE
#> 3   FALSE
```
