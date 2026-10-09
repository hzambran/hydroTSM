# Individual quality-control tests for streamflow

Individual tests called by
[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md)
and
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md).
They are exported so each source of evidence can be run, inspected,
deactivated, and tuned independently.

## Usage

``` r
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
```

## Arguments

- x:

  A numeric `zoo` object with one station per column.

- lower, upper:

  Physical limits, scalar or station-specific.

- min.month.values, min.year.values:

  Minimum paired calendar positions for exact copied-month and
  copied-year comparisons.

- min.nonzero:

  Minimum positive observations in a candidate copied block.

- interval:

  Expected interval: days for a `Date` index or seconds for a `POSIXt`
  index. `NULL` uses the modal difference.

- group:

  Seasonal grouping. `"month_slot"` preserves the calendar month and
  hour:minute slot.

- window:

  For climatology, circular calendar-day window width; for fluctuation,
  number of observations; for spatial QC, observations before and after
  the target event.

- z:

  Standardized climatology or first-difference threshold.

- min.samples:

  Minimum climatology or first-difference reference sample.

- offset:

  Positive constant added before taking logarithms.

- run, high.run:

  Minimum ordinary and high-flow flat-line lengths in observations.

- high.prob:

  Target high-flow quantile.

- tolerance:

  Maximum consecutive difference inside a flat line.

- flag.zero:

  Whether repeated zero-flow runs may be flagged.

- factor:

  Multiplicative isolated peak/dip threshold.

- prob:

  Quantile used as the absolute spike threshold.

- max.rise, max.fall:

  Optional absolute consecutive-change limits, scalar or
  station-specific.

- drop.factor:

  Multiplicative abrupt-fall threshold.

- minimum:

  Number of sign reversals that a fluctuation window must exceed.

- low.diff.prob:

  Quantile of absolute changes treated as noise.

- log.z:

  Global log-flow standard-deviation threshold.

- amax.ratio:

  Largest/second-largest annual-maximum ratio threshold.

- return.period:

  GEV return-period threshold in years.

- min.years:

  Minimum annual values required for GEV or breakpoint analysis.

- metadata:

  `NULL`, or station metadata containing all stations.

- station.id:

  Name of the unique station identifier field.

- coords:

  Names of longitude and latitude fields, in that order.

- area:

  `NULL`, or the name of a positive numeric catchment-area field.

- elevation:

  `NULL`, or the name of an optional numeric elevation field.

- elevation.scale:

  Positive elevation-decay scale used in neighbour ranking.

- n.neighbours:

  Maximum selected neighbours per station.

- max.distance:

  Maximum geographic distance in km.

- min.neighbours:

  Minimum simultaneous spatial predictions.

- min.support:

  Minimum neighbours with a supporting high-flow event.

- min.overlap:

  Minimum paired observations for selection and regression.

- min.correlation:

  Minimum preferred Spearman log-flow correlation.

- support.prob:

  Neighbour high-flow quantile.

- f:

  Minimum standardized spatial residual.

- alpha:

  Holm-adjusted Pettitt-test significance level.

- min.relative.change:

  Minimum relative pre/post median change.

- min.completeness:

  Minimum daily completeness of an annual indicator.

- indicators:

  Unique subset of `"mean"`, `"sd"`, `"minimum"`, and `"maximum"`.

## Details

`streamflowQC_range` treats negative discharge and optional
station-specific upper violations as physical errors. Missing values are
handled by the workflow completeness calculation. Non-finite non-missing
values fail the range check.

`streamflowQC_duplicate` finds exactly copied calendar months or years.
It is intended for daily data; requiring a positive value prevents
ordinary all-zero intermittent-flow blocks from being flagged by
default.

`streamflowQC_gap` marks the first finite observation after a missing
value or missing interval. It complements, rather than replaces, station
completeness. A numeric `zoo` cannot retain transmission syntax, so the
QARTOD syntax test is enforced structurally by the workflow validators.

`streamflowQC_climatology` applies the GSIM transformation
`log(Q + 0.01)` and mean plus/minus six standard deviations. Daily
references use a circular calendar-day window; sub-daily references may
use month or month-slot groups.

`streamflowQC_flatline` flags every member of an extended near-constant
run. Daily defaults implement more than 10 equal positive values from
GSIM. The sub-daily workflow converts the UK-Flow15-QC seven-day and
one-day high-flow rules from hours to input observations.

`streamflowQC_spike` requires an isolated centre value to change and
reverse by a factor of five or by more than the station 0.99 quantile.
`streamflowQC_rate` combines robust monthly log-difference screening,
optional absolute limits, and the five-fold rapid-drop rule.
`streamflowQC_fluctuation` flags windows with recurrent first-derivative
sign reversals after suppressing the smallest changes.

`streamflowQC_highflow` returns one combined flag plus separate
six-standard-deviation, AMAX-ratio, and GEV components. The GEV is
fitted to annual maxima by base-R maximum likelihood. The combined flag
deliberately counts as one evidence family because all three statistics
describe the same extreme event.

`streamflowQC_spatial` selects neighbours using correlation and optional
geographic/elevation proximity, fits target-on-neighbour regressions in
log-specific-discharge space when catchment area is available, and
combines predictions using inverse-error weighted medians. A rare target
event is flagged only when its residual score exceeds `f` and fewer than
`min.support` neighbours exceed their support quantile in the search
window.

`streamflowQC_breakpoint` aggregates sub-daily input to daily means,
calculates complete annual indicators, applies Pettitt rank tests, and
Holm-adjusts p-values. Its station flag is separate from point QC
because an inhomogeneity may represent real regulation or catchment
change.

## Value

Range, duplicate, gap, climatology, flatline, spike, rate, and
fluctuation tests return a logical `zoo` matching `x`.

`streamflowQC_highflow` returns a list with combined `flags`, component
flag objects, `return.period`, and station fit diagnostics.

`streamflowQC_spatial` returns a list with `flags`, standardized
`scores`, flow `estimate`, neighbour-event `support.count`, and selected
`neighbours`.

`streamflowQC_breakpoint` returns one data.frame row per station.

## References

Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. *EGUsphere preprint*.
doi:10.5194/egusphere-2026-277.

Gudmundsson, L. et al. (2018). The Global Streamflow Indices and
Metadata Archive (GSIM) - Part 2: quality control, time-series indices
and homogeneity assessment. *Earth System Science Data*, 10, 787–804.
doi:10.5194/essd-10-787-2018.

U.S. Integrated Ocean Observing System (2018). *Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0*.
doi:10.25923/gszc-ha43.

Zhao, Q. et al. (2018). Research on the Data-Driven Quality Control
Method of Hydrological Time Series Data. *Water*, 10, 1712.
doi:10.3390/w10121712.

## See also

[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md),
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:19
x <- zoo::zoo(cbind(A=c(rep(10, 11), 11:19),
                    B=seq(5, 24)), dates)
streamflowQC_flatline(x)
#>                A     B
#> 2020-01-01  TRUE FALSE
#> 2020-01-02  TRUE FALSE
#> 2020-01-03  TRUE FALSE
#> 2020-01-04  TRUE FALSE
#> 2020-01-05  TRUE FALSE
#> 2020-01-06  TRUE FALSE
#> 2020-01-07  TRUE FALSE
#> 2020-01-08  TRUE FALSE
#> 2020-01-09  TRUE FALSE
#> 2020-01-10  TRUE FALSE
#> 2020-01-11  TRUE FALSE
#> 2020-01-12 FALSE FALSE
#> 2020-01-13 FALSE FALSE
#> 2020-01-14 FALSE FALSE
#> 2020-01-15 FALSE FALSE
#> 2020-01-16 FALSE FALSE
#> 2020-01-17 FALSE FALSE
#> 2020-01-18 FALSE FALSE
#> 2020-01-19 FALSE FALSE
#> 2020-01-20 FALSE FALSE
streamflowQC_range(x)
#>                A     B
#> 2020-01-01 FALSE FALSE
#> 2020-01-02 FALSE FALSE
#> 2020-01-03 FALSE FALSE
#> 2020-01-04 FALSE FALSE
#> 2020-01-05 FALSE FALSE
#> 2020-01-06 FALSE FALSE
#> 2020-01-07 FALSE FALSE
#> 2020-01-08 FALSE FALSE
#> 2020-01-09 FALSE FALSE
#> 2020-01-10 FALSE FALSE
#> 2020-01-11 FALSE FALSE
#> 2020-01-12 FALSE FALSE
#> 2020-01-13 FALSE FALSE
#> 2020-01-14 FALSE FALSE
#> 2020-01-15 FALSE FALSE
#> 2020-01-16 FALSE FALSE
#> 2020-01-17 FALSE FALSE
#> 2020-01-18 FALSE FALSE
#> 2020-01-19 FALSE FALSE
#> 2020-01-20 FALSE FALSE
```
