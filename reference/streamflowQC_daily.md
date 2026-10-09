# Quality control of daily streamflow time series

Applies individually selectable physical, temporal, high-flow, spatial,
and homogeneity tests to one or more daily streamflow series. It returns
point flags and station acceptance recommendations without writing
files.

## Usage

``` r
streamflowQC_daily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, gap=TRUE, climatology=TRUE,
           flatline=TRUE, spike=TRUE, rate=TRUE, highflow=TRUE,
           spatial=TRUE, breakpoint=TRUE),
  lower=0, upper=Inf,
  duplicate.min.month=20L, duplicate.min.year=300L,
  duplicate.min.nonzero=1L,
  climatology.window=5L, climatology.z=6,
  climatology.min.samples=30L, log.offset=0.01,
  flatline.run=11L, flatline.tolerance=0, flag.zero=FALSE,
  spike.factor=5, spike.prob=0.99,
  rate.z=6, rate.min.samples=30L,
  rate.max.rise=Inf, rate.max.fall=Inf, rate.drop.factor=5,
  highflow.log.z=6, highflow.amax.ratio=2,
  highflow.return.period=1000, highflow.min.years=10L,
  spatial.window.days=3L, spatial.high.prob=0.99,
  spatial.support.prob=0.95, spatial.f=3,
  n.neighbours=5L, max.distance=400, min.neighbours=2L,
  min.support=1L, min.overlap=90L, min.correlation=0.5,
  breakpoint.min.years=10L, breakpoint.alpha=0.01,
  breakpoint.min.relative.change=0.5,
  breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=3L, max.missing=0.2, max.review=0.2,
  max.suspicious=0.05, min.years=1, discard.breakpoint=FALSE,
  area=NULL, elevation=NULL, elevation.scale=500
)
```

## Arguments

- x:

  A numeric daily `zoo` object. Each column is one station and the index
  is `Date` or `POSIXt`.

- metadata:

  `NULL`, or a data.frame containing every station in `x`. Extra fields
  are preserved in the output.

- station.id:

  Name of the unique station-identifier field in `metadata`. Identifiers
  are matched to `colnames(x)`.

- coords:

  Names of numeric longitude and latitude fields, in that order. Missing
  coordinates are allowed; non-missing coordinates are range-checked.

- checks:

  Named logical vector. Omitted names retain their defaults; all daily
  tests are enabled by default.

- lower, upper:

  Physical limits, scalar or station-specific. The default rejects
  negative flow but deliberately imposes no universal upper limit.

- duplicate.min.month, duplicate.min.year:

  Minimum paired calendar positions for exact copied-month and
  copied-year checks.

- duplicate.min.nonzero:

  Minimum positive values required in each candidate copied block.

- climatology.window:

  Width in calendar days of the circular seasonal reference window.

- climatology.z:

  Standard-deviation multiplier for the log-flow climatology test.

- climatology.min.samples:

  Minimum seasonal reference observations.

- log.offset:

  Positive constant added before taking logarithms.

- flatline.run:

  Minimum number of consecutive equal or near-equal daily values. The
  default flags runs of more than 10 days.

- flatline.tolerance:

  Maximum consecutive numerical difference within a flat line.

- flag.zero:

  Whether repeated zero-flow runs may be flagged. The default protects
  legitimate no-flow periods.

- spike.factor:

  Multiplicative isolated peak/dip threshold.

- spike.prob:

  Station quantile used as the absolute isolated-spike threshold.

- rate.z:

  Robust standardized first-difference threshold.

- rate.min.samples:

  Minimum monthly first-difference reference sample.

- rate.max.rise, rate.max.fall:

  Optional absolute consecutive rise and fall limits, scalar or
  station-specific.

- rate.drop.factor:

  Multiplicative abrupt-fall threshold.

- highflow.log.z:

  Standard-deviation multiplier for unusually high global log-flow.

- highflow.amax.ratio:

  Ratio of the largest annual maximum to the second largest that
  generates a high-flow flag.

- highflow.return.period:

  GEV return-period threshold in years.

- highflow.min.years:

  Minimum annual maxima required for GEV fitting.

- spatial.window.days:

  Number of days before and after a high-flow event searched for
  neighbouring support.

- spatial.high.prob, spatial.support.prob:

  Target and neighbour high-flow quantiles.

- spatial.f:

  Minimum standardized spatial-regression residual.

- n.neighbours:

  Maximum selected neighbours per station.

- max.distance:

  Maximum target-neighbour distance in km.

- min.neighbours:

  Minimum simultaneous spatial predictions.

- min.support:

  Minimum neighbouring high-flow events needed to corroborate a target
  event.

- min.overlap:

  Minimum paired observations for neighbour selection and pairwise
  regression.

- min.correlation:

  Minimum Spearman correlation on log-flow for a preferred neighbour.

- breakpoint.min.years:

  Minimum complete annual indicators for a Pettitt test.

- breakpoint.alpha:

  Holm-adjusted breakpoint significance level.

- breakpoint.min.relative.change:

  Minimum relative pre/post median change.

- breakpoint.min.completeness:

  Minimum daily completeness for an annual indicator.

- correction:

  `"none"` preserves all values, `"set_na"` removes confirmed points,
  and `"spatial"` uses an available spatial estimate and otherwise `NA`.

- min.evidence:

  Number of independent flag families needed to confirm a point that did
  not fail a hard test.

- max.missing, max.review, max.suspicious:

  Maximum station fractions of missing expected values, any flagged
  observations, and confirmed suspicious observations.

- min.years:

  Minimum record duration for station acceptance.

- discard.breakpoint:

  Whether a significant large breakpoint discards a station. It is
  diagnostic by default because real catchment change or regulation can
  cause inhomogeneity.

- area:

  `NULL`, or the name of a positive numeric catchment-area field used to
  compare specific discharge spatially.

- elevation:

  `NULL`, or the name of an optional numeric elevation field.

- elevation.scale:

  Positive elevation-decay scale used in neighbour ranking.

## Details

The daily climatology test reproduces the GSIM structure: it compares
`log(Q + log.offset)` with a five-day calendar window using a
six-standard deviation default. Negative/non-finite non-missing values
and exact copied calendar blocks are hard failures. Other tests remain
evidence for review; the default requires three independent families
before point rejection because floods, regulation, and intermittent flow
can legitimately trigger individual statistical rules.

High-flow diagnostics combine the six-standard-deviation, largest-AMAX
ratio, and GEV return-period tests into one evidence family so
correlated extreme statistics are not counted as independent proof.
Spatial QC fits pairwise log-flow regressions, optionally on flow
divided by catchment area, and flags a rare event only when its
regression residual is large and too few selected neighbours show high
flow within the search window.

Station acceptance requires all user-defined completeness, review,
confirmed suspicion, duration, and optional breakpoint criteria. Point
and station decisions are therefore related but distinct.

## Value

An object of class `"streamflowQC"`; see
[`streamflowQC-class`](https://hzambran.github.io/hydroTSM/reference/streamflowQC-class.md).

## References

Crochemore, L. et al. (2020). Lessons learnt from checking the quality
of openly accessible river flow data worldwide. *Hydrological Sciences
Journal*, 65, 699–711. doi:10.1080/02626667.2019.1659509.

Gudmundsson, L. et al. (2018). The Global Streamflow Indices and
Metadata Archive (GSIM) - Part 2: quality control, time-series indices
and homogeneity assessment. *Earth System Science Data*, 10, 787–804.
doi:10.5194/essd-10-787-2018.

U.S. Integrated Ocean Observing System (2018). *Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0*.
doi:10.25923/gszc-ha43.

## See also

[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md),
[`streamflowQC`](https://hzambran.github.io/hydroTSM/reference/streamflowQC.md),
[`streamflowQC_tests`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:59
x <- zoo::zoo(cbind(A=10 + sin(seq_along(dates) / 5),
                    B=12 + sin(seq_along(dates) / 5)), dates)
x[20, "A"] <- -1
qc <- streamflowQC_daily(
  x, checks=c(duplicate=FALSE, climatology=FALSE, flatline=FALSE,
              spike=FALSE, rate=FALSE, highflow=FALSE,
              spatial=FALSE, breakpoint=FALSE),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1
)
qc$suspicious
#>         time station original spatial.estimate spatial.score n.tests tests
#> 1 2020-01-20       A       -1               NA            NA       1 range
#>   action corrected correction
#> 1 reject        -1       none
```
