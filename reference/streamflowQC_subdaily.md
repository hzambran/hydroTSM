# Quality control of sub-daily streamflow time series

Applies individually selectable physical, temporal, high-flow, spatial,
and homogeneity tests to one or more regular minute/hourly streamflow
series. It returns point flags and station acceptance recommendations
without writing files.

## Usage

``` r
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
```

## Arguments

- x:

  A numeric regular sub-daily `zoo` object with a `POSIXt` index and one
  station per column.

- metadata:

  `NULL`, or a data.frame containing every station in `x`. Extra fields
  are retained.

- station.id:

  Name of the unique metadata station-identifier field.

- coords:

  Names of longitude and latitude fields, in that order. Individual
  coordinates may be missing.

- checks:

  Named logical vector. Omitted names retain their defaults; all
  sub-daily tests are enabled by default.

- lower, upper:

  Physical limits, scalar or station-specific. The default rejects
  negative flow without imposing a region-specific maximum.

- climatology.z:

  Standard-deviation multiplier for monthly log-flow.

- climatology.min.samples:

  Minimum monthly reference observations.

- log.offset:

  Positive constant added before taking logarithms.

- flatline.hours:

  Duration in hours of a low/ordinary-flow flat line.

- high.flatline.hours:

  Shorter flat-line duration applied above the station high-flow
  quantile.

- flatline.high.prob:

  High-flow quantile for the shorter flat-line rule.

- flatline.tolerance:

  Maximum consecutive numerical difference within a flat line.

- flag.zero:

  Whether repeated zero-flow runs may be flagged.

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

- fluctuation.window:

  Number of observations in the moving sign-reversal window.

- fluctuation.minimum:

  Number of derivative sign reversals that the window must exceed.

- fluctuation.low.diff.prob:

  Quantile of absolute changes treated as measurement noise before
  counting reversals.

- highflow.log.z:

  Standard-deviation multiplier for unusually high global log-flow.

- highflow.amax.ratio:

  Ratio of the largest annual maximum to the second largest that
  generates a flag.

- highflow.return.period:

  GEV return-period threshold in years.

- highflow.min.years:

  Minimum annual maxima required for GEV fitting.

- spatial.window.hours:

  Hours before and after a rare event searched for neighbouring support.

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

  Minimum neighbouring high-flow events needed for corroboration.

- min.overlap:

  Minimum paired observations for neighbour selection and regression.

- min.correlation:

  Minimum preferred Spearman log-flow correlation.

- breakpoint.min.years:

  Minimum complete annual indicators for a Pettitt test.

- breakpoint.alpha:

  Holm-adjusted breakpoint significance level.

- breakpoint.min.relative.change:

  Minimum relative pre/post median change.

- breakpoint.min.completeness:

  Minimum daily completeness for an annual indicator.

- correction:

  `"none"` preserves values, `"set_na"` removes confirmed points, and
  `"spatial"` uses an available spatial estimate and otherwise `NA`.

- min.evidence:

  Independent flag families needed to confirm a non-hard-failure point.

- max.missing, max.review, max.suspicious:

  Maximum station fractions of missing expected values, any flagged
  observations, and confirmed suspicious observations.

- min.years:

  Minimum record duration for station acceptance.

- discard.breakpoint:

  Whether a significant large breakpoint discards a station.

- area:

  `NULL`, or the name of a positive numeric catchment-area field used to
  compare specific discharge spatially.

- elevation:

  `NULL`, or an optional numeric elevation field.

- elevation.scale:

  Positive elevation-decay scale used in neighbour ranking.

## Details

The defaults generalize the 15-minute UK-Flow15-QC rules to the actual
modal interval. Thus, ordinary flat lines use 168 hours and high-flow
flat lines use 24 hours rather than fixed counts of 672 and 96. Relative
spikes and abrupt falls use a factor of five; absolute spikes use the
station 0.99 quantile; and rapid fluctuations use more than eight sign
reversals over 16 observations.

High-flow diagnostics include unusually large log-flow, an
annual-maximum ratio, and a base-R maximum-likelihood GEV fit after at
least 10 years. These correlated diagnostics contribute one evidence
family. A spatial flag requires a rare target event, a large
leave-one-station-out regression residual, and insufficient high-flow
support among selected neighbours within the default 72-hour window.
Catchment area, coordinates, and elevation are optional;
correlation-based spatial comparison remains available without metadata.

Only physical range failure is hard evidence by default. The
three-family default reduces the risk of rejecting genuine flash floods,
reservoir operations, tidal behaviour, intermittent zeros, or
high-baseflow plateaus. The review fraction can still recommend
discarding a station with pervasive single-test artefacts.

## Value

An object of class `"streamflowQC"`; see
[`streamflowQC-class`](https://hzambran.github.io/hydroTSM/reference/streamflowQC-class.md).

## References

Fileni, F., Fowler, H. J., Lewis, E., McLay, F. and Yang, L. (2023). A
quality-control framework for sub-daily flow and level data for
hydrological modelling in Great Britain. *Hydrology Research*, 54,
1357–1367. doi:10.2166/nh.2023.045.

Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. *EGUsphere preprint*.
doi:10.5194/egusphere-2026-277.

U.S. Integrated Ocean Observing System (2018). *Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0*.
doi:10.25923/gszc-ha43.

Zhao, Q. et al. (2018). Research on the Data-Driven Quality Control
Method of Hydrological Time Series Data. *Water*, 10, 1712.
doi:10.3390/w10121712.

## See also

[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md),
[`streamflowQC`](https://hzambran.github.io/hydroTSM/reference/streamflowQC.md),
[`streamflowQC_tests`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md)

## Examples

``` r
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
#>                  time station original spatial.estimate spatial.score n.tests
#> 1 2020-01-01 19:00:00       A       -1               NA            NA       1
#>   tests action corrected correction
#> 1 range reject        -1       none
```
