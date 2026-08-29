# Quality control of daily air-temperature time series

Applies a conservative, multi-test quality-control workflow to one or
more daily air-temperature station series in a `zoo` object. It returns
auditable point flags, optional corrections, and station acceptance or
discard recommendations without writing to disk.

## Usage

``` r
tempQC_daily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, duplicate=TRUE, climatology=TRUE,
           persistence=TRUE, step=TRUE, spike=TRUE,
           spatial=TRUE, breakpoint=TRUE),
  lower=-89.4, upper=57.7,
  duplicate.min.month=20L, duplicate.min.year=300L,
  climatology.window=15L, climatology.z=6,
  climatology.min.samples=100L,
  persistence.run=7L, persistence.tolerance=0,
  step.z=3, step.min.samples=20L,
  step.max.rise=Inf, step.max.fall=Inf,
  spike.threshold=25,
  n.neighbours=5L, max.distance=200, min.neighbours=2L,
  min.overlap=90L, min.group.overlap=20L,
  min.correlation=0.6, spatial.f=4, spatial.min.difference=8,
  breakpoint.min.years=10L, breakpoint.alpha=0.05,
  breakpoint.min.shift=1, breakpoint.min.completeness=0.8,
  correction=c("none", "set_na", "spatial"),
  min.evidence=2L, max.missing=0.2, max.suspicious=0.05,
  min.years=1, discard.breakpoint=FALSE,
  elevation=NULL, elevation.scale=500
)
```

## Arguments

- x:

  A numeric daily `zoo` object with one uniquely named station column
  per series. Values are assumed to be degrees Celsius.

- metadata:

  `NULL`, or a data.frame with one row per station. Additional columns
  are retained in both metadata outputs. When supplied, the selected
  identifier, longitude, and latitude fields are required. Rows are
  matched to `colnames(x)` and need not be pre-sorted.

- station.id:

  Name of the metadata column containing the column names of `x`.

- coords:

  Names of numeric longitude and latitude metadata columns in decimal
  degrees, in that order.

- elevation:

  `NULL`, or the name of an optional numeric metadata column containing
  elevation in metres.

- elevation.scale:

  Positive elevation-decay scale in metres. It controls how strongly
  height differences reduce neighbour suitability and regression
  weights. It is ignored when `elevation=NULL`.

- checks:

  Named logical vector used to activate or deactivate any of `"range"`,
  `"duplicate"`, `"climatology"`, `"persistence"`, `"step"`, `"spike"`,
  `"spatial"`, and `"breakpoint"`. All are active by default.

- lower, upper:

  Physical screening limits. Each can be scalar or contain one value per
  station. Regionally authoritative limits should replace the defaults
  when available.

- duplicate.min.month, duplicate.min.year:

  Minimum number of complete, paired calendar positions needed to
  declare two months or years identical.

- climatology.window:

  Width in days of the circular day-of-year reference window.

- climatology.z:

  Absolute robust standardized anomaly threshold.

- climatology.min.samples:

  Minimum reference sample size for a calendar-window climatology.

- persistence.run:

  Minimum number of consecutive values in a flat line.

- persistence.tolerance:

  Maximum range within a flat-line window.

- step.z:

  Robust standardized threshold for a consecutive-day change.

- step.min.samples:

  Minimum monthly sample of first differences.

- step.max.rise, step.max.fall:

  Optional absolute rise and fall limits; `Inf` leaves the
  month-specific statistical limits in control.

- spike.threshold:

  Minimum absolute difference from both adjacent days, with the two
  differences having the same sign, for an isolated spike or dip.

- n.neighbours:

  Maximum number of spatial-regression neighbours.

- max.distance:

  Maximum target-neighbour great-circle distance in km.

- min.neighbours:

  Minimum number of simultaneous regression estimates.

- min.overlap:

  Minimum paired observations for initial neighbour selection.

- min.group.overlap:

  Minimum paired observations within a calendar month for fitting an
  individual regression.

- min.correlation:

  Minimum Pearson correlation for an eligible neighbour.

- spatial.f:

  Minimum standardized spatial-regression residual.

- spatial.min.difference:

  Minimum absolute observed-estimated difference in degrees Celsius.
  Both spatial criteria must be exceeded.

- breakpoint.min.years:

  Minimum complete annual indicators for a Pettitt test.

- breakpoint.alpha:

  Holm-adjusted significance level for breakpoint evidence.

- breakpoint.min.shift:

  Minimum absolute difference between median annual indicators before
  and after a breakpoint.

- breakpoint.min.completeness:

  Minimum fraction of daily observations in an annual indicator.

- correction:

  `"none"` preserves values, `"set_na"` replaces confirmed suspicious
  values by `NA`, and `"spatial"` uses the spatial estimate where
  available and `NA` otherwise.

- min.evidence:

  Number of coincident non-hard flags needed to reject a point. Physical
  range, copied-block, flat-line, and isolated-spike flags are hard
  evidence.

- max.missing, max.suspicious:

  Maximum missing and confirmed-suspicious fractions for recommending a
  station for acceptance.

- min.years:

  Minimum record length for station acceptance.

- discard.breakpoint:

  Logical. If `TRUE`, a flagged breakpoint causes a discard
  recommendation. The default is conservative because a change may be
  climatic.

## Details

The tests represent independent error mechanisms and are deliberately
combined as evidence instead of interpreting every statistical outlier
as an error.

1.  *Physical range and copied blocks*. Values beyond observed world
    limits and exactly duplicated months or years are gross-integrity
    failures. The limits and block comparisons follow Durre et al.
    (2010). The duplicate test requires complete paired calendar
    positions to limit false positives.

2.  *Climatological plausibility*. For each station and calendar day,
    the median and MAD are calculated from a moving 15-day window. An
    absolute robust score of six is flagged. This is a resistant
    implementation of the 15-day, six-standard-deviation test in Durre
    et al. (2010) and the biweight calendar-window approach of Feng et
    al. (2004).

3.  *Persistence, steps, and spikes*. Flat lines expose stuck sensors or
    copied reports (Feng et al., 2004; Boulanger et al., 2010). First
    differences are checked against month-specific robust limits,
    adapting the seasonal rate-of-change test of Hubbard et al. (2007).
    A value at least 25 degrees from both neighbours in the same
    direction implements the daily spike/dip criterion of Durre et al.
    (2010).

4.  *Spatial regression*. Month-specific linear regressions predict the
    target from eligible correlated stations. The five regressions with
    the lowest proximity-adjusted errors are combined with
    proximity/inverse-error-variance weights. Proximity decays with
    horizontal distance and, when selected, absolute elevation
    difference. The pairwise regression intercept estimates systematic
    station offsets rather than imposing a universal lapse rate. A point
    is flagged only if both its absolute residual and its standardized
    residual are large. This combines the SRT formulation of Hubbard et
    al. (2007) and Estevez et al. (2018) with the conservative dual
    threshold of Durre et al. (2010). Correlation-based selection is
    important in complex terrain (Xiong et al., 2022; Xiong et al.,
    2024).

5.  *Homogeneity*. Pettitt tests are applied to sufficiently complete
    annual mean and variability series and adjusted by Holm's method.
    The result is diagnostic by default, reflecting the distinction
    between QC and homogenization emphasized by Boulanger et al. (2010)
    and Brunet et al. (2020).

The paired logical constraints \\T\_{min} \le T\_{max}\\ and lagged
minimum- maximum checks cannot be inferred from a single univariate
station matrix. Use
[`tempQC_internal`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md)
when paired Tmin and Tmax `zoo` objects are available.

## Value

An object of class `"tempQC"`; see
[`tempQC-class`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md).

## References

Boulanger, J.-P., Aizpuru, J., Leggieri, L. and Marino, M. (2010). A
procedure for automated quality control and homogenization of historical
daily temperature and precipitation data (APACH): part 1. *Climatic
Change*, 98, 471–491.

Brunet, M. et al. (2020). *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

Durre, I., Menne, M. J., Gleason, B. E., Houston, T. G. and Vose, R. S.
(2010). Comprehensive automated quality assurance of daily surface
observations. *Journal of Applied Meteorology and Climatology*, 49,
1615–1633.

Estevez, J., Gavilan, P. and Garcia-Marin, A. P. (2018). Spatial
regression test for ensuring temperature data quality in southern Spain.
*Theoretical and Applied Climatology*, 131, 309–318.

Feng, S., Hu, Q. and Qian, W. (2004). Quality control of daily
meteorological data in China, 1951–2000. *International Journal of
Climatology*, 24, 853–870.

Hubbard, K. G., Guttman, N. B., You, J. and Chen, Z. (2007). An improved
QC process for temperature in the daily cooperative weather
observations. *Journal of Atmospheric and Oceanic Technology*, 24,
206–213.

Xiong, X. et al. (2022). Research on quality control methods for surface
temperature observations via spatial correlation analysis.
*International Journal of Climatology*. doi:10.1002/joc.7897.

Xiong, X. et al. (2024). Research on quality control method of surface
temperature observations for complex physical geography. *Journal of
Atmospheric and Oceanic Technology*, 41, 803–815.

## See also

[`tempQC`](https://hzambran.github.io/hydroTSM/reference/tempQC.md),
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md),
[`tempQC_tests`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md),
[`plot.tempQC`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:89
v <- 12 + 8 * sin(2 * pi * seq_along(dates) / 365)
x <- zoo::zoo(cbind(A=v, B=v + 0.3), dates)
qc <- tempQC_daily(
  x, checks=c(range=TRUE, duplicate=FALSE, climatology=FALSE,
              persistence=TRUE, step=FALSE, spike=TRUE,
              spatial=FALSE, breakpoint=FALSE),
  min.years=0, max.missing=1
)
qc$station.summary
#>   station expected observed missing.percent review.count review.percent
#> A       A       90       90               0            0              0
#> B       B       90       90               0            0              0
#>   suspicious.count suspicious.percent breakpoint.year breakpoint.indicator
#> A                0                  0              NA                 <NA>
#> B                0                  0              NA                 <NA>
#>   breakpoint.p.value breakpoint.relative.change breakpoint.n.indicators
#> A                 NA                         NA                       0
#> B                 NA                         NA                       0
#>   breakpoint.flag record.years recommendation                   reason
#> A           FALSE    0.2464066         accept within acceptance limits
#> B           FALSE    0.2464066         accept within acceptance limits
```
