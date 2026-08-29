# Individual quality-control tests for air temperature

Individual tests called by
[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md)
and
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md).
They are exported so each source of evidence can be run, inspected, and
tuned independently.

## Usage

``` r
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
```

## Arguments

- x:

  A numeric `zoo` object with one station per column.

- lower, upper:

  Physical limits, scalar or station-specific.

- min.month.values, min.year.values:

  Minimum complete paired positions for copied-month and copied-year
  comparisons.

- run:

  Minimum flat-line window length in observations.

- tolerance:

  Maximum within-window temperature range.

- group:

  Calendar grouping. `"month_hour"` uses month and integer hour of day.

- window:

  Circular calendar-day window width.

- z:

  Absolute robust standardized climatology or first-difference
  threshold.

- min.samples:

  Minimum climatological or first-difference reference sample.

- max.rise, max.fall:

  Optional absolute consecutive-change limits.

- threshold:

  Absolute difference from both adjacent values required by the isolated
  spike/dip test.

- metadata:

  `NULL`, or station metadata. When supplied, the selected identifier,
  longitude, and latitude fields are required.

- station.id:

  Name of the metadata station identifier column.

- coords:

  Names of numeric longitude and latitude metadata columns in decimal
  degrees, in that order.

- elevation:

  `NULL`, or the name of an optional numeric metadata column containing
  elevation in metres.

- elevation.scale:

  Positive elevation-decay scale in metres used to penalize neighbours
  with large height differences.

- n.neighbours:

  Maximum selected spatial-regression neighbours.

- max.distance:

  Maximum target-neighbour distance in km.

- min.neighbours:

  Minimum simultaneous valid regression estimates.

- min.overlap:

  Minimum overall paired observations for neighbour selection.

- min.group.overlap:

  Minimum paired observations in a model group.

- min.correlation:

  Minimum target-neighbour Pearson correlation.

- f:

  Standardized residual threshold for spatial regression.

- min.difference:

  Absolute spatial residual threshold.

- min.se:

  Positive lower bound on each regression standard error.

- tmin, tmax:

  Numeric `zoo` objects with identical time indices and station columns
  containing paired minimum and maximum temperatures.

- lagged:

  Whether to apply previous/current/following-day logical tests.

- max.range:

  Maximum allowed lagged Tmin–Tmax separation.

- min.years:

  Minimum complete annual indicators for breakpoint testing.

- alpha:

  Holm-adjusted breakpoint significance level.

- min.shift:

  Minimum absolute pre/post median change in degrees Celsius.

- min.completeness:

  Minimum daily completeness of an annual indicator.

- indicators:

  Unique subset of `"mean"`, `"sd"`, `"minimum"`, and `"maximum"`.

## Details

`tempQC_range` is a gross-error test using the world-record screening
limits compiled by Durre et al. (2010). Limits are intentionally
configurable.

`tempQC_duplicate` compares corresponding calendar positions and flags
complete months or years that are copied exactly. Durre et al. (2010)
identify such duplication as a keying, transmission, or processing
failure.

`tempQC_persistence` moves a window through each station and flags every
observation in windows whose range does not exceed `tolerance`. Exact
seven-day flat lines follow Feng et al. (2004); the adjustable tolerance
and duration support sub-daily sensor tests such as Beele et al. (2022).

`tempQC_climatology` calculates a resistant median and MAD reference.
Daily tests use a circular day-of-year window; sub-daily tests can
preserve the seasonal and diurnal cycles with month-hour groups. The
six-score default is based on Durre et al. (2010), with the resistant
formulation motivated by Feng et al. (2004).

`tempQC_step` robustly standardizes consecutive changes within calendar
groups and can also apply absolute rise/fall limits. This is the
seasonal rate-of-change structure of Hubbard et al. (2007), extended to
hour-specific groups following Cheng et al. (2016).

`tempQC_spike` requires the centre value to differ from both neighbours
by at least `threshold` in the same direction. Its daily default is the
25-degree spike/dip rule of Durre et al. (2010).

`tempQC_spatial` fits target-on-neighbour linear regressions separately
by calendar group, retains the lowest proximity-adjusted-error eligible
neighbours, and combines their predictions with geographic/elevation
proximity and inverse-error-variance weights. The pairwise regression
intercept estimates systematic station offsets, including persistent
height effects, without imposing a fixed lapse rate. A flag requires
both an absolute and a standardized residual. The equations follow the
spatial regression test of Hubbard et al. (2007) and Estevez et al.
(2018); grouping and correlation screening incorporate findings of Cheng
et al. (2016) and Xiong et al. (2022, 2024).

`tempQC_internal` flags Tmin above Tmax, possible interchange relative
to the adjacent three-day window, and the 40-degree lagged range
conditions in Durre et al. (2010). It returns separate flags because the
erroneous member of an inconsistent pair is not identifiable
automatically.

`tempQC_breakpoint` aggregates sub-daily data to daily means, calculates
complete annual indicators, applies Pettitt rank tests, and adjusts
p-values across indicators. Boulanger et al. (2010) and Brunet et al.
(2020) motivate reporting homogeneity separately from point QC.

## Value

The range, duplicate, persistence, climatology, step, and spike tests
return a logical `zoo` object matching `x`. `tempQC_spatial` returns a
list with logical `flags`, numeric `scores`, numeric `estimate`, and
selected `neighbours`. `tempQC_internal` returns a list of Tmin and Tmax
flag objects. `tempQC_breakpoint` returns one data.frame row per
station.

## References

Beele, E. et al. (2022). Quality control and correction method for air
temperature data from a citizen science weather station network. *Earth
System Science Data*, 14, 4681–4717.

Boulanger, J.-P. et al. (2010). A procedure for automated quality
control and homogenization of historical daily temperature and
precipitation data. *Climatic Change*, 98, 471–491.

Brunet, M. et al. (2020). *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

Cheng, A. R. et al. (2016). Quality control program for real-time hourly
temperature observation in Taiwan. *Journal of Atmospheric and Oceanic
Technology*, 33, 953–965.

Durre, I. et al. (2010). Comprehensive automated quality assurance of
daily surface observations. *Journal of Applied Meteorology and
Climatology*, 49, 1615–1633.

Estevez, J. et al. (2018). Spatial regression test for ensuring
temperature data quality in southern Spain. *Theoretical and Applied
Climatology*, 131, 309–318.

Feng, S., Hu, Q. and Qian, W. (2004). Quality control of daily
meteorological data in China, 1951–2000. *International Journal of
Climatology*, 24, 853–870.

Hubbard, K. G. et al. (2007). An improved QC process for temperature in
the daily cooperative weather observations. *Journal of Atmospheric and
Oceanic Technology*, 24, 206–213.

Xiong, X. et al. (2022). Research on quality control methods for surface
temperature observations via spatial correlation analysis.
*International Journal of Climatology*. doi:10.1002/joc.7897.

Xiong, X. et al. (2024). Research on quality control method of surface
temperature observations for complex physical geography. *Journal of
Atmospheric and Oceanic Technology*, 41, 803–815.

## See also

[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:9
x <- zoo::zoo(cbind(A=c(rep(10, 7), 11, 12, 13)), dates)
tempQC_persistence(x)
#>                A
#> 2020-01-01  TRUE
#> 2020-01-02  TRUE
#> 2020-01-03  TRUE
#> 2020-01-04  TRUE
#> 2020-01-05  TRUE
#> 2020-01-06  TRUE
#> 2020-01-07  TRUE
#> 2020-01-08 FALSE
#> 2020-01-09 FALSE
#> 2020-01-10 FALSE
tempQC_range(x)
#>                A
#> 2020-01-01 FALSE
#> 2020-01-02 FALSE
#> 2020-01-03 FALSE
#> 2020-01-04 FALSE
#> 2020-01-05 FALSE
#> 2020-01-06 FALSE
#> 2020-01-07 FALSE
#> 2020-01-08 FALSE
#> 2020-01-09 FALSE
#> 2020-01-10 FALSE
```
