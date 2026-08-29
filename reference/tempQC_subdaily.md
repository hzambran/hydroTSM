# Quality control of sub-daily air-temperature time series

Applies physical, climatological, temporal, spatial, and homogeneity
checks to minute or hourly air-temperature station series in a `zoo`
object. The method preserves the native time index and writes no files.

## Usage

``` r
tempQC_subdaily(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"),
  checks=c(range=TRUE, climatology=TRUE, persistence=TRUE,
           step=TRUE, spike=TRUE, spatial=TRUE, breakpoint=TRUE),
  lower=-89.4, upper=57.7,
  climatology.z=6, climatology.min.samples=30L,
  persistence.hours=3, persistence.tolerance=0.05,
  step.z=5, step.min.samples=30L,
  step.max.rise=Inf, step.max.fall=Inf,
  spike.threshold=12,
  n.neighbours=5L, max.distance=50, min.neighbours=2L,
  min.overlap=100L, min.group.overlap=20L,
  min.correlation=0.7, spatial.f=4, spatial.min.difference=5,
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

  A numeric `zoo` object with a strictly increasing POSIXt index at
  minute or hourly resolution. Each column is a station in degrees
  Celsius.

- metadata:

  `NULL`, or station metadata with one row per column. When supplied, it
  must include the selected identifier, longitude, and latitude fields.

- station.id:

  Name of the station identifier column in `metadata`.

- coords:

  Names of longitude and latitude metadata columns in decimal degrees,
  in that order.

- elevation:

  `NULL`, or the name of an optional numeric elevation column in metres.

- elevation.scale:

  Positive height-decay scale in metres used in neighbour selection and
  weighting when `elevation` is supplied.

- checks:

  Named logical vector that overrides any of `"range"`, `"climatology"`,
  `"persistence"`, `"step"`, `"spike"`, `"spatial"`, and `"breakpoint"`.

- lower, upper:

  Physical screening limits, scalar or station-specific.

- climatology.z:

  Absolute robust anomaly threshold within each calendar-month and
  hour-of-day group.

- climatology.min.samples:

  Minimum group sample size.

- persistence.hours:

  Duration of an almost-flat period. The corresponding number of
  observations is derived from the modal sampling interval.

- persistence.tolerance:

  Maximum temperature range within that period.

- step.z:

  Robust standardized threshold for consecutive changes within each
  month and hour.

- step.min.samples:

  Minimum first-difference sample per group.

- step.max.rise, step.max.fall:

  Optional absolute consecutive rise and fall limits; use regional,
  interval-specific values when available.

- spike.threshold:

  Minimum difference from both adjacent observations for an isolated
  spike or dip.

- n.neighbours, max.distance, min.neighbours:

  Maximum neighbour count, maximum distance in km, and minimum
  simultaneous estimates.

- min.overlap, min.group.overlap:

  Minimum overall and month-hour paired samples for neighbour selection
  and regression fitting.

- min.correlation:

  Minimum eligible target-neighbour Pearson correlation.

- spatial.f:

  Minimum standardized spatial residual.

- spatial.min.difference:

  Minimum absolute spatial residual in degrees Celsius. Both spatial
  thresholds must be exceeded.

- breakpoint.min.years, breakpoint.alpha, breakpoint.min.shift,
  breakpoint.min.completeness:

  Minimum annual sample size, Holm-adjusted significance level, minimum
  absolute median shift, and annual completeness for the homogeneity
  diagnostic.

- correction:

  `"none"`, `"set_na"`, or `"spatial"`; see
  [`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md).

- min.evidence:

  Number of coincident non-hard tests needed to reject a point. Range,
  persistence, and isolated-spike flags are hard evidence.

- max.missing, max.suspicious, min.years:

  Station acceptance limits.

- discard.breakpoint:

  Whether a flagged breakpoint should cause a station discard
  recommendation.

## Details

The sub-daily workflow uses the same evidence and audit model as
[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
but its climatological, first-difference, and spatial-regression
reference groups are conditioned jointly on calendar month and hour of
day. Cheng et al. (2016) showed that hourly reference-station relations
can change sharply by hour and season, especially across complex
terrain.

1.  *Range*: global physical limits screen gross coding and unit errors.
    Local limits may be supplied; elevation-adjusted regional limits are
    preferable where established (Cheng et al., 2016).

2.  *Climatology*: robust median/MAD limits within month-hour groups
    preserve the diurnal and seasonal cycle (Cheng et al., 2016; Brunet
    et al., 2020).

3.  *Persistence*: the default identifies a temperature range no greater
    than 0.05 degrees across three hours, adapting the 10-minute
    flat-line test of Beele et al. (2022) to the detected sampling
    interval.

4.  *Steps and isolated spikes*: month-hour first differences are
    standardized robustly. Optional absolute rise and fall limits
    support local real-time criteria. Cheng et al. (2016) demonstrate
    why weather- and hour-dependent limits are safer than a universal
    hourly threshold; Beele et al. (2022) provide an operational
    10-minute example.

5.  *Spatial regression*: correlated neighbours are chosen by
    proximity-adjusted regression error and combined with proximity and
    inverse-error-variance weights. Geographic distance and, when
    supplied, elevation difference define proximity. Models are fitted
    separately by month and hour. This implements the SRT family
    evaluated by Hubbard et al. (2007), Cheng et al. (2016), and Estevez
    et al. (2018), while the correlation screen addresses the terrain
    sensitivity discussed by Xiong et al. (2022, 2024).

6.  *Homogeneity*: native values are converted to daily means before
    annual mean and variability breakpoint diagnostics. The result is
    not a point flag and does not cause discard unless explicitly
    requested.

## Value

An object of class `"tempQC"`; see
[`tempQC-class`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md).

## References

Beele, E., Reyniers, M., Aerts, R. and Somers, B. (2022). Quality
control and correction method for air temperature data from a citizen
science weather station network in Leuven, Belgium. *Earth System
Science Data*, 14, 4681–4717.

Brunet, M. et al. (2020). *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

Cheng, A. R., Lee, T. H., Ku, H. I. and Chen, Y. W. (2016). Quality
control program for real-time hourly temperature observation in Taiwan.
*Journal of Atmospheric and Oceanic Technology*, 33, 953–965.

Estevez, J., Gavilan, P. and Garcia-Marin, A. P. (2018). Spatial
regression test for ensuring temperature data quality in southern Spain.
*Theoretical and Applied Climatology*, 131, 309–318.

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
[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
[`tempQC_tests`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md),
[`plot.tempQC`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md)

## Examples

``` r
times <- seq(as.POSIXct("2020-01-01", tz="UTC"), by="hour", length.out=72)
h <- as.integer(format(times, "%H"))
v <- 12 + 5 * sin(2 * pi * h / 24)
x <- zoo::zoo(cbind(A=v, B=v + 0.2), times)
qc <- tempQC_subdaily(
  x, checks=c(range=TRUE, climatology=FALSE, persistence=TRUE,
              step=FALSE, spike=TRUE, spatial=FALSE,
              breakpoint=FALSE),
  min.years=0, max.missing=1
)
qc$station.summary
#>   station expected observed missing.percent review.count review.percent
#> A       A       72       72               0            0              0
#> B       B       72       72               0            0              0
#>   suspicious.count suspicious.percent breakpoint.year breakpoint.indicator
#> A                0                  0              NA                 <NA>
#> B                0                  0              NA                 <NA>
#>   breakpoint.p.value breakpoint.relative.change breakpoint.n.indicators
#> A                 NA                         NA                       0
#> B                 NA                         NA                       0
#>   breakpoint.flag record.years recommendation                   reason
#> A           FALSE  0.008213552         accept within acceptance limits
#> B           FALSE  0.008213552         accept within acceptance limits
```
