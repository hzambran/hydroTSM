# Frequency-aware quality control of streamflow time series

Detects the sampling frequency of a numeric `zoo` object and dispatches
daily data to
[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md)
or minute/hourly data to
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md).
No files are written.

## Usage

``` r
streamflowQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  area=NULL, elevation=NULL, elevation.scale=500
)
```

## Arguments

- x:

  A numeric `zoo` object with one streamflow station per column.

- metadata:

  `NULL`, or a data.frame with one row per station.

- station.id:

  Name of the station-identifier field in `metadata`.

- coords:

  Names of longitude and latitude fields in `metadata`, in that order.

- ...:

  Arguments passed unchanged to the selected workflow.

- area:

  `NULL`, or the name of a positive numeric catchment-area field. Units
  may be chosen by the user but must be consistent among stations.

- elevation:

  `NULL`, or the name of a numeric station-elevation field.

- elevation.scale:

  Positive elevation-decay scale used in spatial neighbour ranking.

## Details

The wrapper changes only the resolution-specific workflow. Test
activation, thresholds, station metadata, evidence requirements,
correction policy, and station recommendations remain explicit user
controls. Daily and sub-daily observations must be in consistent
discharge units. Duplicate timestamps are rejected because selecting
among contradictory measurements would require provenance or
agency-quality information not contained in a numeric `zoo` object.

The design combines the point plausibility and homogeneity principles of
Gudmundsson et al. (2018), the real-time hierarchy of U.S. IOOS (2018),
and the sub-daily artefact and event-corroboration framework of Fileni
et al. (2026).

## Value

An object of class `"streamflowQC"`; see
[`streamflowQC-class`](https://hzambran.github.io/hydroTSM/reference/streamflowQC-class.md).

## References

Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. *EGUsphere preprint*.
doi:10.5194/egusphere-2026-277.

Gudmundsson, L., Do, H. X., Leonard, M. and Westra, S. (2018). The
Global Streamflow Indices and Metadata Archive (GSIM) - Part 2: quality
control, time-series indices and homogeneity assessment. *Earth System
Science Data*, 10, 787–804. doi:10.5194/essd-10-787-2018.

U.S. Integrated Ocean Observing System (2018). *Manual for Real-Time
Quality Control of Stream Flow Data Version 1.0*.
doi:10.25923/gszc-ha43.

## See also

[`streamflowQC_daily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_daily.md),
[`streamflowQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_subdaily.md),
[`streamflowQC_tests`](https://hzambran.github.io/hydroTSM/reference/streamflowQC_tests.md),
[`precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC.md),
[`tempQC`](https://hzambran.github.io/hydroTSM/reference/tempQC.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:29
x <- zoo::zoo(cbind(A=10 + sin(seq_along(dates)),
                    B=12 + sin(seq_along(dates))), dates)
qc <- streamflowQC(
  x, checks=c(range=TRUE, duplicate=FALSE, gap=FALSE,
              climatology=FALSE, flatline=FALSE, spike=FALSE,
              rate=FALSE, highflow=FALSE, spatial=FALSE,
              breakpoint=FALSE),
  min.years=0, max.missing=1, max.review=1, max.suspicious=1
)
qc$station.summary
#>   station expected observed missing.percent review.count review.percent
#> A       A       30       30               0            0              0
#> B       B       30       30               0            0              0
#>   suspicious.count suspicious.percent breakpoint.year breakpoint.indicator
#> A                0                  0              NA                 <NA>
#> B                0                  0              NA                 <NA>
#>   breakpoint.p.value breakpoint.relative.change breakpoint.n.indicators
#> A                 NA                         NA                       0
#> B                 NA                         NA                       0
#>   breakpoint.flag record.years recommendation                   reason
#> A           FALSE   0.08213552         accept within acceptance limits
#> B           FALSE   0.08213552         accept within acceptance limits
```
