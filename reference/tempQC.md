# Frequency-aware quality control of air-temperature time series

Detects the sampling frequency of a numeric `zoo` object with
[`sfreq`](https://hzambran.github.io/hydroTSM/reference/sfreq.md) and
dispatches it to
[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md)
or
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md).
No files are written.

## Usage

``` r
tempQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  elevation=NULL, elevation.scale=500
)
```

## Arguments

- x:

  A numeric `zoo` object. Each column is one station.

- metadata, station.id, coords, elevation, elevation.scale:

  Optional station metadata, field-name selectors, and elevation-decay
  control passed to the selected workflow. See
  [`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md).

- ...:

  Arguments passed unchanged to the selected workflow.

## Details

Daily input is sent to `tempQC_daily`; minute or hourly input is sent to
`tempQC_subdaily`. Other frequencies are rejected explicitly. Automatic
dispatch changes only the workflow: test activation, thresholds,
metadata, correction policy, and station recommendation criteria remain
under user control. The staged design follows the gross-error,
tolerance, temporal, inter-variable, and inter-station QC categories
described by Brunet et al. (2020).

## Value

An object of class `"tempQC"`; see
[`tempQC-class`](https://hzambran.github.io/hydroTSM/reference/tempQC-class.md).

## References

Brunet, M. et al. (2020). *Best Practice Guidelines for Climate Data and
Metadata Formatting, Quality Control and Submission*. Copernicus Climate
Change Service.

## See also

[`tempQC_daily`](https://hzambran.github.io/hydroTSM/reference/tempQC_daily.md),
[`tempQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/tempQC_subdaily.md),
[`tempQC_tests`](https://hzambran.github.io/hydroTSM/reference/tempQC_tests.md),
[`precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC.md)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:29
x <- zoo::zoo(cbind(A=10 + sin(seq_along(dates)),
                    B=11 + sin(seq_along(dates))), dates)
qc <- tempQC(x, checks=c(range=TRUE, duplicate=FALSE,
                          climatology=FALSE, persistence=FALSE,
                          step=FALSE, spike=FALSE, spatial=FALSE,
                          breakpoint=FALSE), min.years=0)
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
