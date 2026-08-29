# Frequency-aware quality control of precipitation time series

Detects the sampling frequency of a precipitation `zoo` object with
[`sfreq`](https://hzambran.github.io/hydroTSM/reference/sfreq.md) and
dispatches to
[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md)
or
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md).
No files are written.

## Usage

``` r
precipQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  elevation=NULL, elevation.scale=500
)
```

## Arguments

- x:

  A univariate or multivariate numeric `zoo` object. Each column is one
  precipitation station and must have a unique name.

- metadata, station.id, coords, elevation, elevation.scale:

  Optional station metadata, field-name selectors, and elevation-decay
  control passed to the selected workflow. See
  [`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md).

- ...:

  Arguments passed unchanged to the selected QC function. Because the
  daily and sub-daily functions have resolution-specific arguments, only
  arguments accepted by the detected workflow should be supplied.

## Details

`sfreq(x)` returning `"daily"` selects `precipQC_daily(x, ...)`. Values
`"minute"` and `"hourly"` select `precipQC_subdaily(x, ...)`. Weekly,
monthly, quarterly, annual, or other frequencies are rejected with an
informative error because the implemented thresholds and tests are not
valid at those resolutions.

## Value

An object of class `"precipQC"`; see
[`precipQC-class`](https://hzambran.github.io/hydroTSM/reference/precipQC-class.md).

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`sfreq`](https://hzambran.github.io/hydroTSM/reference/sfreq.md),
[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md),
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md),
[`plot.precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC-class.md)

## Examples

``` r
dates <- seq(as.Date("2020-01-01"), by="day", length.out=40)
x <- zoo(cbind(A=rep(0, 40), B=rep(0, 40)), dates)

qc <- precipQC(
  x, min.years=0,
  checks=c(climatology=FALSE, spatial=FALSE, dryspell=FALSE,
           breakpoint=FALSE)
)
qc$settings$resolution
#> [1] "daily"
```
