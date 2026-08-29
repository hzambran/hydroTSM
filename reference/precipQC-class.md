# Precipitation quality-control result

Print and plot methods for objects returned by
[`precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC.md),
[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md),
and
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md).

## Usage

``` r
# S3 method for class 'precipQC'
print(x, ...)

# S3 method for class 'precipQC'
plot(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
```

## Arguments

- x:

  An object of class `"precipQC"`.

- max.stations:

  Maximum number of stations displayed in the missing/suspicious
  percentage panel. Stations with the largest suspicious and missing
  fractions are shown.

- col:

  At least three colours for accepted stations, discarded/rejected
  values, and missing/review flags.

- ...:

  For `plot`, additional arguments passed to the final timeline
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) call. Ignored
  by `print`.

## Details

The plot contains four panels: numbers of accepted and discarded
stations; missing and confirmed-suspicious percentages for the most
affected stations; flag counts by individual test; and the number of
confirmed suspicious values through time.

## Value

Both methods return `x` invisibly. The print method writes a compact
summary to the console and the plot method draws on the active graphics
device.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`precipQC`](https://hzambran.github.io/hydroTSM/reference/precipQC.md),
[`precipQC_daily`](https://hzambran.github.io/hydroTSM/reference/precipQC_daily.md),
[`precipQC_subdaily`](https://hzambran.github.io/hydroTSM/reference/precipQC_subdaily.md),
[`precipQC_range`](https://hzambran.github.io/hydroTSM/reference/precipQC_tests.md)
