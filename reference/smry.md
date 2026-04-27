# Summary

Extended summary function for numeric objects, with 13 summary
statistics.

## Usage

``` r
smry(x, ...)

# Default S3 method
smry(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

# S3 method for class 'zoo'
smry(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

# S3 method for class 'Date'
smry(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

# S3 method for class 'matrix'
smry(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

# S3 method for class 'data.frame'
smry(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)
```

## Arguments

- x:

  a numeric object, vector, matrix or data.frame, for which a summary is
  desired.

- na.rm:

  a logical value indicating whether 'NA' values should be stripped
  before the computation proceeds.

- digits:

  numeric, with the amount of decimal places to be included in the
  result

- ...:

  further arguments passed to or from other methods.

## Value

Computed summary statistics are:  

- Min:

  Minimum

- 1stQ:

  First quartile (lower-hinge)

- Mean:

  Mean value

- Median:

  Median

- 3rdQ:

  Third quartile ( upper-hinge

- Max:

  Maximum of the input values.

- IQR:

  Interquartile Range. `IQR(x) = quantile(x,3/4) - quantile(x,1/4)`

- sd:

  Standard deviation. It uses 'n-1' as denominator.

- cv:

  Coefficient of variation ( `cv= sd / |mean|` )

- skewness:

  Skewness (using e1071 package)

- kurtosis:

  Kurtosis (using e1071 package)

- n:

  Total number of elements

- NA's:

  Amount of missing values

## Author

Mauricio Zambrano-Bigiarini <mzb.devel@gmail>

## Note

Skewness and Kurtosis are computed with the e1071 package

## See also

[`summary`](https://rdrr.io/r/base/summary.html),
[`fivenum`](https://rdrr.io/r/stats/fivenum.html),
[`IQR`](https://rdrr.io/r/stats/IQR.html),
[`sd`](https://rdrr.io/r/stats/sd.html),
[skewness](https://rdrr.io/pkg/e1071/man/skewness.html),
[kurtosis](https://rdrr.io/pkg/e1071/man/kurtosis.html)

## Examples

``` r
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Summary of monthly precipitation values for the first 7 stations in 'EbroPPtsMonthly'
smry(EbroPPtsMonthly[,2:8])
#>             P9001   P9008X    P9012    P9015    P9019    P9027    P9034
#> Min.       0.2000   0.0000   0.0000   0.0000   0.0000   0.0000   0.0000
#> 1st Qu.   31.0200  36.9200  32.9300  24.0500  23.9000  25.4200  20.8800
#> Median    54.5000  66.0500  58.7000  51.1000  43.8500  48.9000  44.9000
#> Mean      71.9100  85.1700  75.9000  56.0400  55.2800  56.0500  52.9900
#> 3rd Qu.   93.9000 118.4000  99.0800  81.7500  71.0500  71.7000  71.4300
#> Max.     332.3000 363.0000 250.2000 177.6000 222.0000 199.8000 264.0000
#> IQR       62.8750  81.4500  66.1500  57.7000  47.1500  46.2750  50.5500
#> sd        61.3760  70.6334  58.4684  39.0676  41.4306  42.1284  45.5866
#> cv         0.8536   0.8293   0.7703   0.6972   0.7495   0.7516   0.8603
#> Skewness   1.8593   1.4694   1.1176   0.9089   1.4439   1.2535   1.4455
#> Kurtosis   4.2949   2.3423   0.5254   0.4836   2.3071   1.7802   2.9941
#> NA's       0.0000   0.0000   0.0000   0.0000   0.0000   0.0000   0.0000
#> n        120.0000 120.0000 120.0000 120.0000 120.0000 120.0000 120.0000
```
