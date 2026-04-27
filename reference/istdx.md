# Inverse Standarization

This function back transforms a standarized vector/matrix `z` into their
original values, i.e., re-scales all the values in the \[0,1\] interval
to the original range of values
`z = re-scale(x) = x*[ xmax - xmin ] + xmin`.

## Usage

``` r
istdx(x, ...)
# Default S3 method
istdx(x, xmin, xrange, ...)
```

## Arguments

- x:

  standarized vector or matrix to be re-scaled, all the values have to
  be in the range \[0,1\]

- xmin:

  numeric with the minimum value(s) in the original `x`  
  -) if `x` is a vector, `xmin` has to be a real  
  -) if `x` is a matrix/data.frame, `xmin` has to be a vector, with the
  minimum values for each column of the original `x`. In this case, the
  vector of minimums can be obtained as:
  `xmin <- apply(x, 2, min, na.rm=TRUE)`

- xrange:

  numeric with the range of value(s) in the original `x`  
  -) if `x` is a vector, `xrange` has to be a real  
  -) if `x` is a matrix/data.frame, `xrange` has to be a vector, with
  the range of values for each column of the original `x`. In this case,
  the vector of ranges can be obtained as:  
  `xrange <- apply(x, 2,range, na.rm=TRUE)`  
  `xrange <- apply(xrange, 2, diff, na.rm=TRUE)`

- ...:

  further arguments passed to or from other methods

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`stdx`](https://hzambran.github.io/hydroTSM/reference/stdx.md),
[`scale`](https://rdrr.io/r/base/scale.html)

## Examples

``` r
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Computing xmin and the range of 'x'
xmin <- min(x, na.rm=TRUE)
r <- diff(range(x, na.rm=TRUE))

## Standarized variable
s <- stdx(x)

## Inverse of the standarized variable
si <- istdx(s, xmin, xrange=r)

## 'si' and 'x' should be the same
summary(x-si)
#>      Index                x - si          
#>  Min.   :1961-01-01   Min.   :-3.553e-15  
#>  1st Qu.:1961-10-01   1st Qu.: 0.000e+00  
#>  Median :1962-07-02   Median : 0.000e+00  
#>  Mean   :1962-07-02   Mean   : 1.176e-17  
#>  3rd Qu.:1963-04-01   3rd Qu.: 0.000e+00  
#>  Max.   :1963-12-31   Max.   : 3.553e-15  

###########
### Standarizing a subset of the stations 9 to 12 in 'EbroPPtsMonthly'

## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

pp <- EbroPPtsMonthly[1:70,10:13]
xmin   <- apply(pp, 2, min, na.rm=TRUE)
xrange <- apply(pp, 2, range, na.rm=TRUE)
xrange <- apply(xrange, 2, diff, na.rm=TRUE)

## Standarized variable
s <- stdx(as.matrix(pp))

## Inverse of the standarized variable
si <- istdx(s, xmin, xrange)

## 'si' and 'pp' should be the same
summary(pp - si)
#>      P9041                P9048                P9056           
#>  Min.   :-1.421e-14   Min.   :-1.421e-14   Min.   :-7.105e-15  
#>  1st Qu.: 0.000e+00   1st Qu.: 0.000e+00   1st Qu.: 0.000e+00  
#>  Median : 0.000e+00   Median : 0.000e+00   Median : 0.000e+00  
#>  Mean   : 1.015e-16   Mean   : 1.269e-17   Mean   : 1.523e-16  
#>  3rd Qu.: 0.000e+00   3rd Qu.: 0.000e+00   3rd Qu.: 0.000e+00  
#>  Max.   : 1.421e-14   Max.   : 1.421e-14   Max.   : 1.421e-14  
#>      P9060          
#>  Min.   :0.000e+00  
#>  1st Qu.:0.000e+00  
#>  Median :0.000e+00  
#>  Mean   :1.047e-16  
#>  3rd Qu.:0.000e+00  
#>  Max.   :7.105e-15  
```
