# Days with Information

This function generates a table indicating the number of days with
information (\<\>NA) within a zoo object, aggregated by year, month or
month per year.

## Usage

``` r
dwi(x, ...)

# Default S3 method
dwi(x, out.unit = "years", from = start(x), to = end(x), 
     date.fmt = "%Y-%m-%d", tstep="days", ...)
     
# S3 method for class 'zoo'
dwi(x, out.unit = "years", from = start(x), to = end(x), 
     date.fmt = "%Y-%m-%d", tstep="days", ...)

# S3 method for class 'data.frame'
dwi(x, out.unit = "years", from, to, date.fmt = "%Y-%m-%d", tstep="days", 
     dates = 1, verbose = TRUE, ...)
     
# S3 method for class 'matrix'
dwi(x, out.unit = "years", from, to, date.fmt = "%Y-%m-%d", tstep="days", 
     dates = 1, verbose = TRUE, ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with daily/monthly/annual time
  series.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- out.unit:

  aggregation time for the computation of the amount of days with
  information. Valid values are:  
  -) months: monthly;  
  -) years : annual;  
  -) mpy : month per year (not available for data.frames)

- from:

  Character indicating the starting date for the computations. It has to
  be in the format indicated by `date.fmt`.  
  When `x` is a data.frame and this value is not provided, the date
  corresponding to the first row of `x` is used

- to:

  Character indicating the ending date for the computations. It has to
  be in the format indicated by `date.fmt`.  
  When `x` is a data.frame and this value is not provided, the date
  corresponding to the last row of `x` is used

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- tstep:

  Time step used for storing the values in `x`. Valid values are: days,
  months, years. Since the version 0.3-0 of hydroTSM, this argument is
  not required any more, because it is not used any longer.

- dates:

  numeric, factor or Date object indicating how to obtain the dates for
  each column of `x`  
  If `dates` is a number, it indicates the index of the column in `x`
  that stores the dates  
  If `dates` is a factor, it is converted into Date class, using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days in `dates` be equal to the number of element in `x`

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  further arguments passed to or from other methods.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`matrixplot`](https://hzambran.github.io/hydroTSM/reference/matrixplot.md)

## Examples

``` r
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

if (FALSE) { # \dontrun{
## Days with information per year
dwi(x)

## Days with information per month per year.
dwi(x, out.unit="mpy")
} # }

###########
if (FALSE) { # \dontrun{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Months with information per year in the 9 first stations of 'EbroPPtsMonthly'
a <- dwi(EbroPPtsMonthly[,1:10], out.unit="years", dates=1)

## Before plotting the results in 'a', and just for obtaining a more interesting
## plot, 70 random numbers (between 1 and 11) are introduced in 'a'
a[sample(length(a), size = 70)] <- rep(1:11, length=70)

## Plotting the amount of months with information per year in each station
matrixplot(a, var.type="Days", main="Number of months with info per year")
} # }
```
