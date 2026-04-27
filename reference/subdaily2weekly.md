# Subdaily -\> Weekly

Generic function for transforming a DAILY (or sub-daily) regular time
series into a WEEKLY one

## Usage

``` r
subdaily2weekly(x, ...)

# Default S3 method
subdaily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "%H:%M:%S", tz, ...)

# S3 method for class 'zoo'
subdaily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "%H:%M:%S", tz, ...)

# S3 method for class 'data.frame'
subdaily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "%H:%M:%S", tz,
           dates=1, date.fmt = "%Y-%m-%d %H:%M:%S",  
           out.fmt="zoo", verbose=TRUE, ...)

# S3 method for class 'matrix'
subdaily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "%H:%M:%S", tz,
           dates=1, date.fmt = "%Y-%m-%d %H:%M:%S",  
           out.fmt="zoo", verbose=TRUE, ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with (sub)daily time series.  
  Measurements at several gauging stations can be stored in a data.frame
  or matrix object, and in that case, each column of `x` represents the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- FUN:

  Function that have to be applied for transforming from daily to weekly
  time step (e.g., for precipitation `FUN=sum` and for temperature and
  streamflow ts `FUN=mean`).  

  `FUN` MUST accept the `na.rm` argument, because `na.rm` is passed to
  `FUN`.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the weekly values are computed only for weeks with a
  percentage of missing values less than `na.rm.max`  
  -) FALSE: if there is AT LEAST one NA within a month, the corresponing
  weekly values in the output object will be `NA`.

- na.rm.max:

  Numeric in \[0, 1\]. It is used to define the maximum percentage of
  missing values allowed in each week to keep the weekly aggregated
  value in the output object of this function. In other words, if the
  percentage of missing values in a given week is larger than
  `na.rm.max` the corresponding weekly value will be `NA`.

- start:

  character, indicating the starting time used for aggregating sub-daily
  time series into daily ones. It MUST be provided in the format
  specified by `start.fmt`.  
  This value is used to define the time when a new day begins (e.g., for
  some rain gauge stations).  
  -) All the values of `x` with a time attribute before `start` are
  considered as belonging to the day before the one indicated in the
  time attribute of those values.  
  -) All the values of `x` with a time attribute equal to `start` are
  considered to be equal to `"00:00:00"` in the output zoo object.  
  -) All the values of `x` with a time attribute after `start` are
  considered as belonging to the same day as the one indicated in the
  time attribute of those values.  

  It is useful when the daily values start at a time different from
  `"00:00:00"`. Use with caution. See examples.

- start.fmt:

  character indicating the format in which the time is provided in
  `start`, By default `date.fmt=%H:%M:%S`. See `format` in
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).

- tz:

  character, with the specification of the time zone used in both `x`
  and `start`. System-specific (see time zones), but `""` is the current
  time zone, and `"GMT"` is UTC (Universal Time, Coordinated). See
  [`Sys.timezone`](https://rdrr.io/r/base/timezones.html) and
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).  
  If `tz` is missing (the default), it is automatically set to the time
  zone used in `time(x)`.  
  This argument can be used to force using the local time zone or any
  other time zone instead of UTC as time zone.

- dates:

  numeric, factor or Date object indicating how to obtain the dates for
  each gauging station  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into Date class, using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days on it be equal to the number of elements in `x`

- date.fmt:

  character indicating the format in which the DateTime objects are
  stored in `dates`, e.g. %Y-%m-%d %H:%M:%S. See `format` in
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.fmt:

  OPTIONAL. Only used when `x` is a matrix or data.frame object /cr
  character, for selecting if the result will be a matrix/data.frame or
  a zoo object. Valid values are: numeric, zoo.

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  arguments additional to `na.rm` passed to `FUN`.

## Value

a zoo object with weekly time frequency

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`cmv`](https://hzambran.github.io/hydroTSM/reference/cmv.md),
[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`daily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
[`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md),
[`weeklyfunction`](https://hzambran.github.io/hydroTSM/reference/weeklyfunction.md),
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md),
[`as.Date`](https://rdrr.io/r/base/as.Date.html)

## Examples

``` r
######################
## Ex1: Computation of WEEKLY values from HOURLY ts, only when the percentage of NAs in 
#       each week is lower than a user-defined percentage (10% in this example).

## Loading the HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Subsetting 'x' to its first three weeks 
# (31-December-1979 to 31-March-1980)
x <- window(x, end="1980-03-31 23:59:00")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Daily to Weekly, only for weeks with less than 10% of missing values
( w2 <- subdaily2weekly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1) )
#> 1979-53 1980-00 1980-01 1980-02 1980-03 1980-04 1980-05 1980-06 1980-07 1980-08 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-09 1980-10 1980-11 1980-12 1980-13 
#>      NA      NA      NA      NA      NA 

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="weekly")
#>              
#> 1979-53 0.250
#> 1980-00 0.083
#> 1980-01 0.101
#> 1980-02 0.083
#> 1980-03 0.089
#> 1980-04 0.107
#> 1980-05 0.095
#> 1980-06 0.095
#> 1980-07 0.071
#> 1980-08 0.155
#> 1980-09 0.131
#> 1980-10 0.113
#> 1980-11 0.083
#> 1980-12 0.089
#> 1980-13 0.125

if (FALSE) { # \dontrun{
######################
## Ex2: Computation of WEEKLY values from HOURLY ts, removing any missing value in 'x'
#       Loading the HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to weekly ts
subdaily2weekly(x, FUN=mean, na.rm=TRUE)
} # }
```
