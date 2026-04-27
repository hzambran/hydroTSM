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
# (Monday 2nd December 1985 - Sunday 29th uanuary 20th 1980)
x <- window(x, end="1985-12-31 23:59:00")

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
#> 1980-09 1980-10 1980-11 1980-12 1980-13 1980-14 1980-15 1980-16 1980-17 1980-18 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-19 1980-20 1980-21 1980-22 1980-23 1980-24 1980-25 1980-26 1980-27 1980-28 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-29 1980-30 1980-31 1980-32 1980-33 1980-34 1980-35 1980-36 1980-37 1980-38 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-39 1980-40 1980-41 1980-42 1980-43 1980-44 1980-45 1980-46 1980-47 1980-48 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-49 1980-50 1980-51 1980-52 1981-00 1981-01 1981-02 1981-03 1981-04 1981-05 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-06 1981-07 1981-08 1981-09 1981-10 1981-11 1981-12 1981-13 1981-14 1981-15 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-16 1981-17 1981-18 1981-19 1981-20 1981-21 1981-22 1981-23 1981-24 1981-25 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-26 1981-27 1981-28 1981-29 1981-30 1981-31 1981-32 1981-33 1981-34 1981-35 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-36 1981-37 1981-38 1981-39 1981-40 1981-41 1981-42 1981-43 1981-44 1981-45 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-46 1981-47 1981-48 1981-49 1981-50 1981-51 1981-52 1982-00 1982-01 1982-02 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-03 1982-04 1982-05 1982-06 1982-07 1982-08 1982-09 1982-10 1982-11 1982-12 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-13 1982-14 1982-15 1982-16 1982-17 1982-18 1982-19 1982-20 1982-21 1982-22 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-23 1982-24 1982-25 1982-26 1982-27 1982-28 1982-29 1982-30 1982-31 1982-32 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-33 1982-34 1982-35 1982-36 1982-37 1982-38 1982-39 1982-40 1982-41 1982-42 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-43 1982-44 1982-45 1982-46 1982-47 1982-48 1982-49 1982-50 1982-51 1982-52 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-00 1983-01 1983-02 1983-03 1983-04 1983-05 1983-06 1983-07 1983-08 1983-09 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-10 1983-11 1983-12 1983-13 1983-14 1983-15 1983-16 1983-17 1983-18 1983-19 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-20 1983-21 1983-22 1983-23 1983-24 1983-25 1983-26 1983-27 1983-28 1983-29 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-30 1983-31 1983-32 1983-33 1983-34 1983-35 1983-36 1983-37 1983-38 1983-39 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-40 1983-41 1983-42 1983-43 1983-44 1983-45 1983-46 1983-47 1983-48 1983-49 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-50 1983-51 1983-52 1984-00 1984-01 1984-02 1984-03 1984-04 1984-05 1984-06 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-07 1984-08 1984-09 1984-10 1984-11 1984-12 1984-13 1984-14 1984-15 1984-16 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-17 1984-18 1984-19 1984-20 1984-21 1984-22 1984-23 1984-24 1984-25 1984-26 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-27 1984-28 1984-29 1984-30 1984-31 1984-32 1984-33 1984-34 1984-35 1984-36 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-37 1984-38 1984-39 1984-40 1984-41 1984-42 1984-43 1984-44 1984-45 1984-46 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-47 1984-48 1984-49 1984-50 1984-51 1984-52 1984-53 1985-00 1985-01 1985-02 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-03 1985-04 1985-05 1985-06 1985-07 1985-08 1985-09 1985-10 1985-11 1985-12 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-13 1985-14 1985-15 1985-16 1985-17 1985-18 1985-19 1985-20 1985-21 1985-22 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-23 1985-24 1985-25 1985-26 1985-27 1985-28 1985-29 1985-30 1985-31 1985-32 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-33 1985-34 1985-35 1985-36 1985-37 1985-38 1985-39 1985-40 1985-41 1985-42 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-43 1985-44 1985-45 1985-46 1985-47 1985-48 1985-49 1985-50 1985-51 1985-52 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="weekly")
#>              
#> 1979-53 0.500
#> 1980-00 0.104
#> 1980-01 0.060
#> 1980-02 0.083
#> 1980-03 0.101
#> 1980-04 0.089
#> 1980-05 0.125
#> 1980-06 0.107
#> 1980-07 0.065
#> 1980-08 0.095
#> 1980-09 0.125
#> 1980-10 0.071
#> 1980-11 0.065
#> 1980-12 0.125
#> 1980-13 0.113
#> 1980-14 0.125
#> 1980-15 0.089
#> 1980-16 0.107
#> 1980-17 0.083
#> 1980-18 0.083
#> 1980-19 0.125
#> 1980-20 0.125
#> 1980-21 0.089
#> 1980-22 0.083
#> 1980-23 0.065
#> 1980-24 0.143
#> 1980-25 0.065
#> 1980-26 0.089
#> 1980-27 0.060
#> 1980-28 0.125
#> 1980-29 0.095
#> 1980-30 0.077
#> 1980-31 0.089
#> 1980-32 0.083
#> 1980-33 0.107
#> 1980-34 0.113
#> 1980-35 0.089
#> 1980-36 0.125
#> 1980-37 0.077
#> 1980-38 0.096
#> 1980-39 0.095
#> 1980-40 0.078
#> 1980-41 0.089
#> 1980-42 0.125
#> 1980-43 0.131
#> 1980-44 0.095
#> 1980-45 0.113
#> 1980-46 0.077
#> 1980-47 0.077
#> 1980-48 0.089
#> 1980-49 0.095
#> 1980-50 0.113
#> 1980-51 0.125
#> 1980-52 0.069
#> 1981-00 0.104
#> 1981-01 0.131
#> 1981-02 0.065
#> 1981-03 0.060
#> 1981-04 0.113
#> 1981-05 0.131
#> 1981-06 0.083
#> 1981-07 0.101
#> 1981-08 0.095
#> 1981-09 0.101
#> 1981-10 0.095
#> 1981-11 0.083
#> 1981-12 0.137
#> 1981-13 0.125
#> 1981-14 0.095
#> 1981-15 0.095
#> 1981-16 0.137
#> 1981-17 0.095
#> 1981-18 0.113
#> 1981-19 0.131
#> 1981-20 0.095
#> 1981-21 0.125
#> 1981-22 0.125
#> 1981-23 0.089
#> 1981-24 0.048
#> 1981-25 0.095
#> 1981-26 0.143
#> 1981-27 0.125
#> 1981-28 0.054
#> 1981-29 0.119
#> 1981-30 0.113
#> 1981-31 0.137
#> 1981-32 0.083
#> 1981-33 0.089
#> 1981-34 0.101
#> 1981-35 0.089
#> 1981-36 0.119
#> 1981-37 0.113
#> 1981-38 0.126
#> 1981-39 0.060
#> 1981-40 0.090
#> 1981-41 0.095
#> 1981-42 0.065
#> 1981-43 0.089
#> 1981-44 0.065
#> 1981-45 0.101
#> 1981-46 0.125
#> 1981-47 0.060
#> 1981-48 0.119
#> 1981-49 0.113
#> 1981-50 0.095
#> 1981-51 0.131
#> 1981-52 0.073
#> 1982-00 0.097
#> 1982-01 0.119
#> 1982-02 0.107
#> 1982-03 0.083
#> 1982-04 0.089
#> 1982-05 0.083
#> 1982-06 0.095
#> 1982-07 0.089
#> 1982-08 0.107
#> 1982-09 0.143
#> 1982-10 0.089
#> 1982-11 0.101
#> 1982-12 0.095
#> 1982-13 0.083
#> 1982-14 0.083
#> 1982-15 0.113
#> 1982-16 0.095
#> 1982-17 0.125
#> 1982-18 0.077
#> 1982-19 0.125
#> 1982-20 0.083
#> 1982-21 0.113
#> 1982-22 0.077
#> 1982-23 0.125
#> 1982-24 0.107
#> 1982-25 0.089
#> 1982-26 0.101
#> 1982-27 0.095
#> 1982-28 0.107
#> 1982-29 0.095
#> 1982-30 0.113
#> 1982-31 0.101
#> 1982-32 0.125
#> 1982-33 0.083
#> 1982-34 0.107
#> 1982-35 0.071
#> 1982-36 0.101
#> 1982-37 0.119
#> 1982-38 0.072
#> 1982-39 0.125
#> 1982-40 0.066
#> 1982-41 0.095
#> 1982-42 0.113
#> 1982-43 0.113
#> 1982-44 0.119
#> 1982-45 0.107
#> 1982-46 0.107
#> 1982-47 0.125
#> 1982-48 0.101
#> 1982-49 0.143
#> 1982-50 0.125
#> 1982-51 0.137
#> 1982-52 0.108
#> 1983-00 0.062
#> 1983-01 0.077
#> 1983-02 0.107
#> 1983-03 0.101
#> 1983-04 0.113
#> 1983-05 0.131
#> 1983-06 0.083
#> 1983-07 0.083
#> 1983-08 0.095
#> 1983-09 0.137
#> 1983-10 0.131
#> 1983-11 0.095
#> 1983-12 0.113
#> 1983-13 0.101
#> 1983-14 0.095
#> 1983-15 0.119
#> 1983-16 0.119
#> 1983-17 0.131
#> 1983-18 0.048
#> 1983-19 0.155
#> 1983-20 0.107
#> 1983-21 0.089
#> 1983-22 0.113
#> 1983-23 0.077
#> 1983-24 0.125
#> 1983-25 0.089
#> 1983-26 0.113
#> 1983-27 0.089
#> 1983-28 0.107
#> 1983-29 0.107
#> 1983-30 0.137
#> 1983-31 0.107
#> 1983-32 0.083
#> 1983-33 0.113
#> 1983-34 0.089
#> 1983-35 0.095
#> 1983-36 0.125
#> 1983-37 0.089
#> 1983-38 0.060
#> 1983-39 0.101
#> 1983-40 0.072
#> 1983-41 0.060
#> 1983-42 0.107
#> 1983-43 0.060
#> 1983-44 0.077
#> 1983-45 0.125
#> 1983-46 0.131
#> 1983-47 0.095
#> 1983-48 0.060
#> 1983-49 0.101
#> 1983-50 0.125
#> 1983-51 0.113
#> 1983-52 0.076
#> 1984-00 0.000
#> 1984-01 0.060
#> 1984-02 0.060
#> 1984-03 0.119
#> 1984-04 0.077
#> 1984-05 0.131
#> 1984-06 0.131
#> 1984-07 0.089
#> 1984-08 0.083
#> 1984-09 0.113
#> 1984-10 0.083
#> 1984-11 0.149
#> 1984-12 0.107
#> 1984-13 0.125
#> 1984-14 0.095
#> 1984-15 0.101
#> 1984-16 0.071
#> 1984-17 0.077
#> 1984-18 0.149
#> 1984-19 0.137
#> 1984-20 0.119
#> 1984-21 0.143
#> 1984-22 0.107
#> 1984-23 0.101
#> 1984-24 0.119
#> 1984-25 0.101
#> 1984-26 0.083
#> 1984-27 0.125
#> 1984-28 0.071
#> 1984-29 0.125
#> 1984-30 0.077
#> 1984-31 0.071
#> 1984-32 0.083
#> 1984-33 0.107
#> 1984-34 0.089
#> 1984-35 0.095
#> 1984-36 0.095
#> 1984-37 0.054
#> 1984-38 0.077
#> 1984-39 0.102
#> 1984-40 0.125
#> 1984-41 0.084
#> 1984-42 0.101
#> 1984-43 0.060
#> 1984-44 0.083
#> 1984-45 0.054
#> 1984-46 0.101
#> 1984-47 0.774
#> 1984-48 1.000
#> 1984-49 1.000
#> 1984-50 1.000
#> 1984-51 0.196
#> 1984-52 0.071
#> 1984-53 0.083
#> 1985-00 0.090
#> 1985-01 0.083
#> 1985-02 0.060
#> 1985-03 0.107
#> 1985-04 0.095
#> 1985-05 0.077
#> 1985-06 0.131
#> 1985-07 0.113
#> 1985-08 0.143
#> 1985-09 0.113
#> 1985-10 0.113
#> 1985-11 0.054
#> 1985-12 0.101
#> 1985-13 0.060
#> 1985-14 0.083
#> 1985-15 0.107
#> 1985-16 0.083
#> 1985-17 0.089
#> 1985-18 0.101
#> 1985-19 0.137
#> 1985-20 0.071
#> 1985-21 0.071
#> 1985-22 0.101
#> 1985-23 0.119
#> 1985-24 0.071
#> 1985-25 0.107
#> 1985-26 0.107
#> 1985-27 0.095
#> 1985-28 0.071
#> 1985-29 0.119
#> 1985-30 0.077
#> 1985-31 0.095
#> 1985-32 0.119
#> 1985-33 0.119
#> 1985-34 0.065
#> 1985-35 0.083
#> 1985-36 0.113
#> 1985-37 0.095
#> 1985-38 0.066
#> 1985-39 0.060
#> 1985-40 0.102
#> 1985-41 0.137
#> 1985-42 0.089
#> 1985-43 0.107
#> 1985-44 0.155
#> 1985-45 0.125
#> 1985-46 0.089
#> 1985-47 0.113
#> 1985-48 0.089
#> 1985-49 0.143
#> 1985-50 0.131
#> 1985-51 0.131
#> 1985-52 0.190

######################
## Ex2: Computation of WEEKLY values from HOURLY ts, removing any missing value in 'x'
#       Loading the HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to weekly ts
subdaily2weekly(x, FUN=mean, na.rm=TRUE)
#> 1979-53 1980-00 1980-01 1980-02 1980-03 1980-04 1980-05 1980-06 1980-07 1980-08 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-09 1980-10 1980-11 1980-12 1980-13 1980-14 1980-15 1980-16 1980-17 1980-18 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-19 1980-20 1980-21 1980-22 1980-23 1980-24 1980-25 1980-26 1980-27 1980-28 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-29 1980-30 1980-31 1980-32 1980-33 1980-34 1980-35 1980-36 1980-37 1980-38 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-39 1980-40 1980-41 1980-42 1980-43 1980-44 1980-45 1980-46 1980-47 1980-48 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1980-49 1980-50 1980-51 1980-52 1981-00 1981-01 1981-02 1981-03 1981-04 1981-05 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-06 1981-07 1981-08 1981-09 1981-10 1981-11 1981-12 1981-13 1981-14 1981-15 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-16 1981-17 1981-18 1981-19 1981-20 1981-21 1981-22 1981-23 1981-24 1981-25 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-26 1981-27 1981-28 1981-29 1981-30 1981-31 1981-32 1981-33 1981-34 1981-35 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-36 1981-37 1981-38 1981-39 1981-40 1981-41 1981-42 1981-43 1981-44 1981-45 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1981-46 1981-47 1981-48 1981-49 1981-50 1981-51 1981-52 1982-00 1982-01 1982-02 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-03 1982-04 1982-05 1982-06 1982-07 1982-08 1982-09 1982-10 1982-11 1982-12 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-13 1982-14 1982-15 1982-16 1982-17 1982-18 1982-19 1982-20 1982-21 1982-22 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-23 1982-24 1982-25 1982-26 1982-27 1982-28 1982-29 1982-30 1982-31 1982-32 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-33 1982-34 1982-35 1982-36 1982-37 1982-38 1982-39 1982-40 1982-41 1982-42 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1982-43 1982-44 1982-45 1982-46 1982-47 1982-48 1982-49 1982-50 1982-51 1982-52 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-00 1983-01 1983-02 1983-03 1983-04 1983-05 1983-06 1983-07 1983-08 1983-09 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-10 1983-11 1983-12 1983-13 1983-14 1983-15 1983-16 1983-17 1983-18 1983-19 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-20 1983-21 1983-22 1983-23 1983-24 1983-25 1983-26 1983-27 1983-28 1983-29 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-30 1983-31 1983-32 1983-33 1983-34 1983-35 1983-36 1983-37 1983-38 1983-39 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-40 1983-41 1983-42 1983-43 1983-44 1983-45 1983-46 1983-47 1983-48 1983-49 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1983-50 1983-51 1983-52 1984-00 1984-01 1984-02 1984-03 1984-04 1984-05 1984-06 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-07 1984-08 1984-09 1984-10 1984-11 1984-12 1984-13 1984-14 1984-15 1984-16 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-17 1984-18 1984-19 1984-20 1984-21 1984-22 1984-23 1984-24 1984-25 1984-26 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-27 1984-28 1984-29 1984-30 1984-31 1984-32 1984-33 1984-34 1984-35 1984-36 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-37 1984-38 1984-39 1984-40 1984-41 1984-42 1984-43 1984-44 1984-45 1984-46 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1984-47 1984-48 1984-49 1984-50 1984-51 1984-52 1984-53 1985-00 1985-01 1985-02 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-03 1985-04 1985-05 1985-06 1985-07 1985-08 1985-09 1985-10 1985-11 1985-12 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-13 1985-14 1985-15 1985-16 1985-17 1985-18 1985-19 1985-20 1985-21 1985-22 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-23 1985-24 1985-25 1985-26 1985-27 1985-28 1985-29 1985-30 1985-31 1985-32 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-33 1985-34 1985-35 1985-36 1985-37 1985-38 1985-39 1985-40 1985-41 1985-42 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
#> 1985-43 1985-44 1985-45 1985-46 1985-47 1985-48 1985-49 1985-50 1985-51 1985-52 
#>      NA      NA      NA      NA      NA      NA      NA      NA      NA      NA 
```
