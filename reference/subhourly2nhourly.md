# Sub-hourly or hourly -\> n-hourly

Generic function for aggregating a sub-hourly or hourly time series into
a user-defined n-hourly one.

## Usage

``` r
subhourly2nhourly(x, ...)

# Default S3 method
subhourly2nhourly(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt="%H:%M:%S", tz, ...)

# S3 method for class 'zoo'
subhourly2nhourly(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt="%H:%M:%S", tz, ...)

# S3 method for class 'data.frame'
subhourly2nhourly(x, FUN, n.hours, na.rm=TRUE,
           na.rm.max=0, start="00:00:00", start.fmt="%H:%M:%S", tz,
           dates=1, date.fmt="%Y-%m-%d %H:%M:%S", out.fmt="zoo",
           verbose=TRUE, ...)

# S3 method for class 'matrix'
subhourly2nhourly(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt="%H:%M:%S", tz,
           dates=1, date.fmt="%Y-%m-%d %H:%M:%S", out.fmt="zoo",
           verbose=TRUE, ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with sub-hourly or hourly time
  series.  
  Measurements at several gauging stations can be stored in a data.frame
  or matrix object, and in that case, each column of `x` represents the
  time series measured in each gauging station, and the column names of
  `x` should correspond to the ID of each station.

- FUN:

  Function that have to be applied for aggregating from sub-hourly or
  hourly into n-hourly time step (e.g., for precipitation `FUN=sum` and
  for temperature and streamflows ts `FUN=mean`).  

  `FUN` MUST accept the `na.rm` argument, because `na.rm` is passed to
  `FUN`.

- n.hours:

  positive integer indicating the length, in hours, of each output
  aggregation period.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the n-hourly values are computed only for periods with a
  percentage of missing values less than or equal to `na.rm.max`  
  -) FALSE: if there is AT LEAST one NA within an n-hour period, the
  corresponding n-hourly value in the output object will be `NA`.

- na.rm.max:

  Numeric in \[0, 1\]. It is used to define the maximum percentage of
  missing values allowed in each n-hour period to keep the aggregated
  value in the output object of this function. In other words, if the
  percentage of missing values in a given n-hour period is larger than
  `na.rm.max`, the corresponding n-hourly value will be `NA`.

- start:

  character, indicating the starting time used for anchoring the n-hour
  aggregation periods. It MUST be provided in the format specified by
  `start.fmt`.  

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
  If `tz` is provided, it forces `time(x)` to be in the time zone
  specified by `tz`, without modifying the values (hours, minutes,
  seconds, etc).

- dates:

  numeric, factor, character, POSIXct or POSIXt object indicating how to
  obtain the dates and times for each column of `x` (e.g., gauging
  station)  
  If `dates` is a number, it indicates the index of the column in `x`
  that stores the date and times  
  If `dates` is a factor or character, it is converted into POSIXct
  class, using the date format specified by `date.fmt`  
  If `dates` is already of POSIXct or POSIXt class, the code verifies
  that the number of elements on it be equal to the number of elements
  in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, By default `date.fmt=%Y-%m-%d %H:%M:%S`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"`,
  `class(dates)=="character"` or `class(dates)=="numeric"`.

- out.fmt:

  OPTIONAL. Only used when `x` is a matrix or data.frame object /cr
  character, for selecting if the result will be a matrix/data.frame or
  a zoo object. Valid values are: numeric, zoo (default)

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  arguments additional to `na.rm` passed to `FUN`.

## Details

This function assumes that `x` has an hourly or sub-hourly temporal
frequency. The output time index corresponds to the beginning of each
n-hour aggregation period.

## Value

a zoo object with n-hourly time series

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`subhourly2nminutes`](https://hzambran.github.io/hydroTSM/reference/subhourly2nminutes.md),
[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md),
[`subdaily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
[`subdaily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`subdaily2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html),
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md)

## Examples

``` r
## Creating a 30-min time sequence and counting its length
dt  <- seq(from=as.POSIXct("2021-06-30 00:30", tz="UTC"),
           to=as.POSIXct("2021-06-30 23:30", tz="UTC"), by="30 min")
ndt <- length(dt)

## Creating a dummy 30-min zoo object, with 1 as the only value in each time period
x <- zoo(rep(1, ndt), dt)

## Aggregation from 30-minute single ts into 3-hourly ts
h1 <- subhourly2nhourly(x, FUN=sum, n.hours=3, na.rm=TRUE)

## Aggregation of 3 ts with 30-minute time frequency (i.e., a zoo matrix)
## into a 6-hourly zoo object.
X  <- cbind(x, x, x)
h2 <- subhourly2nhourly(X, FUN=sum, n.hours=6, na.rm=TRUE)

## Hourly input into 6-hourly output
h  <- subhourly2hourly(x, FUN=sum)
h3 <- subhourly2nhourly(h, FUN=sum, n.hours=6, na.rm=TRUE)
```
