# Sub-hourly -\> n-minutes

Generic function for aggregating a sub-hourly time series into a
"n-minutes' one.

## Usage

``` r
subhourly2nminutes(x, ...)

# Default S3 method
subhourly2nminutes(x, nminutes, FUN, na.rm=TRUE, 
                                     from=start(x), to=end(x), ...)

# S3 method for class 'zoo'
subhourly2nminutes(x, nminutes, FUN, na.rm=TRUE, 
                                 from=start(x), to=end(x), tz, ...)

# S3 method for class 'data.frame'
subhourly2nminutes(x, nminutes, FUN, na.rm=TRUE, 
        from=start(x), to=end(x), dates=1, date.fmt="%Y-%m-%d %H:%M:%S", 
        out.fmt="zoo", verbose= TRUE, ...)

# S3 method for class 'matrix'
subhourly2nminutes(x, nminutes, FUN, na.rm=TRUE, 
        from=start(x), to=end(x), dates=1, date.fmt="%Y-%m-%d %H:%M:%S", 
        out.fmt="zoo", verbose= TRUE, ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with sub-hourly time series.  
  Measurements at several gauging stations can be stored in a data.frame
  or matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` represent the ID of each station.

- nminutes:

  numeric, defining the amount of minutes to be used for aggregating
  `x`. `nminutes` must be larger than the amount of minutes between each
  `x` value (computed as `time(x)[2]-time(x)[1]`).

- FUN:

  Function that have to be applied for aggregating from sub-hourly into
  n-minutes time step. (e.g., for precipitation `FUN=sum` and for
  temperature and streamflow ts, `FUN=mean`).  

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the hourly values are computed considering only those values
  different from NA  
  -) FALSE: if there is AT LEAST one NA sub-hourly value within a day,
  the corresponding hourly value(s) will be NA as well

- from:

  POSIX object indicating the starting time used to carry out the
  temporal aggregation.  
  When `from > start(x)` then `x` is cut in time to the starting
  DateTime defined by `from`.  
  When `from < start(x)` then `x` is extended backward with NAs to the
  starting DateTime defined by `from`.

- to:

  POSIX object indicating the ending time used to carry out the temporal
  aggregation.  
  When `to < end(x)` then `x` is cut in time to the ending DateTime
  defined by `to`.  
  When `to > end(x)` then `x` is extended forward with NAs to the ending
  DateTime defined by `from`.

- tz:

  character, with the specification of the time zone used for `x`,
  `from`, and `to`. System-specific (see time zones), but `""` is the
  current time zone, and `"GMT"` is UTC (Universal Time, Coordinated).
  See [`Sys.timezone`](https://rdrr.io/r/base/timezones.html) and
  [`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html).  

  If `tz` is missing (the default), it is automatically set to the time
  zone used in `time(x)`.  

  If `tz` is provided, it forces `time(x)` to be in the tome zone
  specified by `tz`, without modifying the the values (hours, minutes,
  seconds, etc).  

  A list of valid time zones can be obtained by calling the base
  function [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).  

  This argument can be used when working with sub-daily zoo objects to
  force using time zones other than the local time zone for `from` and
  `to`. It should be used with caution, being well aware of the time
  zone of the data. See examples.

- dates:

  numeric, factor, POSIXct or POSIXt object indicating how to obtain the
  dates and times for each column of `x` (e.g., gauging station)  
  If `dates` is a number, it indicates the index of the column in `x`
  that stores the date and times  
  If `dates` is a factor, it is converted into POSIXct class, using the
  date format specified by `date.fmt`  
  If `dates` is already of POSIXct or POSIXt class, the code verifies
  that the number of elements on it be equal to the number of elements
  in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, By default `date.fmt=%Y-%m-%d %H:%M:%S`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.fmt:

  OPTIONAL. Only used when `x` is a matrix or data.frame object /cr
  character, for selecting if the result will be a matrix/data.frame or
  a zoo object. Valid values are: numeric, zoo (default)

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  further arguments passed to or from other methods.

## Value

a zoo object with hourly time series

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md),
[`subdaily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
[`subdaily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`subdaily2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html),
[`dm2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md),
[`seasonalfunction`](https://hzambran.github.io/hydroTSM/reference/seasonalfunction.md),
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md)

## Examples

``` r
## Creating a 5-min time sequence and counting its length
dt  <- seq( from=as.POSIXct("2021-06-30 00:00"), to=as.POSIXct("2021-06-30 23:55"), by="5 min" )
ndt <- length(dt)

## Creating a dummy 5-min zoo object, with 1 as the only value in each time period
x <- zoo( rep(1, ndt), dt)


## Aggregation from 5-minute single ts into 10-minute ts
h1 <- subhourly2nminutes(x, nminutes= 10, FUN=sum, na.rm=TRUE)

## Aggregation of 3 ts with 5-minute time frequency (i.e., a zoo matrix) 
## into a 30-minute zoo object.
X  <- cbind(x, x, x)
h2 <- subhourly2nminutes(X, nminutes= 30, FUN=sum, na.rm=TRUE)
```
