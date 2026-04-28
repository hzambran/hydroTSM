# Irregular Zoo -\> Regular Zoo

It takes an irregular zoo object (with non-existing values for some
dates/times) and converts it into a regularly spaced zoo object within
the time period defined by `from` and `to`, by filling the missing dates
with ‘NA’

## Usage

``` r
izoo2rzoo(x, ...)

# Default S3 method
izoo2rzoo(x, from= start(x), to= end(x), 
                   date.fmt, tstep, tz, 
                   na.action=c("keep", "linear", "spline"), ... )
     
# S3 method for class 'zoo'
izoo2rzoo(x, from= start(x), to= end(x), 
                   date.fmt, tstep, tz, 
                   na.action=c("keep", "linear", "spline"), ... )
```

## Arguments

- x:

  irregular zoo object (vector or matrix) representing a time series
  (very likely read with some user-defined procedure, and with some
  missing values for particular days/months/years)

- from:

  Character indicating the starting date for creating the regularly
  spaced zoo object. The default value corresponds to the date of the
  first element of `x`  
  It has to be in the format indicated by `date.fmt`.

- to:

  Character indicating the ending date for creating the regularly spaced
  zoo object. The default value corresponds to the date of the last
  element of `x`  
  It has to be in the format indicated by `date.fmt`.

- date.fmt:

  character indicating the format in which the dates are stored in
  `from` and `to`, e.g. %Y-%m-%d. See ‘Details’ section in
  [`strptime`](https://rdrr.io/r/base/strptime.html). By default,
  `date.fmt` is missing, and it is automatically set to %Y-%m-%d when
  `time(x)` is `Date` object, and set to %Y-%m-%d %H:%M:%S when `x` is a
  sub-daily zoo object.

- tstep:

  character, indicating the time step used for creating the time
  sequence going from `from` to `to` that will be used as `time(x)`  
  Valid values are (but not limited to) hours, days, months, years. By
  default, `tstep` is missing, and it is automatically set to "minutes"
  when `sfreq(x)` is min, to "hours" when `sfreq(x)` is hourly, to
  "days" when `sfreq(x)` is daily, to "weeks" when `sfreq(x)` is weekly,
  to "months" when `sfreq(x)` is monthly, to "quarters" when `sfreq(x)`
  is quarterly, and to "years" when `sfreq(x)` is annual.

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

- na.action:

  character indicating whether to keep 'NA' values in `x` (default) or
  interpolate them with linear or spline interpolation using the
  [`na.approx`](https://rdrr.io/pkg/zoo/man/na.approx.html) or the
  [`na.spline`](https://rdrr.io/pkg/zoo/man/na.approx.html) function of
  the zoo package.

- ...:

  further arguments passed to or from other methods

## Details

If the full time period of `x` is a subset of the time period defined by
`from` and `to`, the time period of the resulting zoo is the one defined
by `from` and `to`, assigning 'NA' to all the dates in which `x` does
not have a value.

## Value

a regularly spaced zoo object, with values given by `x` and time stamps
going from `from` to `to` at intervals defined by `tsteps`.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`zoo`](https://rdrr.io/pkg/zoo/man/zoo.html),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html),
[`Sys.timezone`](https://rdrr.io/r/base/timezones.html)

## Examples

``` r
##
## Example 1: Adding NA for February 29th to an existing zoo object

# dummy values and dates (February 29th is not present !)
x <- 1:9
dates <- c("1964-02-25", "1964-02-26", "1964-02-27", "1964-02-28", "1964-03-01", 
           "1964-03-02", "1964-03-03", "1964-03-04", "1964-03-05")

# From 'character' to 'Date' class
dates <- as.Date(dates)

## From 'numeric' to 'zoo' class
( x <- zoo(x, dates) ) # Feb 29th is still not present in 'x'
#> 1964-02-25 1964-02-26 1964-02-27 1964-02-28 1964-03-01 1964-03-02 1964-03-03 
#>          1          2          3          4          5          6          7 
#> 1964-03-04 1964-03-05 
#>          8          9 
## checking the length of 'x'
length(x) # 9 elements (there is no data for Feb 29th)
#> [1] 9

## Adding a missing value (NA in this case) for Feb 29th
( y <- izoo2rzoo(x) )
#> 1964-02-25 1964-02-26 1964-02-27 1964-02-28 1964-02-29 1964-03-01 1964-03-02 
#>          1          2          3          4         NA          5          6 
#> 1964-03-03 1964-03-04 1964-03-05 
#>          7          8          9 

## checking the new length
length(y) # 1 element more than the original 'x' (thre is an NA value in Feb 29th)
#> [1] 10


##
## Example 2: Extending the original 'x' object from February 1st to the end of March, 
#             assigning 'NA' to the days in which 'x' do not have a value.
( y <- izoo2rzoo(x, from="1964-02-01", to="1964-03-31") )
#> 1964-02-01 1964-02-02 1964-02-03 1964-02-04 1964-02-05 1964-02-06 1964-02-07 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-02-08 1964-02-09 1964-02-10 1964-02-11 1964-02-12 1964-02-13 1964-02-14 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-02-15 1964-02-16 1964-02-17 1964-02-18 1964-02-19 1964-02-20 1964-02-21 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-02-22 1964-02-23 1964-02-24 1964-02-25 1964-02-26 1964-02-27 1964-02-28 
#>         NA         NA         NA          1          2          3          4 
#> 1964-02-29 1964-03-01 1964-03-02 1964-03-03 1964-03-04 1964-03-05 1964-03-06 
#>         NA          5          6          7          8          9         NA 
#> 1964-03-07 1964-03-08 1964-03-09 1964-03-10 1964-03-11 1964-03-12 1964-03-13 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-03-14 1964-03-15 1964-03-16 1964-03-17 1964-03-18 1964-03-19 1964-03-20 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-03-21 1964-03-22 1964-03-23 1964-03-24 1964-03-25 1964-03-26 1964-03-27 
#>         NA         NA         NA         NA         NA         NA         NA 
#> 1964-03-28 1964-03-29 1964-03-30 1964-03-31 
#>         NA         NA         NA         NA 


##
## Example 3: Working with a zoo matrix with two identical 'x' time series, 
##            from 1964-02-25 to 1964-03-05
( Y <- cbind(x,x) )
#>            x x
#> 1964-02-25 1 1
#> 1964-02-26 2 2
#> 1964-02-27 3 3
#> 1964-02-28 4 4
#> 1964-03-01 5 5
#> 1964-03-02 6 6
#> 1964-03-03 7 7
#> 1964-03-04 8 8
#> 1964-03-05 9 9

# Adding a missing value (NA in this case) for Feb 29th in all the columns of Y
( rY <- izoo2rzoo(Y) )
#>             x  x
#> 1964-02-25  1  1
#> 1964-02-26  2  2
#> 1964-02-27  3  3
#> 1964-02-28  4  4
#> 1964-02-29 NA NA
#> 1964-03-01  5  5
#> 1964-03-02  6  6
#> 1964-03-03  7  7
#> 1964-03-04  8  8
#> 1964-03-05  9  9


##
## Example 4: Working with hourly data, from 01:00 to 10:00 UTC on 12th December 2000
dates  <- ISOdatetime(year=2000, month=12, day=12, hour=1:10, min=0, sec=0, tz="UTC")
values <- 1:10
x      <- zoo(values, dates)

# removing four values in 'x', from 02:00 to 05:00, i.e., they will not be present 
# anymore in 'x' at all, not even NA !)
x <- x[-c(2:5)]
time(x)
#> [1] "2000-12-12 01:00:00 UTC" "2000-12-12 06:00:00 UTC"
#> [3] "2000-12-12 07:00:00 UTC" "2000-12-12 08:00:00 UTC"
#> [5] "2000-12-12 09:00:00 UTC" "2000-12-12 10:00:00 UTC"
length(x)
#> [1] 6

# Adding missing values (NA in this case) from 02:00 to 05:00
y <-  izoo2rzoo(x)
time(y)
#>  [1] "2000-12-12 01:00:00 UTC" "2000-12-12 02:00:00 UTC"
#>  [3] "2000-12-12 03:00:00 UTC" "2000-12-12 04:00:00 UTC"
#>  [5] "2000-12-12 05:00:00 UTC" "2000-12-12 06:00:00 UTC"
#>  [7] "2000-12-12 07:00:00 UTC" "2000-12-12 08:00:00 UTC"
#>  [9] "2000-12-12 09:00:00 UTC" "2000-12-12 10:00:00 UTC"
length(y)
#> [1] 10


##
## Example 5: Extending hourly data to a DateTime before 'start(x)', 
##            specifying only the date.
##            Time of 'x' is in local time zone (tz="") instead of UTC
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00", tz="")
x  <- zoo(0:20, dt)
(y  <- izoo2rzoo(x, from="2020-12-31") )# 00:00:00 is ommited
#> 2020-12-31 00:00:00 2020-12-31 01:00:00 2020-12-31 02:00:00 2020-12-31 03:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2020-12-31 04:00:00 2020-12-31 05:00:00 2020-12-31 06:00:00 2020-12-31 07:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2020-12-31 08:00:00 2020-12-31 09:00:00 2020-12-31 10:00:00 2020-12-31 11:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2020-12-31 12:00:00 2020-12-31 13:00:00 2020-12-31 14:00:00 2020-12-31 15:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2020-12-31 16:00:00 2020-12-31 17:00:00 2020-12-31 18:00:00 2020-12-31 19:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2020-12-31 20:00:00 2020-12-31 21:00:00 2020-12-31 22:00:00 2020-12-31 23:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-01 00:00:00 2021-01-01 01:00:00 2021-01-01 02:00:00 2021-01-01 03:00:00 
#>                   0                   1                   2                   3 
#> 2021-01-01 04:00:00 2021-01-01 05:00:00 2021-01-01 06:00:00 2021-01-01 07:00:00 
#>                   4                   5                   6                   7 
#> 2021-01-01 08:00:00 2021-01-01 09:00:00 2021-01-01 10:00:00 2021-01-01 11:00:00 
#>                   8                   9                  10                  11 
#> 2021-01-01 12:00:00 2021-01-01 13:00:00 2021-01-01 14:00:00 2021-01-01 15:00:00 
#>                  12                  13                  14                  15 
#> 2021-01-01 16:00:00 2021-01-01 17:00:00 2021-01-01 18:00:00 2021-01-01 19:00:00 
#>                  16                  17                  18                  19 
#> 2021-01-01 20:00:00 
#>                  20 
(time(y))
#>  [1] "2020-12-31 00:00:00 UTC" "2020-12-31 01:00:00 UTC"
#>  [3] "2020-12-31 02:00:00 UTC" "2020-12-31 03:00:00 UTC"
#>  [5] "2020-12-31 04:00:00 UTC" "2020-12-31 05:00:00 UTC"
#>  [7] "2020-12-31 06:00:00 UTC" "2020-12-31 07:00:00 UTC"
#>  [9] "2020-12-31 08:00:00 UTC" "2020-12-31 09:00:00 UTC"
#> [11] "2020-12-31 10:00:00 UTC" "2020-12-31 11:00:00 UTC"
#> [13] "2020-12-31 12:00:00 UTC" "2020-12-31 13:00:00 UTC"
#> [15] "2020-12-31 14:00:00 UTC" "2020-12-31 15:00:00 UTC"
#> [17] "2020-12-31 16:00:00 UTC" "2020-12-31 17:00:00 UTC"
#> [19] "2020-12-31 18:00:00 UTC" "2020-12-31 19:00:00 UTC"
#> [21] "2020-12-31 20:00:00 UTC" "2020-12-31 21:00:00 UTC"
#> [23] "2020-12-31 22:00:00 UTC" "2020-12-31 23:00:00 UTC"
#> [25] "2021-01-01 00:00:00 UTC" "2021-01-01 01:00:00 UTC"
#> [27] "2021-01-01 02:00:00 UTC" "2021-01-01 03:00:00 UTC"
#> [29] "2021-01-01 04:00:00 UTC" "2021-01-01 05:00:00 UTC"
#> [31] "2021-01-01 06:00:00 UTC" "2021-01-01 07:00:00 UTC"
#> [33] "2021-01-01 08:00:00 UTC" "2021-01-01 09:00:00 UTC"
#> [35] "2021-01-01 10:00:00 UTC" "2021-01-01 11:00:00 UTC"
#> [37] "2021-01-01 12:00:00 UTC" "2021-01-01 13:00:00 UTC"
#> [39] "2021-01-01 14:00:00 UTC" "2021-01-01 15:00:00 UTC"
#> [41] "2021-01-01 16:00:00 UTC" "2021-01-01 17:00:00 UTC"
#> [43] "2021-01-01 18:00:00 UTC" "2021-01-01 19:00:00 UTC"
#> [45] "2021-01-01 20:00:00 UTC"


##
## Example 6: Extending hourly data to a DateTime before 'start(x)', 
##            specifying date and time.
##            Time of 'x' is in local time zone (tz="") instead of UTC
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00", tz="")
x  <- zoo(0:20, dt)
( y  <- izoo2rzoo(x, from="2020-12-31 20:00:00") )
#> 2020-12-31 20:00:00 2020-12-31 21:00:00 2020-12-31 22:00:00 2020-12-31 23:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-01 00:00:00 2021-01-01 01:00:00 2021-01-01 02:00:00 2021-01-01 03:00:00 
#>                   0                   1                   2                   3 
#> 2021-01-01 04:00:00 2021-01-01 05:00:00 2021-01-01 06:00:00 2021-01-01 07:00:00 
#>                   4                   5                   6                   7 
#> 2021-01-01 08:00:00 2021-01-01 09:00:00 2021-01-01 10:00:00 2021-01-01 11:00:00 
#>                   8                   9                  10                  11 
#> 2021-01-01 12:00:00 2021-01-01 13:00:00 2021-01-01 14:00:00 2021-01-01 15:00:00 
#>                  12                  13                  14                  15 
#> 2021-01-01 16:00:00 2021-01-01 17:00:00 2021-01-01 18:00:00 2021-01-01 19:00:00 
#>                  16                  17                  18                  19 
#> 2021-01-01 20:00:00 
#>                  20 


##
## Example 7: Extending hourly data to a DateTime before 'start(x)', 
##            specifying date and time, and forcing UTC to be the time zone.
##            Time of 'x' is in local time zone (tz="") instead of UTC, but
##            it will be treated as UTC by using the 'tz' argument
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00", tz="")
x  <- zoo(0:20, dt)
(time(x))
#>  [1] "2021-01-01 00:00:00 UTC" "2021-01-01 01:00:00 UTC"
#>  [3] "2021-01-01 02:00:00 UTC" "2021-01-01 03:00:00 UTC"
#>  [5] "2021-01-01 04:00:00 UTC" "2021-01-01 05:00:00 UTC"
#>  [7] "2021-01-01 06:00:00 UTC" "2021-01-01 07:00:00 UTC"
#>  [9] "2021-01-01 08:00:00 UTC" "2021-01-01 09:00:00 UTC"
#> [11] "2021-01-01 10:00:00 UTC" "2021-01-01 11:00:00 UTC"
#> [13] "2021-01-01 12:00:00 UTC" "2021-01-01 13:00:00 UTC"
#> [15] "2021-01-01 14:00:00 UTC" "2021-01-01 15:00:00 UTC"
#> [17] "2021-01-01 16:00:00 UTC" "2021-01-01 17:00:00 UTC"
#> [19] "2021-01-01 18:00:00 UTC" "2021-01-01 19:00:00 UTC"
#> [21] "2021-01-01 20:00:00 UTC"
(y  <- izoo2rzoo(x, from="2020-12-31 20:00:00", tz="UTC") )# 00:00:00 is ommited
#> 2020-12-31 20:00:00 2020-12-31 21:00:00 2020-12-31 22:00:00 2020-12-31 23:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-01 00:00:00 2021-01-01 01:00:00 2021-01-01 02:00:00 2021-01-01 03:00:00 
#>                   0                   1                   2                   3 
#> 2021-01-01 04:00:00 2021-01-01 05:00:00 2021-01-01 06:00:00 2021-01-01 07:00:00 
#>                   4                   5                   6                   7 
#> 2021-01-01 08:00:00 2021-01-01 09:00:00 2021-01-01 10:00:00 2021-01-01 11:00:00 
#>                   8                   9                  10                  11 
#> 2021-01-01 12:00:00 2021-01-01 13:00:00 2021-01-01 14:00:00 2021-01-01 15:00:00 
#>                  12                  13                  14                  15 
#> 2021-01-01 16:00:00 2021-01-01 17:00:00 2021-01-01 18:00:00 2021-01-01 19:00:00 
#>                  16                  17                  18                  19 
#> 2021-01-01 20:00:00 
#>                  20 
(time(y))
#>  [1] "2020-12-31 20:00:00 UTC" "2020-12-31 21:00:00 UTC"
#>  [3] "2020-12-31 22:00:00 UTC" "2020-12-31 23:00:00 UTC"
#>  [5] "2021-01-01 00:00:00 UTC" "2021-01-01 01:00:00 UTC"
#>  [7] "2021-01-01 02:00:00 UTC" "2021-01-01 03:00:00 UTC"
#>  [9] "2021-01-01 04:00:00 UTC" "2021-01-01 05:00:00 UTC"
#> [11] "2021-01-01 06:00:00 UTC" "2021-01-01 07:00:00 UTC"
#> [13] "2021-01-01 08:00:00 UTC" "2021-01-01 09:00:00 UTC"
#> [15] "2021-01-01 10:00:00 UTC" "2021-01-01 11:00:00 UTC"
#> [17] "2021-01-01 12:00:00 UTC" "2021-01-01 13:00:00 UTC"
#> [19] "2021-01-01 14:00:00 UTC" "2021-01-01 15:00:00 UTC"
#> [21] "2021-01-01 16:00:00 UTC" "2021-01-01 17:00:00 UTC"
#> [23] "2021-01-01 18:00:00 UTC" "2021-01-01 19:00:00 UTC"
#> [25] "2021-01-01 20:00:00 UTC"

##
## Example 8: Extending hourly data to a DateTime after 'end(x)', 
##            specifying date and time.
##            Time of 'x' is in local time zone (tz="") instead of UTC
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00", tz="")
x  <- zoo(0:20, dt)
( y  <- izoo2rzoo(x, to="2021-01-02 12:00:00") )
#> 2021-01-01 00:00:00 2021-01-01 01:00:00 2021-01-01 02:00:00 2021-01-01 03:00:00 
#>                   0                   1                   2                   3 
#> 2021-01-01 04:00:00 2021-01-01 05:00:00 2021-01-01 06:00:00 2021-01-01 07:00:00 
#>                   4                   5                   6                   7 
#> 2021-01-01 08:00:00 2021-01-01 09:00:00 2021-01-01 10:00:00 2021-01-01 11:00:00 
#>                   8                   9                  10                  11 
#> 2021-01-01 12:00:00 2021-01-01 13:00:00 2021-01-01 14:00:00 2021-01-01 15:00:00 
#>                  12                  13                  14                  15 
#> 2021-01-01 16:00:00 2021-01-01 17:00:00 2021-01-01 18:00:00 2021-01-01 19:00:00 
#>                  16                  17                  18                  19 
#> 2021-01-01 20:00:00 2021-01-01 21:00:00 2021-01-01 22:00:00 2021-01-01 23:00:00 
#>                  20                  NA                  NA                  NA 
#> 2021-01-02 00:00:00 2021-01-02 01:00:00 2021-01-02 02:00:00 2021-01-02 03:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-02 04:00:00 2021-01-02 05:00:00 2021-01-02 06:00:00 2021-01-02 07:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-02 08:00:00 2021-01-02 09:00:00 2021-01-02 10:00:00 2021-01-02 11:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-02 12:00:00 
#>                  NA 


##
## Example 9: Extending hourly data to a DateTime before 'start(x)'.
##            Note that the 'tz' argument can be ommited in the 'hip' function, 
##            because by default it assumes UTC as time zone
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00", tz="UTC")
x  <- zoo(0:20, dt)
( y  <- izoo2rzoo(x, from="2020-12-31 20:00:00", tz="UTC") )
#> 2020-12-31 20:00:00 2020-12-31 21:00:00 2020-12-31 22:00:00 2020-12-31 23:00:00 
#>                  NA                  NA                  NA                  NA 
#> 2021-01-01 00:00:00 2021-01-01 01:00:00 2021-01-01 02:00:00 2021-01-01 03:00:00 
#>                   0                   1                   2                   3 
#> 2021-01-01 04:00:00 2021-01-01 05:00:00 2021-01-01 06:00:00 2021-01-01 07:00:00 
#>                   4                   5                   6                   7 
#> 2021-01-01 08:00:00 2021-01-01 09:00:00 2021-01-01 10:00:00 2021-01-01 11:00:00 
#>                   8                   9                  10                  11 
#> 2021-01-01 12:00:00 2021-01-01 13:00:00 2021-01-01 14:00:00 2021-01-01 15:00:00 
#>                  12                  13                  14                  15 
#> 2021-01-01 16:00:00 2021-01-01 17:00:00 2021-01-01 18:00:00 2021-01-01 19:00:00 
#>                  16                  17                  18                  19 
#> 2021-01-01 20:00:00 
#>                  20 


##
## Example 10: Extending hourly data to a date before 'start(x)'. However, hourly 'x'
##            values are given at HH:15:00 hours instead of HH:00:00 hours.

## Loading the time series of hourly streamflows for the station Karamea at Gorge
## Time Zone for 'KarameaAtGorgeQts' data is 'UTC' (see ?KarameaAtGorgeQts), but it will
## be tr4ated as 'Pacific/Auckland' (Zealand Standard Time) for this example
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Subsetting 'x' to its first day only
# (01/Jan/1980 08:15:00 - 01/Jan/1980 23:15:00)
x <- window(x, end="1980-01-01 23:59:00")

# Adding NA hourly data since 1979-12-31 21:15:00
izoo2rzoo(x, from="1979-12-31 21:15:00", tz="Pacific/Auckland")
#>                         
#> 1979-12-31 21:15:00 72.6
#> 1979-12-31 22:15:00 72.4
#> 1979-12-31 23:15:00 71.8
#> 1980-01-01 00:15:00 71.6
#> 1980-01-01 01:15:00 71.2
#> 1980-01-01 02:15:00 71.2
#> 1980-01-01 03:15:00 71.0
#> 1980-01-01 04:15:00 70.4
#> 1980-01-01 05:15:00 70.3
#> 1980-01-01 06:15:00 70.2
#> 1980-01-01 07:15:00 70.5
#> 1980-01-01 08:15:00 70.4
#> 1980-01-01 09:15:00 70.6
#> 1980-01-01 10:15:00 70.4
#> 1980-01-01 11:15:00 70.3
#> 1980-01-01 12:15:00 69.9
#> 1980-01-01 13:15:00 69.6
#> 1980-01-01 14:15:00 69.1
#> 1980-01-01 15:15:00 68.9
#> 1980-01-01 16:15:00 69.1
#> 1980-01-01 17:15:00 69.4
#> 1980-01-01 18:15:00 70.0
#> 1980-01-01 19:15:00 71.4
#> 1980-01-01 20:15:00 73.0
#> 1980-01-01 21:15:00 73.9
#> 1980-01-01 22:15:00 75.3
#> 1980-01-01 23:15:00 75.7
```
