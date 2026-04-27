# Is Complete?

Generic function for identifying whether a zoo object has a regular time
frequency without missing values from the first one to the last one.

## Usage

``` r
isComplete(x, ...)

# S3 method for class 'zoo'
isComplete(x, tz, out.type=c("single", "all"), verbose=TRUE, ...)
```

## Arguments

- x:

  zoo object with a time attribute that should have a regular time
  frequency (e.g., hourly, daily, monthly, annual).

- tz:

  character, indicating the time zone in which the date of `x` is
  located. System-specific (see time zones), but `""` is the current
  time zone, and `"GMT"` is UTC (Universal Time, Coordinated). See
  [`Sys.timezone`](https://rdrr.io/r/base/timezones.html) and
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

- out.type:

  Character indicating the type of result that is given by this
  function. Valid values are:  
  -) single =\> only a logical output is returned. See details in the
  Value section.  
  -) all =\> a list with 3 elements is returned. See details in the
  Value section.

- verbose:

  logical; if TRUE, informative messages are shown in the screen.

  When `verbose=TRUE` and `x` has some missing temporal layers, the
  DateTime(s) with missing layer(s) are shown in the screen.

- ...:

  further arguments passed to or from other methods.

## Value

- If `out.type="single"`:

  (default value), it returns a logical value identifying whether `x`
  has a regular time frequency with complete time layers from its first
  layer to the last one.

- If `out.type="all"`:

  it returns a list with the following three elements:

  -) isComplete: logical value identifying whether `x` has a regular
  time frequency with complete time layers from its first layer to the
  last one.

  -) NumberMissingDT: integer indicating the amount of missing layers
  from the first layer of `x` to the last one.

  -) missingDateTimes: Numeric, Date, or POSIXct vector showing the
  DateTime of the missing layers in `x`.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`window`](https://rdrr.io/r/stats/window.html)

## Examples

``` r
###########
# EXAMPLE 1: Hourly zoo object, with a POSIXct time attribute
###########

# Loading the HOURLY streamflow data for Karamea at Gorge
data(KarameaAtGorgeQts)
x.h <- KarameaAtGorgeQts

# Checking whether 'x' is complete or not
isComplete(x.h)
#> [ The exact time frequency of 'x' is: 1 hours
#> [ There are 13 missing date/times in 'x' (hourly) ]
#> [ Missing date/times are: 1980-09-27 09:15:00 ; 1980-10-12 00:15:00 ; 1981-09-26 09:15:00 ; 1981-10-11 00:15:00 ; 1982-09-25 09:15:00 ; 1982-10-10 00:15:00 ; 1983-09-24 09:15:00 ; 1983-10-09 00:15:00 ; 1984-09-29 09:15:00 ; 1984-10-14 00:15:00 ; 1985-09-28 09:15:00 ; 1985-10-13 00:15:00 ; 1985-12-30 20:15:00 ]
#> [1] FALSE

###########
## EXAMPLE 2: Daily object, with a Date time attribute
###########
     
# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x.d <- SanMartinoPPts

# Checking whether 'x' is complete or not
isComplete(x.d)
#> [ The exact time frequency of 'x' is: 1 days
#>                                                        
#> [ 'x' (daily) is complete from '1921-01-01' to '1990-12-31' (25567 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.d <-x.d[-c(3,5)]
isComplete(x.d)
#> [ The exact time frequency of 'x' is: 1 days
#> [ There are 2 missing date/times in 'x' (daily) ]
#> [ Missing date/times are: 1921-01-03 ; 1921-01-05 ]
#> [1] FALSE


###########
# EXAMPLE 3: Weekly zoo object, with a Date time attribute
###########

# Creating a dummy weekly object with a Date time attribute
x.w           <- x.d[1:12]
time(x.w) <-  seq(from=as.Date("2001-01-01"), to=as.Date("2001-03-19"), 
                  by="1 week")

# Checking whether 'x.w' is complete or not
isComplete(x.w)
#> [ The exact time frequency of 'x' is: 7 days
#>                                                        
#> [ 'x' (weekly) is complete from '2001-01-01' to '2001-03-19' (12 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.w2 <- x.w[-c(3,5)]
isComplete(x.w2)
#> [ The exact time frequency of 'x' is: 7 days
#> [ There are 2 missing date/times in 'x' (weekly) ]
#> [ Missing date/times are: 2001-01-15 ; 2001-01-29 ]
#> [1] FALSE


###########
# EXAMPLE 4: Monthly zoo object, with a yearmon time attribute
###########

# Transforming the daily object into a monthly one
x.m1 <- daily2monthly(x.d, FUN=sum)

# Checking whether 'x.m1' is complete or not
isComplete(x.m1)
#>                                                        
#> [ 'x' (monthly) is complete from '1921-01-01' to '1990-12-01' (840 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.m12 <- x.m1[-c(3,5)]
isComplete(x.m12)
#> [ There are 2 missing date/times in 'x' (monthly) ]
#> [ Missing date/times are: 1921-03-01 ; 1921-05-01 ]
#> [1] FALSE


###########
# EXAMPLE 5: Monthly zoo object, with a Date time attribute
###########

# Creating a monthly object with a Date time attribute
x.m2       <- x.m1
time(x.m2) <-  as.Date( paste0( format(zoo::as.yearmon( time(x.m1) ), "%Y-%m"), "-01") )

# Checking whether 'x.m2' is complete or not
isComplete(x.m2)
#>                                                        
#> [ 'x' (monthly) is complete from '1921-01-01' to '1990-12-01' (840 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.m22 <- x.m2[-c(3,5)]
isComplete(x.m22)
#> [ There are 2 missing date/times in 'x' (monthly) ]
#> [ Missing date/times are: 1921-03-01 ; 1921-05-01 ]
#> [1] FALSE


###########
# EXAMPLE 6: Annual zoo object, with a Date time attribute
###########

# Creating a dummy annual object with Date time attribute
x.a1       <- x.m1[1:12]
time(x.a1) <- hydroTSM::yip("2001-01-01", "2012-01-01")

# Checking whether 'x.a1' is complete or not
isComplete(x.a1)
#> [ The exact time frequency of 'x' is: 365 days
#>                                                        
#> [ 'x' (annual) is complete from '2001-01-01' to '2012-01-01' (12 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.a12 <- x.a1[-c(3,5)]
isComplete(x.a12)
#> [ The exact time frequency of 'x' is: 365 days
#> [ There are 2 missing date/times in 'x' (annual) ]
#> [ Missing date/times are: 2003-01-01 ; 2005-01-01 ]
#> [1] FALSE


###########
# EXAMPLE 7: 3-hourly zoo object, with a POSIXct time attribute
###########

# Creating a dummy 3-hourly object with a POSIXct time attribute
x.3h       <- x.m1[1:12]
time(x.3h) <- seq(from=as.POSIXct("2001-01-01 00:00:00", tz="UTC"), 
                  to=as.POSIXct("2001-01-02 09:00:00", tz="UTC"), 
                  by="3 hours")

# Checking whether 'x.3h' is complete or not
isComplete(x.3h)
#> [ The exact time frequency of 'x' is: 3 hours
#>                                                        
#> [ 'x' (3 hours) is complete from '2001-01-01' to '2001-01-02 09:00:00' (12 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.3h2 <- x.3h[-c(3,5)]
isComplete(x.3h2)
#> [ The exact time frequency of 'x' is: 3 hours
#> [ There are 2 missing date/times in 'x' (3 hours) ]
#> [ Missing date/times are: 2001-01-01 06:00:00 ; 2001-01-01 12:00:00 ]
#> [1] FALSE


############
# EXAMPLE 8: 8-day zoo object, with a Date time attribute
############

# Creating a dummy 8-day object with a Date time attribute
x.8d       <- x.m1[1:12]
time(x.8d) <- seq(from=as.Date("2001-01-01"), to=as.Date("2001-03-31"), by="8 days")

# Checking whether 'x.8d' is complete or not
isComplete(x.8d)
#> [ The exact time frequency of 'x' is: 8 days
#>                                                        
#> [ 'x' (8 days) is complete from '2001-01-01' to '2001-03-30' (12 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.8d2 <- x.8d[-c(3,5)]
isComplete(x.8d2)
#> [ The exact time frequency of 'x' is: 8 days
#> [ There are 2 missing date/times in 'x' (8 days) ]
#> [ Missing date/times are: 2001-01-17 ; 2001-02-02 ]
#> [1] FALSE


############
# EXAMPLE 9: 3-monthly zoo object, with a Date time attribute
############

# Creating a dummy 3-monthly object with a Date time attribute
x.3m       <- x.m1[1:12]
time(x.3m) <- seq(from=as.Date("2001-01-01"), to=as.Date("2003-12-01"), by="3 months")

# Checking whether 'x.3m' is complete or not
isComplete(x.3m)
#> [ The exact time frequency of 'x' is: 91 days
#>                                                        
#> [ 'x' (quarterly) is complete from '2001-01-01' to '2003-10-01' (12 elements) ! ]
#> [1] TRUE

# Removing two values and testing it again
x.3m2 <- x.3m[-c(3,5)]
isComplete(x.3m2)
#> [ The exact time frequency of 'x' is: 92 days
#> [ There are 2 missing date/times in 'x' (quarterly) ]
#> [ Missing date/times are: 2001-07-01 ; 2002-01-01 ]
#> [1] FALSE
```
