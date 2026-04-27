# (sub)Daily/Monthly -\> Annual

Generic function for transforming a (sub)DAILY/MONTHLY (weekly and
quarterly) regular time series into an ANNUAL one.

## Usage

``` r
daily2annual(x, ...)
subdaily2annual(x, ...)
monthly2annual(x, ...)

# Default S3 method
daily2annual(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("%Y", "%Y-%m-%d"), start.month=1, ...)

# S3 method for class 'zoo'
daily2annual(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("%Y", "%Y-%m-%d"), start.month=1, ...) 

# S3 method for class 'data.frame'
daily2annual(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("%Y", "%Y-%m-%d"), start.month=1, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", verbose = TRUE, ...)

# S3 method for class 'matrix'
daily2annual(x, FUN, na.rm = TRUE, na.rm.max=0, 
        out.fmt=c("%Y", "%Y-%m-%d"), start.month=1, 
        dates=1, date.fmt = "%Y-%m-%d", out.type = "data.frame", 
        verbose = TRUE,  ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with (sub)daily/monthly time
  series.  
  Measurements at several gauging stations can be stored in a data.frame
  or matrix object, and in that case, each column of `x` represents the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- FUN:

  Function that have to be applied for aggregating from
  (sub)daily/monthly into annual time step (e.g., for precipitation
  `FUN=sum` and for temperature and streamflows ts `FUN=mean`).  

  `FUN` MUST accept the `na.rm` argument, because `na.rm` is passed to
  `FUN`.  

  When `FUN=max` or `FUN=min` the date(time) where the maximum/minimum
  value actually occurs is returned in the output object, otherwise, a
  generic 1st of january for each year is returned.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the annual values are computed only for years with a
  percentage of missing values less than `na.rm.max`  
  -) FALSE: if there is AT LEAST one NA within a year, the corresponing
  annual values in the output object will be `NA`.

- na.rm.max:

  Numeric in \[0, 1\]. It is used to define the maximum percentage of
  missing values allowed in each year to keep the yearly aggregated
  value in the output object of this function. In other words, if the
  percentage of missing values in a given year is larger than
  `na.rm.max` the corresponding annual value will be `NA`.

- out.fmt:

  Character indicating the date format for the output zoo object. See
  `format` in [`as.Date`](https://rdrr.io/r/base/as.Date.html). Possible
  values are:  
  -) %Y : only the year will be used for the time. Default option.
  (e.g., "1961" "1962"...)  
  -) %Y-%m-%d: a complete date format will be used for the time. (e.g.,
  "1961-01-01" "1962-01-01"...). See Details.

- start.month:

  numeric in \[1,..,12\], representing the starting month (1:Jan, ...,
  12:Dec) to be used in the computation of annual values. By default
  `start.month=1`.

- dates:

  numeric, factor or Date object indicating how to obtain the dates for
  corresponding to each gauging station  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into Date class, using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days on it be equal to the number of element in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.type:

  Character that defines the desired type of output. Valid values are:  
  -) data.frame: a data.frame, with as many columns as stations are
  included in `x`, and row names indicating the Year  
  -) db : a data.frame, with 3 columns will be produced.  
  The first column (StationID) will store the ID of the station  
  The second column (Year) will store the year,  
  The third column (Value) will contain the annual value corresponding
  to the two previous columns.

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  arguments additional to `na.rm` passed to `FUN`.

## Value

When `FUN!=max` and `FUN!=min` the output is a zoo object with annual
time frequency, where the time attribute has the format defined in
`out.fmt`.  
When `FUN!=max` and `FUN!=min` and `out.fmt="%Y-%m-%d"` the time
attribute of the output zoo object will use the 1st of January of each
year to create a full Date object from the corresponding year of each
element of the output object (e.g., fi the year is 2022, the time
attribute will be 2022-01-01). The only exception occurrs when `FUN=max`
or `FUN=min`, where the time attribute of each element will correspond
to the actual date where the annual maximum/minimum occurs (which is
very useful for identifying the date of the annual maximum or the annual
minimum of a time series).  

When `FUN=max` or `FUN=min` and `x` is a single time series, the output
is a zoo object with annual time frequency, where the time attribute has
the same class than `time(x)`, and the date(time) value corresponds to
the date(time) where the maximum/minimum value actually occurs.  
When `FUN=max` or `FUN=min` and `x` has two or more time series, the
output is a list object where each element has an annual time frequency.
The time attribute of each list element has the same class than
`time(x)`, and the date(time) value of each list element corresponds to
the date(time) where the maximum/minimum value actually occurs.  

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`daily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
`monthly2annual`,
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`annualfunction`](https://hzambran.github.io/hydroTSM/reference/annualfunction.md),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`as.Date`](https://rdrr.io/r/base/as.Date.html)

## Examples

``` r
######################
## Ex1: Computation of annual values, removing any missing value in 'x'

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three months (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Agreggating from Daily to Annual, removing any missing value in 'x'
( a <- daily2annual(x, FUN=sum, na.rm=TRUE) )
#>  1921 
#> 163.2 

######################
## Ex2: Compuation of annual values only when the percentage of NAs in each
#       year is lower than a user-defined percentage (10% in this example).

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three months (Jan/1921 - Mar/1921)
x <- window(x, end="1921-12-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Daily to annual, only for months with less than 10% of missing values
( a2 <- daily2annual(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1) )
#>  1921 
#> 712.3 

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="annual")
#>  1921 
#> 0.099 


######################
## Ex3: Getting the annual maximum value, including the date where this annual 
##      maximum actually occurs
daily2annual(x, FUN=max)
#> 1921-01-31 
#>         48 


######################
## Ex4: Monthly to Annual, allowing a maximum of 20% of missing values in each month 
m <- daily2monthly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.2)
monthly2annual(m, FUN=sum, na.rm=TRUE)
#>  1921 
#> 710.3 


######################
## Ex5: Loading the time series of HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to monthly ts
subdaily2annual(x, FUN=mean, na.rm=TRUE)
#>      1979      1980      1981      1982      1983      1984      1985 
#>  72.26667 147.87130 119.98304 120.76738 137.22793 100.21928  90.03222 

############
## Ex6: Loading the monthly time series of precipitation within the Ebro River basin
data(EbroPPtsMonthly)

# computing the annual values for the first 10 gauging stations in 'EbroPPtsMonthly'
a <- monthly2annual(EbroPPtsMonthly[,1:11], FUN=sum, dates=1)
#> [Starting computations...]
#> Station: P9001      :   1/10 =>     10%
#> Station: P9008X     :   2/10 =>     20%
#> Station: P9012      :   3/10 =>     30%
#> Station: P9015      :   4/10 =>     40%
#> Station: P9019      :   5/10 =>     50%
#> Station: P9027      :   6/10 =>     60%
#> Station: P9034      :   7/10 =>     70%
#> Station: P9037      :   8/10 =>     80%
#> Station: P9041      :   9/10 =>     90%
#> Station: P9048      :  10/10 =>    100%

# same as before, but with a nicer format of years
a <- monthly2annual(EbroPPtsMonthly[,1:11], FUN=sum, dates=1, out.fmt="%Y")
#> [Starting computations...]
#> Station: P9001      :   1/10 =>     10%
#> Station: P9008X     :   2/10 =>     20%
#> Station: P9012      :   3/10 =>     30%
#> Station: P9015      :   4/10 =>     40%
#> Station: P9019      :   5/10 =>     50%
#> Station: P9027      :   6/10 =>     60%
#> Station: P9034      :   7/10 =>     70%
#> Station: P9037      :   8/10 =>     80%
#> Station: P9041      :   9/10 =>     90%
#> Station: P9048      :  10/10 =>    100%
```
