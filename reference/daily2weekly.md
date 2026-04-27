# Daily -\> Weekly

Generic function for transforming a DAILY (or sub-daily) regular time
series into a WEEKLY one

## Usage

``` r
daily2weekly(x, ...)

# Default S3 method
daily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

# S3 method for class 'zoo'
daily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

# S3 method for class 'data.frame'
daily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", out.fmt="numeric", 
        verbose=TRUE, ...)

# S3 method for class 'matrix'
daily2weekly(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", out.fmt="numeric", 
        verbose=TRUE, ...)
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

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.type:

  Character that defines the desired type of output. Valid values are:  
  -) data.frame: a data.frame, with as many columns as stations are
  included in `x`, and row names indicating the month and year for each
  value.  
  -) db : a data.frame, with 4 columns will be produced.  
  The first column (StationID) stores the ID of the station,  
  The second column (Year) stores the year  
  The third column (Month) stores the Month  
  The fourth column (Value) stores the numerical values corresponding to
  the values specified in the three previous columns.  

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
## Ex1: Computation of weekly values, removing any missing value in 'x'

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three weeks (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Agreggating from Daily to Weekly, removing any missing value in 'x'
w <- daily2weekly(x, FUN=sum, na.rm=TRUE)

######################
## Ex2: Computation of Weekly values only when the percentage of NAs in each
#       week is lower than a user-defined percentage (10% in this example).

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three weeks (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Daily to Weekly, only for weeks with less than 10% of missing values
w2 <- daily2weekly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1)

# Verifying that the weeks 01, 02, 06, 08, 10, 11, 12 of 'x' had 10% or more of missing values
cmv(x, tscale="weekly")
#> 1921-00 1921-01 1921-02 1921-03 1921-04 1921-05 1921-06 1921-07 1921-08 1921-09 
#>   0.000   0.143   0.143   0.000   0.000   0.000   0.143   0.000   0.286   0.000 
#> 1921-10 1921-11 1921-12 1921-13 
#>   0.143   0.286   0.143   0.000 

######################
## Ex3: Computation of Weekly values in a two-column zoo object, 
##      only when the percentage of NAs in each week is lower than a user-defined 
##      percentage (10% in this example).

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three weeks (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Creating a two-column zoo object
X <- cbind(x, y=x)

## Daily to Weekly, only for weeks with less than 10% of missing values
w2 <- daily2weekly(X, FUN=sum, na.rm=TRUE, na.rm.max=0.1)

# Verifying that the weeks 01, 02, 06, 08, 10, 11, 12 of 'x' had 10% or more of missing values
cmv(X, tscale="weekly")
#>             x     y
#> 1921-00 0.000 0.000
#> 1921-01 0.143 0.143
#> 1921-02 0.143 0.143
#> 1921-03 0.000 0.000
#> 1921-04 0.000 0.000
#> 1921-05 0.000 0.000
#> 1921-06 0.143 0.143
#> 1921-07 0.000 0.000
#> 1921-08 0.286 0.286
#> 1921-09 0.000 0.000
#> 1921-10 0.143 0.143
#> 1921-11 0.286 0.286
#> 1921-12 0.143 0.143
#> 1921-13 0.000 0.000
```
