# (sub)Daily -\> Monthly

Generic function for transforming a DAILY (sub-daily or weekly) regular
time series into a MONTHLY one

## Usage

``` r
daily2monthly(x, ...)
subdaily2monthly(x, ...)

# Default S3 method
daily2monthly(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

# S3 method for class 'zoo'
daily2monthly(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

# S3 method for class 'data.frame'
daily2monthly(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", out.fmt="numeric", 
        verbose=TRUE, ...)

# S3 method for class 'matrix'
daily2monthly(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
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

  Function that have to be applied for transforming from daily to
  monthly time step (e.g., for precipitation `FUN=sum` and for
  temperature and streamflow ts `FUN=mean`).  

  `FUN` MUST accept the `na.rm` argument, because `na.rm` is passed to
  `FUN`.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the monthly values are computed only for months with a
  percentage of missing values less than `na.rm.max`  
  -) FALSE: if there is AT LEAST one NA within a month, the corresponing
  monthly values in the output object will be `NA`.

- na.rm.max:

  Numeric in \[0, 1\]. It is used to define the maximum percentage of
  missing values allowed in each month to keep the monthly aggregated
  value in the output object of this function. In other words, if the
  percentage of missing values in a given month is larger than
  `na.rm.max` the corresponding monthly value will be `NA`.

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

a zoo object with monthly time frequency

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`cmv`](https://hzambran.github.io/hydroTSM/reference/cmv.md),
[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md),
[`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md),
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md),
[`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md),
[`as.Date`](https://rdrr.io/r/base/as.Date.html)

## Examples

``` r
######################
## Ex1: Computation of monthly values, removing any missing value in 'x'

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

## Agreggating from Daily to Monthly, removing any missing value in 'x'
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

## Agreggating from Daily to Monthly, removing any missing value in 'x', but now
## allowing a maximum of 20% of missing values in each month 
m <- daily2monthly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.2)

######################
## Ex2: Computation of monthly values only when the percentage of NAs in each
#       month is lower than a user-defined percentage (10% in this example).

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

## Daily to monthly, only for months with less than 10% of missing values
m2 <- daily2monthly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1)

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="month")
#> 1921-01 1921-02 1921-03 
#>   0.065   0.107   0.129 

######################
## Ex3: Loading the HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to monthly ts
subdaily2monthly(x, FUN=mean, na.rm=TRUE)
#>  Dec 1979  Jan 1980  Feb 1980  Mar 1980  Apr 1980  May 1980  Jun 1980  Jul 1980 
#>  72.26667 191.62124 124.76954 117.49852  90.50181 169.69301  93.47722  87.06210 
#>  Aug 1980  Sep 1980  Oct 1980  Nov 1980  Dec 1980  Jan 1981  Feb 1981  Mar 1981 
#> 222.10578 303.19277 133.99139 177.99583  63.62298  31.54099  55.51131  52.34556 
#>  Apr 1981  May 1981  Jun 1981  Jul 1981  Aug 1981  Sep 1981  Oct 1981  Nov 1981 
#>  93.68347 110.16062 123.24333 199.69718  77.07191 250.47316 168.87443 144.29861 
#>  Dec 1981  Jan 1982  Feb 1982  Mar 1982  Apr 1982  May 1982  Jun 1982  Jul 1982 
#> 131.14839 109.35054  82.63497  43.15659  37.57625 182.13065  96.27083 129.31156 
#>  Aug 1982  Sep 1982  Oct 1982  Nov 1982  Dec 1982  Jan 1983  Feb 1983  Mar 1983 
#> 132.17325 156.18373  94.50417 178.57236 203.19906 168.58710  55.22664 107.80282 
#>  Apr 1983  May 1983  Jun 1983  Jul 1983  Aug 1983  Sep 1983  Oct 1983  Nov 1983 
#> 182.95583 266.34610 120.74417 118.37500  86.66075 173.20348 163.91548 100.01792 
#>  Dec 1983  Jan 1984  Feb 1984  Mar 1984  Apr 1984  May 1984  Jun 1984  Jul 1984 
#>  95.95202  58.99758  33.92385  50.36371  90.55611  93.75255  98.94542 152.39005 
#>  Aug 1984  Sep 1984  Oct 1984  Nov 1984  Dec 1984  Jan 1985  Feb 1985  Mar 1985 
#> 153.77204  83.41405 204.43755  71.83971  88.81458 134.81720  51.89911  30.08199 
#>  Apr 1985  May 1985  Jun 1985  Jul 1985  Aug 1985  Sep 1985  Oct 1985  Nov 1985 
#>  87.15014  40.92621  82.35111 108.26599  88.46546 131.81391  62.01144 109.56347 
#>  Dec 1985 
#> 153.39036 
```
