# Annual Function

Generic function for obtaining a SINGLE annual value of a zoo object, by
applying any R function to ALL the values in `x` belonging to the same
year, and then applying the same function to ALL the previously computed
annual values (e.g., for computing the average annual precipitation or
the mean annual streamflow of a long-term time series).

## Usage

``` r
annualfunction(x, FUN, na.rm = TRUE, ...)

# Default S3 method
annualfunction(x, FUN, na.rm = TRUE, ...)

# S3 method for class 'zoo'
annualfunction(x, FUN, na.rm = TRUE, ...)

# S3 method for class 'data.frame'
annualfunction(x, FUN, na.rm = TRUE, dates=1, date.fmt = "%Y-%m-%d", 
        verbose = TRUE, ...)
        
# S3 method for class 'matrix'
annualfunction(x, FUN, na.rm = TRUE, dates=1, date.fmt = "%Y-%m-%d", 
        verbose = TRUE, ...)
```

## Arguments

- x:

  zoo, xts, data.frame or matrix object, with daily/monthly/annual time
  series.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- FUN:

  Function that will be used to compute the final annual value (e.g.,
  `FUN` may be some of `mean`, `sum`, `max`, `min`, `sd`) .

- na.rm:

  Logical. Should missing values be removed?.  
  -) TRUE : the annual values are computed considering only those values
  different from NA  
  -) FALSE: if there is AT LEAST one NA within a year, the resulting
  annual value will be NA

- dates:

  numeric, factor or Date object indicating how to obtain the dates
  corresponding to each gauging station.  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it have to be converted into Date class, using
  the date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days in `dates` be equal to the number of elements in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- verbose:

  Logical; if TRUE, progress messages are printed.

- ...:

  further arguments passed to or from other methods.

## Value

When `x` is a time series, a single annual value is returned.  
For a data frame, a named vector with the appropriate method being
applied column by column.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

`FUN` is first applied to all the values of `x` belonging to the same
year and then it is applied to all the previously computed annual values
to get the final result. Its result will depend on the sampling
frequency of `x` and the type of function provided by `FUN` (**special
attention have to be put when `FUN=sum`**)

## See also

[`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md),
[`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`monthly2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`yip`](https://hzambran.github.io/hydroTSM/reference/yip.md)

## Examples

``` r
## Loading the SanMartino daily precipitation data (1921-1990)
data(SanMartinoPPts)
x <- SanMartinoPPts

# Amount of years in 'x' (needed for computing the average)
nyears <- length( seq(from=time(x[1]), to=time(x[length(x)]), by="years") )

## Average annual precipitation for the 70 years period. 
# It is necessary to divide by the amount of years to obtain the average annual value, 
# otherwise it will give the total precipitation for all the 70 years.
annualfunction(x, FUN=sum, na.rm=TRUE) / nyears
#>    value 
#> 1427.934 


#####################
### verification ####
# Daily to annual
a <- daily2annual(x, FUN=sum, na.rm=TRUE)

# Mean annual value
mean(a)
#> [1] 1427.934

##############################
##############################
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Dates of 'x'
dates <- as.Date(x[,1])


## Computation of the average annual precipitation
if (FALSE) { # \dontrun{

## Transforming 'x' into a zoo object
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

## Average annual precipitation, for the first 5 stations in 'x'
annualfunction(z[ ,1:5], FUN=sum)/nyears
 
} # }
```
