# Monthly Function

Generic function for obtaining 12 monthly values of a zoo object, by
applying any R function to ALL the values in the object belonging to
each one of the 12 calendar months (Jan...Dec).

## Usage

``` r
monthlyfunction(x, ...)

# Default S3 method
monthlyfunction(x, FUN, na.rm = TRUE, ...)

# S3 method for class 'zoo'
monthlyfunction(x, FUN, na.rm=TRUE,...)

# S3 method for class 'data.frame'
monthlyfunction(x, FUN, na.rm = TRUE, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", verbose = TRUE, ...)
             
# S3 method for class 'matrix'
monthlyfunction(x, FUN, na.rm = TRUE, dates=1, 
        date.fmt = "%Y-%m-%d", out.type = "data.frame", verbose = TRUE, ...)
```

## Arguments

- x:

  zoo, xts, data.frame or matrix object, with daily or monthly time
  series.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- FUN:

  Function that will be applied to ALL the values in `x` belonging to
  each one of the 12 months of the year (e.g., FUN can be some of
  `mean`, `sum`, `max`, `min`, `sd`).

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the monthly values and FUN are computed considering only
  those values in `x` different from NA  
  -) FALSE: if there is AT LEAST one NA within a month, the
  corresponding monthly value will be NA

- dates:

  It is only used when `x` is not a zoo object.  
  numeric, factor, Date indicating how to obtain the dates.  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into 'Date' class, using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days in `dates` be equal to the number of elements in `x`

- date.fmt:

  It is only used when `x` is not a zoo object.  
  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.type:

  It is only used when `x` is a matrix or data.frame.  
  Character defining the desired type of output. Valid values are:  
  -) data.frame: a data.frame, with 12 columns representing the months,
  and as many rows as gauging stations are included in `x`  
  -) db : a data.frame, with 4 columns will be produced. Useful for a
  posterior boxplot  
  The first column ('StationID') will store the ID of the station,  
  The second column ('Year') will store the year,  
  The third column ('Month') will store month,  
  The fourth column ('Value') will contain the monthly value
  corresponding to the three previous columns.

- verbose:

  Logical; if TRUE, progress messages are printed

- ...:

  further arguments passed to or from other methods

## Value

When `x` is a zoo object, a numeric vector with 12 elements representing
the computed monthly value for each month.  
When `x` is a data.frame which columns represent measurements at
different gauging stations, the resulting object is a data.frame with 12
columns and as many rows as gauging stations are in `x`, each row
storing the computed 12 monthly value for each gauging station.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

Due to the fact that `FUN` is applied over all the elements in `x`
belonging to a given calendar month, its result will depend on the
sampling frequency of `x` and the type of function provided by `FUN`
(**special attention have to be put when `FUN=sum`**)

## See also

[`annualfunction`](https://hzambran.github.io/hydroTSM/reference/annualfunction.md),
[`seasonalfunction`](https://hzambran.github.io/hydroTSM/reference/seasonalfunction.md),
[`dm2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`daily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
[`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md)

## Examples

``` r
## Loading daily streamflows (3 years) at the station 
## Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Mean monthly streamflows at station 'x'
monthlyfunction(x, FUN=mean, na.rm=TRUE)
#>       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug 
#> 12.881613  9.035119  9.015484  7.243778  4.982366  3.907444  2.448387  1.529355 
#>       Sep       Oct       Nov       Dec 
#>  1.909556  1.860215  5.932556  6.895591 


############################
## Boxplot of monthly values

## Daily to Monthly
m <- daily2monthly(x, FUN=mean, na.rm=TRUE)

## Median of the monthly values at the station
monthlyfunction(m, FUN=median, na.rm=TRUE)
#>       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug 
#> 14.804194  7.991071  8.657419  6.917333  4.688065  3.666333  2.287742  1.454839 
#>       Sep       Oct       Nov       Dec 
#>  1.546000  1.791613  4.484000  5.255484 

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values
boxplot( coredata(m) ~ months, col="lightblue", main="Monthly streamflows, [m3/s]")



##############################
##############################
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Dates of 'x'
dates <- as.Date(x[,1])

## Monthly precipitation of all the stations in 'x'
if (FALSE) { # \dontrun{

## Sum of the monthly values in each station of 'x'
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

m <- monthlyfunction(z, FUN=sum)


## Another way of computing the sum of the monthly values in each station of 'x'
## This way is usefult for posteriori boxplots
m2 <- monthlyfunction(x, FUN=sum, dates=1, out.type="db")

## Average monthly precipitation in each station of 'x'
m2$Value <- m2$Value / nyears 

## Creating monthly factors
m2$Month <- factor(m2$Month, levels=month.abb)

## boxplot of the monthly values in all stations
boxplot(Value ~ Month, m2, col="lightyellow", main="Monthly Precipitation, [mm/month]")
} # }
```
