# Weekly Function

Generic function for obtaining 52 weekly values of a zoo object, by
applying any R function to ALL the values in the object belonging to
each one of the 52 calendar weeks (starting on Monday).

## Usage

``` r
weeklyfunction(x, ...)

# Default S3 method
weeklyfunction(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                                 start.fmt= "%H:%M:%S", tz, ...)

# S3 method for class 'zoo'
weeklyfunction(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                             start.fmt= "%H:%M:%S", tz, ...)

# S3 method for class 'data.frame'
weeklyfunction(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                        start.fmt= "%H:%M:%S", tz, dates=1, date.fmt="%Y-%m-%d",
                        out.type="data.frame", verbose=TRUE,...)
             
# S3 method for class 'matrix'
weeklyfunction(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                        start.fmt= "%H:%M:%S", tz, dates=1, date.fmt="%Y-%m-%d",
                        out.type="data.frame", verbose=TRUE,...)
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

- na.rm.max:

  Numeric in \[0, 1\]. It is used to define the maximum percentage of
  missing values allowed in each month to keep the weekly aggregated
  value in the output object of this function. In other words, if the
  percentage of missing values in a given month is larger or equal than
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
## Ex1: Computation of mean WEEKLY values from DAILY ts, removing any missing value in 'x'

# Loading DAILY streamflows (3 years) at the station 
# Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Mean WEEKLY streamflows at station 'x'
weeklyfunction(x, FUN=mean, na.rm=TRUE)
#>        00        01        02        03        04        05        06        07 
#> 14.362857 18.376667 11.539048 11.453333  9.287143  8.720952 10.254762  9.674762 
#>        08        09        10        11        12        13        14        15 
#>  7.917619  8.867143 10.269048  9.977143  8.357619  7.819524  7.841905  6.713810 
#>        16        17        18        19        20        21        22        23 
#>  6.286190  6.950000  5.672381  4.856190  4.525714  4.920476  4.492381  3.684286 
#>        24        25        26        27        28        29        30        31 
#>  3.808095  3.751905  3.450952  3.011429  2.389048  1.985238  1.929524  2.219048 
#>        32        33        34        35        36        37        38        39 
#>  1.595238  1.421429  1.296667  1.248095  1.603333  1.606667  3.009524  1.690000 
#>        40        41        42        43        44        45        46        47 
#>  1.800000  1.579048  2.426190  1.693333  1.911429  4.810952  6.179048  7.212381 
#>        48        49        50        51        52        53 
#> 10.704286  6.670476  5.822381  5.869524  8.880000  5.520000 

######################
## Ex2: Computation of mean WEEKLY values from HOURLY ts, removing any missing value in 'x'

# Loading HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Mean WEEKLY streamflows at station 'x'. Each day starts at 00:00:00
weeklyfunction(x, FUN=mean, na.rm=TRUE)
#>       [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
#> x 64.20701 109.5082 139.8828 155.7803 82.80367 65.03214 54.78373 77.01825
#>      [,9]    [,10]    [,11]    [,12]    [,13]    [,14]   [,15]    [,16]
#> x 73.9119 48.27778 65.67629 54.88958 69.77341 94.99048 70.1496 143.3576
#>      [,17]    [,18]    [,19]   [,20]    [,21]    [,22]   [,23]    [,24]
#> x 81.76815 89.25139 121.5399 149.725 222.3459 98.86915 146.548 88.68194
#>      [,25]    [,26]    [,27]    [,28]  [,29]    [,30]    [,31]    [,32]
#> x 92.18542 83.87103 128.3997 167.6301 151.89 95.66558 115.6955 150.8795
#>      [,33]    [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
#> x 145.6131 111.3687 118.7721 160.5986 184.5939 207.7563 182.2772 144.4881
#>      [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]
#> x 131.6928 121.4306 176.3148 118.6647 127.3617 104.8079 153.1652 156.4974
#>      [,49]    [,50]    [,51]    [,52]    [,53]    [,54]
#> x 107.9865 150.0218 109.5492 148.5809 95.89984 45.85926

######################
## Ex3: Computation of mean WEEKLY values from HOURLY ts, removing any missing value in 'x'
##      and starting each day at 08:00:00

# Loading HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Mean WEEKLY streamflows at station 'x'. Each day starts at 00:00:00
weeklyfunction(x, FUN=mean, na.rm=TRUE, start="00:00:00")
#>       [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
#> x 64.20701 109.5082 139.8828 155.7803 82.80367 65.03214 54.78373 77.01825
#>      [,9]    [,10]    [,11]    [,12]    [,13]    [,14]   [,15]    [,16]
#> x 73.9119 48.27778 65.67629 54.88958 69.77341 94.99048 70.1496 143.3576
#>      [,17]    [,18]    [,19]   [,20]    [,21]    [,22]   [,23]    [,24]
#> x 81.76815 89.25139 121.5399 149.725 222.3459 98.86915 146.548 88.68194
#>      [,25]    [,26]    [,27]    [,28]  [,29]    [,30]    [,31]    [,32]
#> x 92.18542 83.87103 128.3997 167.6301 151.89 95.66558 115.6955 150.8795
#>      [,33]    [,34]    [,35]    [,36]    [,37]    [,38]    [,39]    [,40]
#> x 145.6131 111.3687 118.7721 160.5986 184.5939 207.7563 182.2772 144.4881
#>      [,41]    [,42]    [,43]    [,44]    [,45]    [,46]    [,47]    [,48]
#> x 131.6928 121.4306 176.3148 118.6647 127.3617 104.8079 153.1652 156.4974
#>      [,49]    [,50]    [,51]    [,52]    [,53]    [,54]
#> x 107.9865 150.0218 109.5492 148.5809 95.89984 45.85926
```
