# Amount of dry/wet days in a time series

Given a daily time series (usually precipitation), this function
computes the average amount of wet/dry days in each month.

## Usage

``` r
dwdays(x, ...)

# Default S3 method
dwdays(x, thr=0, type="wet", na.rm=TRUE, ... )

# S3 method for class 'data.frame'
dwdays(x, thr=0, type="wet", na.rm=TRUE, 
        dates=1, date.fmt="%Y-%m-%d", verbose=TRUE,...)

# S3 method for class 'matrix'
dwdays(x, thr=0, type="wet", na.rm=TRUE, 
        dates=1, date.fmt="%Y-%m-%d", verbose=TRUE,...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, usually with daily time series of
  precipitation.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- thr:

  numeric. Value of daily precipitation used as threshold for
  classifying a day as dry/wet or not. Days with a precipitation value
  larger to `thr` are classified as `wet days`, whereas precipitation
  values lower to `thr` are classified as `dry days`.

- type:

  character, indicating if the daily values have to be classified as dry
  or wet days. It works linked to the values specified in `thr`. Valid
  values are: wet, dry.

- na.rm:

  Logical. Should missing values be removed before counting?

- dates:

  numeric, factor or Date object indicating how to obtain the dates  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into Date class, using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days in `dates` be equal to the number of element in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- verbose:

  logical; if TRUE, progress messages are printed

- ...:

  further arguments passed to or from other methods.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Examples

``` r
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

## Average amount of wet days in each month (for this example, this means days 
## with precipitation larger than 0.1mm) 
dwdays(x, thr=0.1)
#>       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug 
#>  7.271429  7.471429 10.128571 13.600000 17.928571 18.185714 16.228571 15.414286 
#>       Sep       Oct       Nov       Dec 
#> 13.500000 12.600000 11.057143  8.442857 
```
