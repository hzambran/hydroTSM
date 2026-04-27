# Seasonal Function

Generic function for applying any R function to a zoo object, in order
to obtain 4 representative seasonal values.

## Usage

``` r
seasonalfunction(x, ...)

# Default S3 method
seasonalfunction(x, FUN, na.rm = TRUE, type="default", ...)

# S3 method for class 'zoo'
seasonalfunction(x, FUN, na.rm = TRUE, type="default", ...)

# S3 method for class 'data.frame'
seasonalfunction(x, FUN, na.rm = TRUE, type="default",
                          dates=1, date.fmt = "%Y-%m-%d", 
                          out.type = "data.frame", verbose = TRUE, ...)
                          
# S3 method for class 'matrix'
seasonalfunction(x, FUN, na.rm = TRUE, type="default",
                          dates=1, date.fmt = "%Y-%m-%d", 
                          out.type = "data.frame", verbose = TRUE, ...)
```

## Arguments

- x:

  zoo, data.frame or matrix object, with daily or monthly time series.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- FUN:

  Function that will be applied to ALL the values in `x` belonging to
  each one of the 4 weather seasons (e.g., `FUN` can be some of `mean`,
  `max`, `min`, `sd`).  

- na.rm:

  Logical. Should missing values be removed before the computations?  
  -) TRUE : the monthly values are computed considering only those
  values in `x` different from NA (**very important when `FUN=sum`**)  
  -) FALSE: if there is AT LEAST one NA within a month, the FUN and
  monthly values are NA

- type:

  character, indicating which weather seasons will be used for computing
  the output. Possible values are:  
  -) default =\> "winter"= Dec, Jan, Feb; "spring"= Mar, Apr, May;
  "summer"=Jun, Jul, Aug; "autumn"= Sep, Oct, Nov  
  -) FrenchPolynesia =\> "winter"= Dec, Jan, Feb, Mar; "spring"= Apr,
  May; "summer"=Jun, Jul, Aug, Sep; "autumn"= Oct, Nov

- dates:

  numeric, factor, Date indicating how to obtain the dates.  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into Date class, by using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days in `dates` be equal to the number of element in `x`

- date.fmt:

  Character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.type:

  Character defining the desired type of output. Valid values are:  
  -) data.frame: a data.frame, with 4 columns representing the weather
  seasons, and as many rows as stations are included in `x`  
  -) db : a data.frame, with 4 colums will be produced. Useful for a
  posterior boxplot  
  The first column (StationID) will store the ID of the station,  
  The second column (Year) will store the year,  
  The third column (Season) will store the season,  
  The fourth column (Value) will contain the seasonal value
  corresponding to that year and that station.

- verbose:

  Logical; if TRUE, progress messages are printed

- ...:

  further arguments passed to or from other methods

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

`FUN` is applied to all the values of `x` belonging to each one of the
four weather seasons, so the results of this function depends on the
frequency sampling of `x` and the type of function given by `FUN`  

## Warning

The `FUN` value for the winter season (DJF) is computed considering the
consecutive months of December, January and February. Therefore, if `x`
starts in January and ends in December of any year, the winter value of
the first year is computed considering only the January and February
value of that year, whereas the December value of the first year is used
to compute the winter value of the next year.

## See also

[`dm2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`time2season`](https://hzambran.github.io/hydroTSM/reference/time2season.md),
[`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md),
[`annualfunction`](https://hzambran.github.io/hydroTSM/reference/annualfunction.md),
[`extract`](https://hzambran.github.io/hydroTSM/reference/extract.md)

## Examples

``` r
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

# Amount of years
nyears <- yip(from=start(x), to=end(x), out.type="nmbr")

## Mean annual precipitation.
# It is necessary to divide by the amount of years to obtain the mean annual value, 
# otherwise it will give the total precipitation for all the 70 years
seasonalfunction(x, FUN=sum, na.rm=TRUE) / nyears
#>      DJF      MAM      JJA      SON 
#> 197.0257 359.0357 456.1629 415.7100 

#####################
### verification ####
# Mean winter (DJF) value
sum( extractzoo(x, trgt="DJF") ) / nyears
#> [1] 197.0257

# Mean spring (MAM) value
sum( extractzoo(x, trgt="MAM") ) / nyears
#> [1] 359.0357

# Mean summer (JJA) value
sum( extractzoo(x, trgt="JJA") ) / nyears
#> [1] 456.1629

# Mean autumn (SON) value
sum( extractzoo(x, trgt="SON") ) / nyears
#> [1] 415.71

```
