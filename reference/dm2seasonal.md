# (sub)Daily/Monthly -\> Seasonal Values

Generic function for computing a seasonal value for every year of a
sub-daily/daily/weekly/monthly time series

## Usage

``` r
dm2seasonal(x, ...)
subdaily2seasonal(x, ...)

# Default S3 method
dm2seasonal(x, season, FUN, na.rm = TRUE, out.fmt="%Y", ...)

# S3 method for class 'zoo'
dm2seasonal(x, season, FUN, na.rm = TRUE, out.fmt="%Y", ...)

# S3 method for class 'data.frame'
dm2seasonal(x, season, FUN, na.rm = TRUE, dates=1, date.fmt = "%Y-%m-%d", 
            out.type = "data.frame", out.fmt="%Y", ...)
        
# S3 method for class 'matrix'
dm2seasonal(x, season, FUN, na.rm = TRUE, dates=1, date.fmt = "%Y-%m-%d", 
            out.type = "data.frame", out.fmt="%Y", ...)
```

## Arguments

- x:

  zoo, xts, data.frame or matrix object, with sub-daily, daily, weekly
  or monthly time series.  
  Measurements at several gauging stations can be stored in a data.frame
  of matrix object, and in that case, each column of `x` represent the
  time series measured in each gauging station, and the column names of
  `x` have to correspond to the ID of each station (starting by a
  letter).

- season:

  character, indicating the weather season to be used for selecting the
  data. Valid values are:  
  -) DJF : December, January, February  
  -) MAM : March, April, May  
  -) JJA : June, July, August  
  -) SON : September, October, November  
  -) DJFM: December, January, February, March  
  -) AM : April, May  
  -) JJAS: June, July, August, September  
  -) ON : October, November

- FUN:

  Function that will be applied to ALL the values of `x` belonging to
  the given weather season (e.g., `FUN` can be some of `mean`, `max`,
  `min`, `sd`).  
  **The FUN value for the winter season (DJF or DJFM) is computed
  considering the consecutive months of December, January and
  February/March**. See 'Note' section.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the seasonal values are computed considering only those
  values different from NA (**very important when `FUN=sum`**)  
  -) FALSE: if there is AT LEAST one NA within a weather season, the
  corresponding seasonal values are NA

- out.fmt:

  Character indicating the date format for the output time series. See
  `format` in [`as.Date`](https://rdrr.io/r/base/as.Date.html). Possible
  values are:  
  -) %Y : only the year will be used for the time. Default option.
  (e.g., "1961" "1962"...)  
  -) %Y-%m-%d: a complete date format will be used for the time. (e.g.,
  "1961-01-01" "1962-01-01"...)

- dates:

  numeric, factor or Date object indicating how to obtain the dates.  
  If `dates` is a number (default), it indicates the index of the column
  in `x` that stores the dates  
  If `dates` is a factor, it is converted into Date class, by using the
  date format specified by `date.fmt`  
  If `dates` is already of Date class, the code verifies that the number
  of days on it be equal to the number of elements in `x`

- date.fmt:

  Character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- out.type:

  Character that defines the desired type of output. Valid values are:  
  -) data.frame: a data.frame, with as many columns as stations are
  included in `x`, the year corresponding to each seasonal value are
  used as row names.  
  -) db : a data.frame, with 4 columns will be produced.  
  The first column (StationID) stores the ID of the station The second
  column (Year) stores the year,  
  The third column (Season) stores the season,  
  The fourth column (Value) contains the seasonal value corresponding to
  the values specified in the previous three columns

- ...:

  further arguments passed to or from other methods.

## Value

A numeric vector with the seasonal values for all the years in which `x`
is defined.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

`FUN` is applied to all the values of `x` belonging to the selected
season, so the results of this function depends on the frequency
sampling of `x` and the type of function given by `FUN`  

## Warning

For any year, the `FUN` value for the winter season (DJF), is computed
considering only January and February, and the value of December is used
for computing the winter value of the next year.

## See also

,
[`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md),
[`seasonalfunction`](https://hzambran.github.io/hydroTSM/reference/seasonalfunction.md),
[`time2season`](https://hzambran.github.io/hydroTSM/reference/time2season.md),
[`extract`](https://hzambran.github.io/hydroTSM/reference/extract.md),
[`daily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md),
[`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md),
[`monthly2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md)

## Examples

``` r
############
## Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

## Winter (DJF) values of precipitation for each year of 'x'
dm2seasonal(x, FUN=sum, season="DJF")
#>  1921  1922  1923  1924  1925  1926  1927  1928  1929  1930  1931  1932  1933 
#> 144.0  71.5 340.0 181.0 360.0 249.5 213.6 110.3  97.9 204.4 194.7  40.2 120.6 
#>  1934  1935  1936  1937  1938  1939  1940  1941  1942  1943  1944  1945  1946 
#> 171.0 251.0 398.2 155.6 141.1 201.2 106.7 189.2  72.2 135.1  78.1 120.5 286.6 
#>  1947  1948  1949  1950  1951  1952  1953  1954  1955  1956  1957  1958  1959 
#> 223.3 295.9 105.2 216.5 769.1  65.4  94.8 157.6 341.8  91.6 149.2 260.9 221.6 
#>  1960  1961  1962  1963  1964  1965  1966  1967  1968  1969  1970  1971  1972 
#> 305.6 313.9 127.5 112.8  66.2 164.6 129.6 133.5 166.4 222.8 117.5 201.8 227.9 
#>  1973  1974  1975  1976  1977  1978  1979  1980  1981  1982  1983  1984  1985 
#> 136.0 137.0  53.4  19.0 568.8 419.4 350.2 241.0  39.8 167.8 147.8 232.0 231.8 
#>  1986  1987  1988  1989  1990 
#> 262.2 178.2 197.6 212.0 174.6 

############
## Loading the HOURLY discharge data for the Karamea at Gorge streamgauge station
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Mean winter (DJF) values of streamflow for each year of 'x'
dm2seasonal(x, FUN=mean, season="DJF")
#>      1980      1981      1982      1983      1984      1985 
#> 159.12862  50.04889 108.54718 145.24130  63.59592  94.16919 
subdaily2seasonal(x, FUN=mean, season="DJF") # same as above
#>      1980      1981      1982      1983      1984      1985 
#> 159.12862  50.04889 108.54718 145.24130  63.59592  94.16919 


```
