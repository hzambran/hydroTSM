# Station Name -\> Time Series

This function takes a data.frame whose columns contains the time series
of several gauging stations, along with a character representing the
name of one gauging station, and extracts the time series corresponding
to that station.

## Usage

``` r
sname2ts(x, sname, dates=1, date.fmt = "%Y-%m-%d", var.type, 
         tstep.out = "daily", FUN, na.rm = TRUE, from, to)
```

## Arguments

- x:

  data.frame containing the complete times series of all the stations.  
  It may also contain 1 column with the dates of the measurements, or
  they can be provided in a different way (see `dates` below).

- sname:

  Character representing the name of a station, which have to correspond
  to one column name in `x`

- dates:

  numeric, factor, Date object indicating how to obtain the dates
  corresponding to the `sname` station.  
  -) If `dates` is a number (default), it indicates the index of the
  column in `x` that stores the dates  
  -) If `dates` is a factor, it is converted into Date class, using the
  date format specified by `date.fmt`  
  -) If `dates` is already of Date class, the code verifies that the
  number of days in `dates` be equal to the number of element in `x`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- var.type:

  character representing the type of variable being plotted. Used for
  determining the function used for computing the monthly or/and annual
  values when `FUN` is missing. Valid values are:  
  -) Precipitation =\> `FUN=sum`  
  -) Temperature =\> `FUN= mean`  
  -) Flow =\> `FUN= mean`

- tstep.out:

  character that defines the time step of the desired output time
  series. Valid values are:  
  -) daily : daily time series  
  -) monthly: monthly time series  
  -) annual : annual time series

- FUN:

  ONLY required when `var.type` is missing and `tstep` is one of
  `monthly` or `annual`.  
  Function that have to be applied for transforming from daily to
  monthly or annual time step (e.g., for precipitation `FUN=sum` and for
  temperature and flow ts, `FUN=mean`)

- na.rm:

  a logical value indicating whether 'NA' values should be stripped
  before the computation proceeds.

- from:

  OPTIONAL, used for extracting a subset of values.  
  Character indicating the starting date for the values to be extracted.
  It must be provided in the format specified by `date.fmt`.

- to:

  OPTIONAL, used for extracting a subset of values.  
  Character indicating the ending date for the values to be extracted.
  It must be provided in the format specified by `date.fmt`.

## Value

zoo object

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`sname2plot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md)

## Examples

``` r
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Annual values of temperature at the station "T9105", stored in 'EbroPPtsMonthly'.
sname2ts(EbroPPtsMonthly, sname="P9001", dates=1, FUN=sum, tstep.out="annual")
#>   1941   1942   1943   1944   1945   1946   1947   1948   1949   1950 
#> 1094.7 1059.5  978.2  936.4  848.1  700.7  869.7  571.7  762.7  807.1 
```
