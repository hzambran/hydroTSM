# Seasonality Index

Function to compute the seasonality index defined by Walsh and Lawler
(1981) to classify the precipitation regime.

## Usage

``` r
si(x, na.rm=TRUE, from=start(x), to=end(x), date.fmt="%Y-%m-%d", start.month=1)
```

## Arguments

- x:

  zoo object with daily or subdaily precipitation data.

- na.rm:

  Logical. Should missing values be removed?  
  -) TRUE : the monthly values are computed considering only those
  values different from NA  
  -) FALSE: if there is AT LEAST one NA within a month, the resulting
  average monthly value is NA .

- from:

  OPTIONAL, used for extracting a subset of values.  
  Character indicating the starting date for the values to be extracted.
  It must be provided in the format specified by `date.fmt`.

- to:

  OPTIONAL, used for extracting a subset of values.  
  Character indicating the ending date for the values to be extracted.
  It must be provided in the format specified by `date.fmt`.

- date.fmt:

  Character indicating the format in which the dates are stored in
  `dates`, `from` and `to`. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="numeric"`.

- start.month:

  \[OPTIONAL\]. Only used when the (hydrological) year of interest is
  different from the calendar year.

  numeric in \[1:12\] indicating the starting month of the
  (hydrological) year. Numeric values in \[1, 12\] represents months in
  \[January, December\]. By default `start.month=1`.

## Details

The seasonality index is computed as following:  

si = (1/R) \*sum(i=1, i=12, abs(xi - R/12) )  
where:  
-) xi: mean monthly precipitation for month i  
-) R: mean annual precipitation  

This index can theoretically vary from 0 (when all months have the same
rainfall) to 1.83 (when all the rainfall ocurrs in a single month). A
qualitative classification of degrees of seasonality is the following:  
——————————————————–  
si values \| Rainfall regime  
——————————————————–  
\<= 0.19 \| Very equable  
0.20 - 0.39 \| Equable but with a definite wetter season  
0.40 - 0.59 \| Rather seasonal with a short drier season  
0.60 - 0.79 \| Seasonal  
0.80 - 0.99 \| Markedly seasonal with a long drier season  
1.00 - 1.19 \| Most rain in 3 months or less  
\>= 1.20 \| Extreme, almost all rain in 1-2 months

## Value

numeric with the seasonality index

## References

Walsh, R. and Lawler, D. (1981). Rainfall seasonality: Description,
spatial patterns and change through time (British Isles, Africa).
Weather, 36(7), 201-208. doi:10.1002/j.1477-8696.1981.tb05400.x.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md)

## Examples

``` r
############################
## Ex 1: Seasonality index for a rain gauge with equable precipitation , 
##       but with a definite wetter season 

## Loading daily precipitation data at the station San Martino di Castrozza, 
## Trento Province, Italy, from 01/Jan/1921 to 31/Dec/1990.
data(SanMartinoPPts)
x <- SanMartinoPPts

## Amount of years in 'x' (needed for computations)
( nyears <- yip(from=start(x), to=end(x), out.type="nmbr" ) )
#> [1] 70


## Boxplot of monthly values, to look at the seasonal cycle

## Daily to Monthly
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

## Mean monthly values at the station
monthlyfunction(m, FUN=sum, na.rm=TRUE) / nyears
#>       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug 
#>  60.52286  59.76571  83.95429 115.40143 159.68000 160.65143 147.85571 147.65571 
#>       Sep       Oct       Nov       Dec 
#> 130.43714 145.35571 139.91714  76.73714 

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values of precipitation
boxplot( coredata(m) ~ months, col="lightblue", 
         main="Monthly precipitation, [mm]", ylab="P, [mm]")


# computing seasonality index
( si(x) )
#> [1] 0.2781517

############################
## Ex 2: Seasonality index for a rain gauge with markedly seasonal regime 
##       with a long dry season

## Loading daily precipitation data at the station Cauquenes en El Arrayan, 
## Maule Region, Chile, from 01/Jan/1979 to 31/Dec/2020.
data(Cauquenes7336001)
x <- Cauquenes7336001[, 1] # P is the first column

## Boxplot of monthly values, to look at the seasonal cycle

## Daily to Monthly
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

## Mean monthly values at the station
monthlyfunction(m, FUN=sum, na.rm=TRUE) / nyears
#>        Jan        Feb        Mar        Apr        May        Jun        Jul 
#>   4.720232   7.717013  11.198082  32.821179 100.056765 120.901501 109.348785 
#>        Aug        Sep        Oct        Nov        Dec 
#>  82.771381  44.487277  26.036642  13.384942   8.066510 

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values of precipitation
boxplot( coredata(m) ~ months, col="lightblue", 
         main="Monthly precipitation, [mm]", ylab="P, [mm]")


# computing seasonality index
( si(x) )
#> [1] 0.8046454
```
