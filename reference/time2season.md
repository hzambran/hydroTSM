# Date/DateTime character -\> Seasonal character

This function transforms a character vector of Dates or DateTimes into a
character vector of seasons (summer, winter, autumn, spring), depending
on the value of `type`:  

When `type=default` -) winter = DJF: December, January, February  
-) spring = MAM: March, April, May  
-) summer = JJA: June, July, August  
-) autumn = SON: September, October, November  

When `type=FrenchPolynesia` -) winter = DJFM: December, January,
February, March  
-) spring = AM : April, May  
-) summer = JJAS: June, July, August, September  
-) autumn = ON : October, November

## Usage

``` r
time2season(x, out.fmt = "months", type="default")
```

## Arguments

- x:

  vector with the dates that have to be transformed. class(x) must be
  Date

- out.fmt:

  character, indicating the format of the output seasons. Possible
  values are:  
  -) seasons =\> c("winter", "spring", "summer", autumn")  
  -) months =\> c("DJF", "MAM", "JJA", SON") or c("DJFM", "AM", "JJAS",
  ON")

- type:

  character, indicating which weather seasons will be used for computing
  the output. Possible values are:  
  -) default =\> "winter"= Dec, Jan, Feb; "spring"= Mar, Apr, May;
  "summer"=Jun, Jul, Aug; "autumn"= Sep, Oct, Nov  
  -) FrenchPolynesia =\> "winter"= Dec, Jan, Feb, Mar; "spring"= Apr,
  May; "summer"=Jun, Jul, Aug, Sep; "autumn"= Oct, Nov

## Value

character vector with the weather season to which each date in `x`
belongs

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

Weather seasons corresponding to French Polynesia were defined following
a comment from Lydie Sichoix

## See also

[`dm2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md),
[`seasonalfunction`](https://hzambran.github.io/hydroTSM/reference/seasonalfunction.md),
[`extract`](https://hzambran.github.io/hydroTSM/reference/extract.md),
[`dip`](https://hzambran.github.io/hydroTSM/reference/dip.md),
[`mip`](https://hzambran.github.io/hydroTSM/reference/mip.md)

## Examples

``` r
## Sequence of daily dates between "1961-01-01" and "1961-12-31"
t <- dip("1961-01-01", "1961-12-31")
time2season(t)
#>   [1] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#>  [13] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#>  [25] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#>  [37] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#>  [49] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "MAM"
#>  [61] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#>  [73] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#>  [85] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#>  [97] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#> [109] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#> [121] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#> [133] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM"
#> [145] "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "MAM" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [157] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [169] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [181] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [193] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [205] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [217] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [229] "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA" "JJA"
#> [241] "JJA" "JJA" "JJA" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [253] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [265] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [277] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [289] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [301] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [313] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON"
#> [325] "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "SON" "DJF" "DJF"
#> [337] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#> [349] "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF" "DJF"
#> [361] "DJF" "DJF" "DJF" "DJF" "DJF"

## Sequence of monthly dates between "1961-01-01" and "1961-12-31"
t <- mip("1961-01-01", "1961-12-31")
time2season(t)
#>  [1] "DJF" "DJF" "MAM" "MAM" "MAM" "JJA" "JJA" "JJA" "SON" "SON" "SON" "DJF"
time2season(t, out.fmt="seasons")
#>  [1] "winter" "winter" "spring" "spring" "spring" "summer" "summer" "summer"
#>  [9] "autumn" "autumn" "autumn" "winter"
```
