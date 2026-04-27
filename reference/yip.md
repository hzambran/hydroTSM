# Years in Period

Given any starting and ending dates, it generates:  
1) a vector of class Date with all the years between the two dates (both
of them included), OR  
2) the amount of years between the two dates

## Usage

``` r
yip(from, to, date.fmt = "%Y-%m-%d", out.type = "seq")
```

## Arguments

- from:

  Character indicating the starting date for creating the sequence. It
  has to be in the format indicated by `date.fmt`.

- to:

  Character indicating the ending date for creating the sequence. It has
  to be in the format indicated by `date.fmt`.

- date.fmt:

  character indicating the format in which the dates are stored in
  `from` and `to`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).

- out.type:

  Character indicating the type of result that is given by this
  function. Valid values are:  
  -) seq =\> a vectorial sequence with all the years within the given
  dates.  
  -) nmbr =\> the number of years within the given dates.

## Value

Depending on the value of `out.type`, it returns:  
1) seq : a vector of class Date with all the years between `from` and
`to` (both of them included), OR  
2) nmbr: a single numeric value with the amount of years between the two
dates.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`dip`](https://hzambran.github.io/hydroTSM/reference/dip.md),
[`diy`](https://hzambran.github.io/hydroTSM/reference/diy.md),
[`mip`](https://hzambran.github.io/hydroTSM/reference/mip.md)

## Examples

``` r
# Sequence of monthly dates between "1961-01-01" and "1961-12-31"
yip("1961-01-01", "1961-12-31")
#> [1] "1961-01-01"

## Computing the number of years between 1961 and 1975, 
## by using "%d-%m-%Y" as date format   ##
yip("01-01-1961", "01-01-1975", date.fmt= "%d-%m-%Y", out.type = "nmbr")
#> [1] 15
```
