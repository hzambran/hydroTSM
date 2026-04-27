# Months in Period

Given any starting and ending dates, it generates:  
1) a vector of class 'Date' with all the months between the two dates
(both of them included), OR  
2) the amount of months between the two dates

## Usage

``` r
mip(from, to, date.fmt = "%Y-%m-%d", out.type = "seq")
```

## Arguments

- from:

  Character indicating the starting date for creating the sequence. It
  has to be in the format indicated by `date.fmt`.

- to:

  Character indicating the ending date for creating the sequence. It has
  to be in the format indicated by `date.fmt`.

- date.fmt:

  Character indicating the format in which the dates are stored in
  `from` and `to`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html).

- out.type:

  character indicating the type of result that is given by this
  function. Valid values are:  
  -) seq : a vectorial sequence with all the months within the given
  year  
  -) nmbr: the number of days in the vectorial sequence with all the
  months within the given year

## Value

Depending on the value of `out.type`, it returns:  
1) a vector of class Date with all the months between `from` and `to`
(both of them included), OR  
2) a single numeric value with the amount of months between the two
dates.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`dip`](https://hzambran.github.io/hydroTSM/reference/dip.md),
[`diy`](https://hzambran.github.io/hydroTSM/reference/diy.md),
[`hip`](https://hzambran.github.io/hydroTSM/reference/hip.md),
[`yip`](https://hzambran.github.io/hydroTSM/reference/yip.md)

## Examples

``` r
# Sequence of monthly dates between "1961-01-01" and "1961-12-31" ##
mip("1961-01-01", "1961-12-31")
#>  [1] "1961-01-01" "1961-02-01" "1961-03-01" "1961-04-01" "1961-05-01"
#>  [6] "1961-06-01" "1961-07-01" "1961-08-01" "1961-09-01" "1961-10-01"
#> [11] "1961-11-01" "1961-12-01"

## Computing the number of months between "1961-01-01" and "1965-06-30", 
## with the date format  "%d-%m-%Y" ##
mip("01-01-1961", "30-06-1965", date.fmt= "%d-%m-%Y", out.type = "nmbr")
#> [1] 54
```
