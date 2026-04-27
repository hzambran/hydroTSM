# Vector -\> Zoo

Transform a numeric vector and its corresponding dates into a zoo
object.

## Usage

``` r
vector2zoo(x, dates, date.fmt = "%Y-%m-%d")
```

## Arguments

- x:

  numeric vector

- dates:

  character, factor, Date or POSIXct object with the dates corresponding
  to each element of `x`. Valid object class for `dates` are:
  `character, factor, Date, POSIXct`

- date.fmt:

  character indicating the format in which the dates are stored in
  `dates`, e.g. %Y-%m-%d. See ‘Details’ section in
  [`strptime`](https://rdrr.io/r/base/strptime.html).  
  ONLY required when `class(dates)=="factor"` or
  `class(dates)=="character"`.

## Value

a zoo object, with values given by `x` and time stamps given by `dates`

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`zoo`](https://rdrr.io/pkg/zoo/man/zoo.html),
[`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md),
[`dip`](https://hzambran.github.io/hydroTSM/reference/dip.md),
[`mip`](https://hzambran.github.io/hydroTSM/reference/mip.md),
[`yip`](https://hzambran.github.io/hydroTSM/reference/yip.md)

## Examples

``` r
##
## Example1: creating daily data

# Generating a numeric variable (e.g., read from the outputs of an hydrological model)
x <- 1:31

# Generating the dates corresponding to the previous values
dates <- dip("1961-01-01", "1961-01-31")

## Transforming from 'numeric' to 'zoo' class
z <- vector2zoo(x, dates)

##
## Example2: creating hourly data

# Generating a numeric variable
x <- rnorm(7)

# Generating hourly times since 17:00:00 up to 23:00:00 for 2012-Oct-15
dates <- ISOdatetime(2012, 10, 15, 17:23, 00, 0)

## Transforming from 'numeric' to 'zoo' class
z <- vector2zoo(x, dates)
```
