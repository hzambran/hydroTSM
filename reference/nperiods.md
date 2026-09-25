# Number of Years, Months, Weeks, Days, or Hours in a zoo Object

Generic functions for computing the temporal span covered by a
[`zoo`](https://rdrr.io/pkg/zoo/man/zoo.html) object, expressed as
years, months, weeks, days, or hours.

## Usage

``` r
nyears(x, ...)
nmonths(x, ...)
nweeks(x, ...)
ndays(x, ...)
nhours(x, ...)

# S3 method for class 'zoo'
nyears(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
# S3 method for class 'zoo'
nmonths(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
# S3 method for class 'zoo'
nweeks(x,
                     week.grouping="calendar",
                     verbose=TRUE, ...)
# S3 method for class 'zoo'
ndays(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
# S3 method for class 'zoo'
nhours(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
```

## Arguments

- x:

  A zoo object with a regular time index.

- only.integer.output:

  logical; if `FALSE` (default), decimal values are returned when the
  last year, month, day, or hour is not complete. If `TRUE`, only the
  number of whole years, months, days, or hours is returned, and a
  warning is issued when the last period is incomplete.

- week.grouping:

  character indicating how weeks are counted by `nweeks`. Valid values
  are:  
  -) calendar: distinct calendar year-week groups defined by %Y-%W.
  Weeks crossing a calendar-year boundary are split.  
  -) sequential: consecutive 7-day groups starting at the first date in
  `x`. This argument is fully compatible with
  [`daily2weekly`](https://hzambran.github.io/hydroTSM/reference/daily2weekly.md).

- tz:

  character, indicating the time zone in which the time index of `x` is
  located. If missing, the time zone stored in the index is used when
  available. See
  [`isComplete`](https://hzambran.github.io/hydroTSM/reference/isComplete.md).

- verbose:

  logical; if `TRUE`, informative messages are shown in the screen. The
  functions call
  [`isComplete`](https://hzambran.github.io/hydroTSM/reference/isComplete.md)
  and, when `x` has missing date/time values, they are reported by
  `isComplete`.

- ...:

  further arguments passed to or from other methods.

## Details

The returned value is computed from the first date/time to the inferred
end of the last observation. The end of the last observation is inferred
from the regular time step of `x`. For example, a daily object from
`2020-01-01` to `2020-01-31` returns `1` month and `31` days.

Before computing the temporal span, the functions call
`isComplete(x, out.type="all")`. Therefore, incomplete objects are still
accepted when their time index has a regular frequency, but missing
date/time values are reported when `verbose=TRUE`.

For `nweeks`, the result is the number of weekly groups that
`daily2weekly` would produce with the same `week.grouping`.

## Value

A numeric value with the temporal span of `x` expressed in the requested
unit. `nweeks` returns the integer number of weekly groups. For the
other functions, an integer value is returned when
`only.integer.output=TRUE`.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`isComplete`](https://hzambran.github.io/hydroTSM/reference/isComplete.md),
[`daily2weekly`](https://hzambran.github.io/hydroTSM/reference/daily2weekly.md),
[`zoo`](https://rdrr.io/pkg/zoo/man/zoo.html)

## Examples

``` r
dates <- as.Date("2020-01-01") + 0:30
x <- zoo::zoo(seq_along(dates), dates)

nyears(x, verbose=FALSE)
#> [1] 0.08469945
nmonths(x, verbose=FALSE)
#> [1] 1
nweeks(x, verbose=FALSE)
#> [1] 5
ndays(x, verbose=FALSE)
#> [1] 31
nhours(x, verbose=FALSE)
#> [1] 744

y <- x[1:15]
ndays(y, verbose=FALSE)
#> [1] 15
ndays(y, only.integer.output=TRUE, verbose=FALSE)
#> [1] 15
```
