# Change the time zone of a sub-daily zoo object

Changes the time zone used by the time index of a sub-daily `zoo` object
whose index inherits from `POSIXct` or `POSIXlt`.

## Usage

``` r
change_tz(x, ...)

# S3 method for class 'zoo'
change_tz(x, new.tz, old.tz=NULL, ...)
```

## Arguments

- x:

  `zoo` object with a sub-daily `POSIXct` or `POSIXlt` time index.

- new.tz:

  character. Time zone to assign to the output time index.

- old.tz:

  character or `NULL`. Time zone currently used by the time index. When
  `NULL`, it is inferred from `attr(time(x), "tzone")`. When provided,
  it must match the detected time zone when the time index already has
  one.

- ...:

  further arguments passed to or from other methods.

## Details

Both `old.tz` and `new.tz` are checked against
[`OlsonNames`](https://rdrr.io/r/base/timezones.html).

`change_tz()` preserves the represented instants and changes their time
zone for display and downstream calendar grouping. For example, midnight
UTC is shown as the previous evening in `"America/Santiago"` when the
applicable UTC offset is negative. It does not reinterpret clock labels
in a new time zone.

The values and dimensions of `x` are preserved; only the time index is
changed.

## Value

A `zoo` object with the same values as `x`, but with its time index
converted to `new.tz`.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`zoo`](https://rdrr.io/pkg/zoo/man/zoo.html),
[`OlsonNames`](https://rdrr.io/r/base/timezones.html),
[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html),
[`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md),
[`subhourly2nhourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2nhourly.md),
[`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md)

## Examples

``` r
dates <- seq(as.POSIXct("2001-01-01 00:00:00", tz="UTC"),
             by="hour", length.out=3)
x <- zoo(1:3, dates)

y <- change_tz(x, new.tz="America/Santiago")
time(y)
#> [1] "2000-12-31 21:00:00 -03" "2000-12-31 22:00:00 -03"
#> [3] "2000-12-31 23:00:00 -03"

## The represented instants and the observed values are preserved.
identical(as.numeric(time(x)), as.numeric(time(y)))
#> [1] TRUE
identical(coredata(x), coredata(y))
#> [1] TRUE
```
