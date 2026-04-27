# Sampling Frequency

This function identifies the sampling frequency of a zoo object. It is
wrapper to the
[`periodicity`](https://rdrr.io/pkg/xts/man/periodicity.html) function
of the xts package.

## Usage

``` r
sfreq(x, min.year = 1800)
```

## Arguments

- x:

  variable of type zoo, xts or ts, with AT LEAST 2 elements, AND with a
  (sub)hourly, hourly, daily, weekly, monthly, quarterly, or annual
  (yearly) sampling frequency.

- min.year:

  integer used for a correct identification of the sampling frequency
  when `x` is an annual (yearly) time series.

## Details

See further details in the
[`periodicity`](https://rdrr.io/pkg/xts/man/periodicity.html) function
of the xts package.

## Value

Character. Possible values are:  
-) minute : indicating that the sampling frequency in `x` is
sub-hourly  
-) hourly : indicating that the sampling frequency in `x` is hourly  
-) daily : indicating that the sampling frequency in `x` is daily  
-) weekly : indicating that the sampling frequency in `x` is weekly  
-) monthly : indicating that the sampling frequency in `x` is monthly  
-) quarterly : indicating that the sampling frequency in `x` is
quarterly  
-) annual : indicating that the sampling frequency in `x` is annual

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## See also

[`frequency`](https://rdrr.io/r/stats/time.html),
[periodicity](https://rdrr.io/pkg/xts/man/periodicity.html)

## Examples

``` r
## Ex1: sub-hourly data
## Creating a dummy 15-min zoo object, with 1 as the only value in each time period
dt  <- seq( from=as.POSIXct("2021-06-30 00:15"), to=as.POSIXct("2021-06-30 23:45"), by="15 min" )
ndt <- length(dt)
shr <- zoo( rep(1, ndt), dt)
sfreq(shr)
#> [1] "minute"


## Ex2: hourly data 
## Loading the time series of HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
hr <- KarameaAtGorgeQts
sfreq(hr)
#> [1] "hourly"


## Ex3: Daily data
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
d <- OcaEnOnaQts
sfreq(d)
#> [1] "daily"


## Ex4: Monthly data
m <- daily2monthly(d, FUN=mean, na.rm=TRUE)
sfreq(m)
#> [1] "monthly"


## Ex5: Annual data
a <- daily2annual(d, FUN=mean, na.rm=TRUE, out.fmt="%Y-%m-%d")
sfreq(a)
#> [1] "annual"

```
