# Visual Correlation Matrix

Visualization of a correlation matrix. On top the (absolute) value of
the correlation plus the result of the
[`cor.test`](https://rdrr.io/r/stats/cor.test.html) as stars. On bottom,
the bivariate scatterplots, with a fitted line. On the diagonal, an
histogram of each variable.

## Usage

``` r
hydropairs(x, dec = 3, use = "pairwise.complete.obs", method = "pearson",...)
```

## Arguments

- x:

  data.frame or matrix object with measurements at several locations.
  Each column of `x` represent values measured at different locations.

- dec:

  decimal places to be used for showing the correlation values

- use:

  See [`cor`](https://rdrr.io/r/stats/cor.html). An optional character
  string giving a method for computing covariances in the presence of
  missing values. This must be (an abbreviation of) one of the strings
  "everything", "all.obs", "complete.obs", "na.or.complete", or
  "pairwise.complete.obs".

- method:

  See [`cor`](https://rdrr.io/r/stats/cor.html). A character string
  indicating which correlation coefficient (or covariance) is to be
  computed. One of "pearson" (default), "kendall", or "spearman", can be
  abbreviated

- ...:

  further arguments passed to or from other methods, in particular it is
  used in the [`pairs`](https://rdrr.io/r/graphics/pairs.html) function.

## Value

- On top:

  the (absolute) value of the correlation plus the result of the
  cor.test as points

- On bottom:

  the bivariate scatterplots, with a fitted line

- On diagonal:

  histograms (from [`pairs`](https://rdrr.io/r/graphics/pairs.html))

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail>

## Note

Original idea taken from the R Graph Gallery (nowadays not available on
its original link:
http://addictedtor.free.fr/graphiques/graphcode.php?graph=137).  

Histogram panel was taken from the R help of the original
[`pairs`](https://rdrr.io/r/graphics/pairs.html) function

## See also

[`cor`](https://rdrr.io/r/stats/cor.html),
[`pairs`](https://rdrr.io/r/graphics/pairs.html)

## Examples

``` r
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Visualizing the correlation among the monthly precipitation values 
## of the first 3 gauging stations in 'EbroPPtsMonthly'. 
## The first column of 'EbroPPtsMonthly' has the dates.
hydropairs(EbroPPtsMonthly[,2:4])
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
```
