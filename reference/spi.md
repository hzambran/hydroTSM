# Standardized Precipitation and Precipitation-Evapotranspiration Indices

Computes the Standardized Precipitation Index (SPI) or the Standardized
Precipitation-Evapotranspiration Index (SPEI) for one or more complete
monthly `zoo` series using base-R distribution and optimisation
functions.

## Usage

``` r
spi(x, scale,
    distribution=c("gamma", "gumbel", "logis", "llogis",
                   "lnorm", "norm", "weibull"),
    fit=c("max-lik", "ub-pwm", "pp-pwm"), zero.threshold=0,
    kernel=list(type="rectangular", shift=0), ref.start=NULL, ref.end=NULL,
    params=NULL, start.fun=NULL, start.fun.fix=FALSE,
    p0=TRUE, p0.center.mass=FALSE, scaling=c("sd", "no", "max"),
    sci.limit=Inf, na.rm=FALSE, out.type=c("zoo", "numeric"),
    verbose=FALSE, warn=TRUE, ...)

spei(x, scale, distribution=c("genlog", "gev", "norm", "pe3"),
     fit=c("max-lik", "ub-pwm", "pp-pwm"),
     kernel=list(type="rectangular", shift=0), ref.start=NULL, ref.end=NULL,
     params=NULL, start.fun=NULL, start.fun.fix=FALSE,
     p0=FALSE, p0.center.mass=FALSE, scaling=c("sd", "no", "max"),
     sci.limit=Inf, na.rm=FALSE, out.type=c("zoo", "numeric"),
     verbose=FALSE, warn=TRUE, ...)
```

## Arguments

- x:

  numeric `zoo` object containing one or more monthly precipitation
  series for `spi`, or climatic water-balance series (precipitation
  minus potential evapotranspiration) for `spei`. Its time index must
  inherit from `Date`, `POSIXt`, or `yearmon`, and it must contain one
  value for every consecutive month.

- scale:

  positive integer indicating the number of months in the
  backward-looking accumulation period. This argument has no default and
  must be provided.

- distribution:

  character string naming the distribution to fit. For `spi`, valid
  values are `"gamma"`, `"gumbel"`, `"logis"` (logistic), `"llogis"`
  (log-logistic), `"lnorm"` (lognormal), `"norm"`, and `"weibull"`. For
  `spei`, valid values are `"genlog"` (generalized logistic), `"gev"`,
  `"norm"`, and `"pe3"` (Pearson type III). These are the candidate
  distributions evaluated by Stagge et al. (2015). When omitted, the
  first value is used: `"gamma"` for SPI and `"genlog"` for SPEI.

- fit:

  character string specifying the parameter-estimation method:
  `"max-lik"` for maximum likelihood, `"ub-pwm"` for unbiased
  probability-weighted moments, or `"pp-pwm"` for plotting-position
  probability-weighted moments. The default is `"max-lik"`.

- zero.threshold:

  non-negative numeric value. For `spi`, precipitation values strictly
  below this threshold are changed to zero before temporal accumulation
  and distribution fitting. The default zero leaves all positive values
  unchanged.

- kernel:

  list with elements `type` and `shift` defining the backward-looking
  accumulation kernel. Valid kernel types are `"rectangular"`,
  `"triangular"`, `"circular"`, and `"gaussian"`. `shift` must be an
  integer between zero and `scale - 1`. The default is an unshifted
  rectangular kernel.

- ref.start, ref.end:

  optional `Date` objects or character strings in `"YYYY-MM"` or
  `"YYYY-MM-DD"` format defining the reference period used to estimate
  the distribution parameters. The day component, when supplied, is used
  only to identify the calendar month. By default, the full period in
  `x` is used.

- params:

  optional numeric distribution parameters that override parameter
  fitting. A vector supplies one parameter set for every month and
  series; a matrix with dimensions `npar` by 12 supplies monthly
  parameters that are reused for every series; and an array with
  dimensions `npar` by `nseries` by 12 supplies parameters for every
  month and series, as in SPEI. Parameter names may be omitted when the
  documented order is used. The default `NULL` estimates parameters from
  `x`.

- start.fun:

  optional function used to calculate initial parameters for
  maximum-likelihood estimation, following the interface used by SCI. It
  is called as `start.fun(x, distr)` for each calendar month and series
  and must return a numeric parameter vector. The default `NULL` uses
  hydroTSM's internal base-R starting-value estimator.

- start.fun.fix:

  logical. If `TRUE` and maximum-likelihood optimisation fails, the
  initial parameters returned by `start.fun` (or by the internal
  estimator) are retained. If `FALSE`, the default, the affected month's
  parameters and drought-index values are set to `NA`.

- p0:

  logical. If `TRUE`, the probability of zero values is represented by a
  mixed distribution. This is the default for SPI and is disabled by
  default for SPEI.

- p0.center.mass:

  logical. If `TRUE`, the probability assigned to zero precipitation is
  estimated with the centre-of-probability-mass estimator described by
  Stagge et al. (2015).

- scaling:

  character string indicating whether the accumulated precipitation is
  divided by its standard deviation (`"sd"`, the default), left
  unchanged (`"no"`), or divided by its maximum (`"max"`) before
  parameter estimation. Scaling can stabilise maximum-likelihood
  estimation.

- sci.limit:

  non-negative numeric value used to truncate the absolute SPI or SPEI
  values. The default `Inf` disables truncation.

- na.rm:

  logical. If `FALSE`, the default, `x` must not contain missing values.
  If `TRUE`, missing values are omitted during parameter fitting;
  accumulation windows containing a missing value remain `NA`.

- out.type:

  character string indicating whether the result is returned as a
  `"zoo"` object (the default) or as a numeric vector or matrix.

- verbose:

  logical indicating whether informative, bracketed progress messages
  are emitted with [`message`](https://rdrr.io/r/base/message.html). The
  default `FALSE` keeps package calls quiet.

- warn:

  logical indicating whether warnings should be issued when parameter
  estimation or transformation fails for a calendar month.

- ...:

  additional maximum-likelihood fitting controls, notably `mledist.par`.

## Details

The functions form a backward-looking accumulation at the selected
`scale`, fit a separate distribution for each calendar month over the
reference period, evaluate the fitted cumulative probabilities, and
transform them to standard-normal variates. The SPI functions default to
a Gamma distribution and a mixed probability at zero. The SPEI functions
default to the generalized logistic distribution and retain negative
climatic-water-balance values.

The seven SPI distributions and four SPEI distributions listed under
`distribution` follow Section 2.4 and Appendices B and C of Stagge et
al. (2015). Their density, cumulative-probability, L-moment conversion,
and maximum-likelihood calculations use only base R.

For `fit="ub-pwm"`, the first three probability-weighted moments use the
unbiased order-statistic estimators. For `fit="pp-pwm"`, non-exceedance
probabilities are \\F_i=(i-0.35)/n\\, corresponding to the
plotting-position constants \\A=-0.35\\ and \\B=0\\ used by SPEI. At
least four non-missing, non-constant calibration values are required for
either PWM method.

Accumulation is performed before the reference period is selected.
Therefore, an accumulated value at the start of the reference period can
use preceding observations from `x`. Kernel values are normalised and
multiplied by `scale`, following the magnitude-preserving convention
used by the SPEI package.

For SPI, `zero.threshold` is applied to individual monthly precipitation
values before accumulation. At scales larger than one, the zero
probability is therefore estimated from accumulated periods whose
threshold-adjusted total is zero.

Maximum-likelihood estimation uses
[`optim`](https://rdrr.io/r/stats/optim.html) and unbiased-PWM starting
values. Generalized-logistic, log-logistic, Pearson type III, GEV, and
Gumbel distribution functions are implemented internally. Neither SCI,
SPEI, lmomco, nor evd is required.

When `params` is supplied, parameter fitting is skipped and the supplied
parameters are applied to the unscaled accumulated series; consequently,
`fit`, `scaling`, `start.fun`, and `start.fun.fix` do not affect the
result. When `p0=TRUE`, the zero probability is still estimated
separately for each calendar month from the reference data. The
parameter orders are: `gamma=(shape, rate)`, `gumbel=(loc, scale)`,
`logis=(location, scale)`, `llogis=(shape, scale)`,
`lnorm=(meanlog, sdlog)`, `norm=(mean, sd)`, `weibull=(shape, scale)`,
`genlog=(shape, scale, location)`, `gev=(loc, scale, shape)`, and
`pe3=(shape, scale, location)`.

`start.fun` is used only with `fit="max-lik"`. Its result can be named,
in which case the names must match those listed above, or unnamed in the
documented order. Retaining initial values with `start.fun.fix=TRUE` can
produce a complete index when optimisation fails, but the retained
values are starting estimates rather than a converged maximum-likelihood
fit.

## Value

A `zoo` object by default, or a numeric vector or matrix when
`out.type="numeric"`. The result has the same time index, length, and
column names as `x`. The first `scale - 1` values are `NA` when
`scale > 1`.

## References

McKee, T. B., Doesken, N. J., and Kleist, J. (1993). The relationship of
drought frequency and duration to time scales. *Proceedings of the 8th
Conference on Applied Climatology*, 17–22 January, Anaheim, California,
179–184.

Stagge, J. H., Tallaksen, L. M., Gudmundsson, L., Van Loon, A. F., and
Stahl, K. (2015). Candidate distributions for climatological drought
indices (SPI and SPEI). *International Journal of Climatology*, 35,
4027–4040. [doi:10.1002/joc.4267](https://doi.org/10.1002/joc.4267) .

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`sfreq`](https://hzambran.github.io/hydroTSM/reference/sfreq.md)

## Examples

``` r
data(EbroPPtsMonthly)

pcp <- zoo(EbroPPtsMonthly$P9001, EbroPPtsMonthly$Date)
spi3 <- spi(pcp, scale=3, zero.threshold=0.1, warn=FALSE)
head(spi3)
#> 1941-01-01 1941-02-01 1941-03-01 1941-04-01 1941-05-01 1941-06-01 
#>         NA         NA  2.0201463  0.5939333  0.6792742  0.6119406 

spi3.pwm <- spi(pcp, scale=3, fit="ub-pwm", warn=FALSE)
head(spi3.pwm)
#> 1941-01-01 1941-02-01 1941-03-01 1941-04-01 1941-05-01 1941-06-01 
#>         NA         NA  1.8218222  0.6300705  0.6572432  0.5861024 

## Artificial monthly climatic water balance for illustrating 'spei'
pet <- zoo(60 + 40*sin(2*pi*(seq_along(pcp)-1)/12), time(pcp))
spei3 <- spei(pcp - pet, scale=3, warn=FALSE)
head(spei3)
#> 1941-01-01 1941-02-01 1941-03-01 1941-04-01 1941-05-01 1941-06-01 
#>         NA         NA  1.6905972  0.3952166  0.6237190  0.4519695 

## A supplied vector is reused for all calendar months
spei.fixed <- spei(pcp - pet, scale=1, distribution="norm",
                   params=c(mean=0, sd=50), warn=FALSE)
head(spei.fixed)
#>  1941-01-01  1941-02-01  1941-03-01  1941-04-01  1941-05-01  1941-06-01 
#>  5.03200000  1.57800000 -0.06882032 -1.21000000  0.81317968 -0.76600000 
```
