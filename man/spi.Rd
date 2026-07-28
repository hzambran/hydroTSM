%% File spi.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{spi}
\alias{spi}
\alias{spei}
\title{Standardized Precipitation and Precipitation-Evapotranspiration Indices}
\description{
Computes the Standardized Precipitation Index (SPI) or the Standardized
Precipitation-Evapotranspiration Index (SPEI) for one or more complete monthly
\code{zoo} series using base-R distribution and optimisation functions.
}
\usage{
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
}
\arguments{
  \item{x}{
  numeric \code{zoo} object containing one or more monthly precipitation series
  for \code{spi}, or climatic water-balance series (precipitation minus
  potential evapotranspiration) for \code{spei}. Its time index must inherit
  from \code{Date}, \code{POSIXt}, or \code{yearmon}, and it must contain one
  value for every consecutive month.
  }
  \item{scale}{
  positive integer indicating the number of months in the backward-looking
  accumulation period. This argument has no default and must be provided.
  }
  \item{distribution}{
  character string naming the distribution to fit. For \code{spi}, valid
  values are \code{"gamma"}, \code{"gumbel"}, \code{"logis"} (logistic),
  \code{"llogis"} (log-logistic), \code{"lnorm"} (lognormal),
  \code{"norm"}, and \code{"weibull"}. For \code{spei}, valid values are
  \code{"genlog"} (generalized logistic), \code{"gev"}, \code{"norm"}, and
  \code{"pe3"} (Pearson type III). These are the candidate distributions
  evaluated by Stagge et al. (2015). When omitted, the first value is used:
  \code{"gamma"} for SPI and \code{"genlog"} for SPEI.
  }
  \item{fit}{
  character string specifying the parameter-estimation method:
  \code{"max-lik"} for maximum likelihood, \code{"ub-pwm"} for unbiased
  probability-weighted moments, or \code{"pp-pwm"} for plotting-position
  probability-weighted moments. The default is \code{"max-lik"}.
  }
  \item{zero.threshold}{
  non-negative numeric value. For \code{spi}, precipitation values strictly
  below this threshold are changed to zero before temporal accumulation and
  distribution fitting. The default zero leaves all positive values unchanged.
  }
  \item{kernel}{
  list with elements \code{type} and \code{shift} defining the
  backward-looking accumulation kernel. Valid kernel types are
  \code{"rectangular"}, \code{"triangular"}, \code{"circular"}, and
  \code{"gaussian"}. \code{shift} must be an integer between zero and
  \code{scale - 1}. The default is an unshifted rectangular kernel.
  }
  \item{ref.start, ref.end}{
  optional \code{Date} objects or character strings in \code{"YYYY-MM"} or
  \code{"YYYY-MM-DD"} format defining the reference period used to estimate the
  distribution parameters. The day component, when supplied, is used only to
  identify the calendar month. By default, the full period in \code{x} is used.
  }
  \item{params}{
  optional numeric distribution parameters that override parameter fitting.
  A vector supplies one parameter set for every month and series; a matrix
  with dimensions \code{npar} by 12 supplies monthly parameters that are
  reused for every series; and an array with dimensions \code{npar} by
  \code{nseries} by 12 supplies parameters for every month and series, as in
  \pkg{SPEI}. Parameter names may be omitted when the documented order is
  used. The default \code{NULL} estimates parameters from \code{x}.
  }
  \item{start.fun}{
  optional function used to calculate initial parameters for maximum-likelihood
  estimation, following the interface used by \pkg{SCI}. It is called as
  \code{start.fun(x, distr)} for each calendar month and series and must return
  a numeric parameter vector. The default \code{NULL} uses hydroTSM's internal
  base-R starting-value estimator.
  }
  \item{start.fun.fix}{
  logical. If \code{TRUE} and maximum-likelihood optimisation fails, the
  initial parameters returned by \code{start.fun} (or by the internal
  estimator) are retained. If \code{FALSE}, the default, the affected month's
  parameters and drought-index values are set to \code{NA}.
  }
  \item{p0}{
  logical. If \code{TRUE}, the probability of zero values is represented by a
  mixed distribution. This is the default for SPI and is disabled by default
  for SPEI.
  }
  \item{p0.center.mass}{
  logical. If \code{TRUE}, the probability assigned to zero precipitation is
  estimated with the centre-of-probability-mass estimator described by
  Stagge et al. (2015).
  }
  \item{scaling}{
  character string indicating whether the accumulated precipitation is divided
  by its standard deviation (\code{"sd"}, the default), left unchanged
  (\code{"no"}), or divided by its maximum (\code{"max"}) before parameter
  estimation. Scaling can stabilise maximum-likelihood estimation.
  }
  \item{sci.limit}{
  non-negative numeric value used to truncate the absolute SPI or SPEI values.
  The default \code{Inf} disables truncation.
  }
  \item{na.rm}{
  logical. If \code{FALSE}, the default, \code{x} must not contain missing
  values. If \code{TRUE}, missing values are omitted during parameter fitting;
  accumulation windows containing a missing value remain \code{NA}.
  }
  \item{out.type}{
  character string indicating whether the result is returned as a
  \code{"zoo"} object (the default) or as a numeric vector or matrix.
  }
  \item{verbose}{
  logical indicating whether informative, bracketed progress messages are
  emitted with \code{\link{message}}. The default \code{FALSE} keeps package
  calls quiet.
  }
  \item{warn}{
  logical indicating whether warnings should be issued when parameter
  estimation or transformation fails for a calendar month.
  }
  \item{\dots}{
  additional maximum-likelihood fitting controls, notably
  \code{mledist.par}.
  }
}
\details{
The functions form a backward-looking accumulation at the selected \code{scale},
fit a separate distribution for each calendar month over the reference period,
evaluate the fitted cumulative probabilities, and transform them to
standard-normal variates. The SPI functions default to a Gamma distribution and
a mixed probability at zero. The SPEI functions default to the generalized
logistic distribution and retain negative climatic-water-balance values.

The seven SPI distributions and four SPEI distributions listed under
\code{distribution} follow Section 2.4 and Appendices B and C of Stagge et al.
(2015). Their density, cumulative-probability, L-moment conversion, and
maximum-likelihood calculations use only base R.

For \code{fit="ub-pwm"}, the first three probability-weighted moments use the
unbiased order-statistic estimators. For \code{fit="pp-pwm"}, non-exceedance
probabilities are \eqn{F_i=(i-0.35)/n}, corresponding to the plotting-position
constants \eqn{A=-0.35} and \eqn{B=0} used by \pkg{SPEI}. At least four
non-missing, non-constant calibration values are required for either PWM
method.

Accumulation is performed before the reference period is selected. Therefore,
an accumulated value at the start of the reference period can use preceding
observations from \code{x}. Kernel values are normalised and multiplied by
\code{scale}, following the magnitude-preserving convention used by the
\pkg{SPEI} package.

For SPI, \code{zero.threshold} is applied to individual monthly precipitation
values before accumulation. At scales larger than one, the zero probability is
therefore estimated from accumulated periods whose threshold-adjusted total is
zero.

Maximum-likelihood estimation uses \code{\link[stats]{optim}} and unbiased-PWM
starting values. Generalized-logistic, log-logistic, Pearson type III, GEV, and
Gumbel distribution functions are implemented internally. Neither \pkg{SCI},
\pkg{SPEI}, \pkg{lmomco}, nor \pkg{evd} is required.

When \code{params} is supplied, parameter fitting is skipped and the supplied
parameters are applied to the unscaled accumulated series; consequently,
\code{fit}, \code{scaling}, \code{start.fun}, and \code{start.fun.fix} do not
affect the result. When \code{p0=TRUE}, the zero probability is still estimated
separately for each calendar month from the reference data. The parameter
orders are:
\code{gamma=(shape, rate)}, \code{gumbel=(loc, scale)},
\code{logis=(location, scale)}, \code{llogis=(shape, scale)},
\code{lnorm=(meanlog, sdlog)}, \code{norm=(mean, sd)},
\code{weibull=(shape, scale)},
\code{genlog=(shape, scale, location)},
\code{gev=(loc, scale, shape)}, and
\code{pe3=(shape, scale, location)}.

\code{start.fun} is used only with \code{fit="max-lik"}. Its result can be
named, in which case the names must match those listed above, or unnamed in the
documented order. Retaining initial values with \code{start.fun.fix=TRUE} can
produce a complete index when optimisation fails, but the retained values are
starting estimates rather than a converged maximum-likelihood fit.
}
\value{
A \code{zoo} object by default, or a numeric vector or matrix when
\code{out.type="numeric"}. The result has the same time index, length, and
column names as \code{x}. The first \code{scale - 1} values are \code{NA} when
\code{scale > 1}.
}
\references{
McKee, T. B., Doesken, N. J., and Kleist, J. (1993). The relationship of
drought frequency and duration to time scales. \emph{Proceedings of the 8th
Conference on Applied Climatology}, 17--22 January, Anaheim, California,
179--184.

Stagge, J. H., Tallaksen, L. M., Gudmundsson, L., Van Loon, A. F., and Stahl,
K. (2015). Candidate distributions for climatological drought indices (SPI and
SPEI). \emph{International Journal of Climatology}, 35, 4027--4040.
\doi{10.1002/joc.4267}.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\seealso{
\code{\link{sfreq}}
}
\examples{
data(EbroPPtsMonthly)

pcp <- zoo(EbroPPtsMonthly$P9001, EbroPPtsMonthly$Date)
spi3 <- spi(pcp, scale=3, zero.threshold=0.1, warn=FALSE)
head(spi3)

spi3.pwm <- spi(pcp, scale=3, fit="ub-pwm", warn=FALSE)
head(spi3.pwm)

## Artificial monthly climatic water balance for illustrating 'spei'
pet <- zoo(60 + 40*sin(2*pi*(seq_along(pcp)-1)/12), time(pcp))
spei3 <- spei(pcp - pet, scale=3, warn=FALSE)
head(spei3)

## A supplied vector is reused for all calendar months
spei.fixed <- spei(pcp - pet, scale=1, distribution="norm",
                   params=c(mean=0, sd=50), warn=FALSE)
head(spei.fixed)
}
\keyword{ts}
\keyword{manip}
