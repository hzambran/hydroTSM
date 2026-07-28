library(hydroTSM)

set.seed(123)
dates <- seq(as.Date("1980-01-01"), by="month", length.out=480)
precipitation <- stats::rgamma(length(dates), shape=2, rate=0.05)
precipitation[c(1, 13, 25, 37)] <- c(0, 0.05, 0.1, 0.5)
pcp <- zoo::zoo(precipitation, dates)

missing.spi.scale <- tryCatch(spi(pcp), error=function(e) e)
missing.spei.scale <- tryCatch(spei(pcp - 40), error=function(e) e)
stopifnot(inherits(missing.spi.scale, "error"),
          inherits(missing.spei.scale, "error"),
          grepl("'scale' must be provided",
                conditionMessage(missing.spi.scale), fixed=TRUE),
          grepl("'scale' must be provided",
                conditionMessage(missing.spei.scale), fixed=TRUE))

threshold.adjusted <- pcp
threshold.adjusted[zoo::coredata(threshold.adjusted) < 0.1] <- 0

threshold.result <- spi(pcp, scale=3, zero.threshold=0.1, warn=FALSE)
adjusted.result <- spi(threshold.adjusted, scale=3, warn=FALSE)

stopifnot(isTRUE(all.equal(threshold.result, adjusted.result)))

for (scale in c(1L, 3L, 6L, 12L)) {
  spi.result <- spi(pcp, scale=scale, warn=FALSE)
  spei.result <- spei(pcp - 40, scale=scale, warn=FALSE)

  stopifnot(zoo::is.zoo(spi.result),
            zoo::is.zoo(spei.result),
            length(spi.result) == length(pcp),
            length(spei.result) == length(pcp),
            sum(is.finite(spi.result)) >= length(pcp) - scale,
            sum(is.finite(spei.result)) >= length(pcp) - scale)
}

for (kernel.type in c("rectangular", "triangular", "circular", "gaussian")) {
  result <- spei(pcp - 40, scale=6,
                 kernel=list(type=kernel.type, shift=1),
                 warn=FALSE)
  stopifnot(length(result) == length(pcp),
            sum(is.finite(result)) >= length(pcp) - 6)
}

spi.distributions <- c("gamma", "gumbel", "logis", "llogis", "lnorm",
                       "norm", "weibull")
spei.distributions <- c("genlog", "gev", "norm", "pe3")
missing.scale.formal <- formals(function(scale) NULL)["scale"]

stopifnot(identical(eval(formals(spi)$distribution), spi.distributions),
          identical(eval(formals(spei)$distribution), spei.distributions),
          identical(formals(spi)["scale"], missing.scale.formal),
          identical(formals(spei)["scale"], missing.scale.formal),
          is.null(formals(spi)$params),
          is.null(formals(spei)$params),
          is.null(formals(spi)$start.fun),
          is.null(formals(spei)$start.fun),
          identical(formals(spi)$start.fun.fix, FALSE),
          identical(formals(spei)$start.fun.fix, FALSE),
          identical(formals(spi)$verbose, FALSE),
          identical(formals(spei)$verbose, FALSE))

for (distribution in spi.distributions) {
  for (fit in c("max-lik", "ub-pwm", "pp-pwm")) {
    result <- spi(pcp, scale=3, distribution=distribution, fit=fit,
                  sci.limit=3, warn=FALSE)
    stopifnot(length(result) == length(pcp),
              sum(is.finite(result)) >= length(pcp) - 3)
  }
}

for (distribution in spei.distributions) {
  for (fit in c("max-lik", "ub-pwm", "pp-pwm")) {
    result <- spei(pcp - 40, scale=3, distribution=distribution, fit=fit,
                   sci.limit=3, warn=FALSE)
    stopifnot(length(result) == length(pcp),
              sum(is.finite(result)) >= length(pcp) - 3)
  }
}

stopifnot(inherits(try(spi(pcp, scale=3, distribution="genlog"),
                         silent=TRUE), "try-error"),
          inherits(try(spei(pcp - 40, scale=3, distribution="gamma"),
                         silent=TRUE), "try-error"))

for (scaling in c("sd", "no", "max")) {
  result <- spi(pcp, scale=3, scaling=scaling, warn=FALSE)
  stopifnot(length(result) == length(pcp),
            sum(is.finite(result)) >= length(pcp) - 3)
}

limited.result <- spi(pcp, scale=1, zero.threshold=0.1,
                      p0.center.mass=TRUE, sci.limit=2, warn=FALSE)
stopifnot(all(abs(limited.result[is.finite(limited.result)]) <= 2))

reference.result <- spei(pcp - 40, scale=3,
                         ref.start="1990-01", ref.end="2010-12",
                         warn=FALSE)
reference.result.ymd <- spei(pcp - 40, scale=3,
                             ref.start="1990-01-01",
                             ref.end="2010-12-01",
                             warn=FALSE)
reference.result.date <- spei(pcp - 40, scale=3,
                              ref.start=as.Date("1990-01-01"),
                              ref.end=as.Date("2010-12-31"),
                              warn=FALSE)
old.reference.result <- try(
  spei(pcp - 40, scale=3, ref.start=c(1990, 1), ref.end="2010-12",
       warn=FALSE),
  silent=TRUE
)
stopifnot(length(reference.result) == length(pcp),
          isTRUE(all.equal(reference.result, reference.result.ymd)),
          isTRUE(all.equal(reference.result, reference.result.date)),
          inherits(old.reference.result, "try-error"),
          sum(is.finite(reference.result)) >= length(pcp) - 3)

pcp.with.na <- pcp
pcp.with.na[100] <- NA_real_
missing.result <- spi(pcp.with.na, scale=3, na.rm=TRUE, warn=FALSE)
stopifnot(length(missing.result) == length(pcp),
          is.na(missing.result[100]))

pcp.matrix <- zoo::merge.zoo(first=pcp, second=1.1 * pcp)
matrix.result <- spi(pcp.matrix, scale=3, warn=FALSE)

stopifnot(identical(dim(matrix.result), dim(pcp.matrix)),
          identical(colnames(matrix.result), colnames(pcp.matrix)),
          is.numeric(spi(pcp, scale=3, out.type="numeric", warn=FALSE)))

balance <- pcp - 40

fixed.vector <- spei(balance, scale=1, distribution="norm",
                     fit="max-lik", scaling="max",
                     params=c(sd=20, mean=0),
                     start.fun=function(...) stop("must not be called"),
                     warn=FALSE)
stopifnot(isTRUE(all.equal(zoo::coredata(fixed.vector),
                          zoo::coredata(balance) / 20,
                          tolerance=1e-7)))

fixed.monthly.params <- rbind(mean=-11:0, sd=rep(20, 12))
fixed.monthly <- spei(balance, scale=1, distribution="norm",
                      params=fixed.monthly.params, warn=FALSE)
month.number <- as.integer(format(zoo::index(balance), "%m"))
fixed.monthly.expected <- (zoo::coredata(balance) -
                           fixed.monthly.params["mean", month.number]) / 20
stopifnot(isTRUE(all.equal(zoo::coredata(fixed.monthly),
                          fixed.monthly.expected,
                          tolerance=1e-7)))

fixed.array.params <- array(NA_real_, dim=c(2, 2, 12),
                            dimnames=list(c("mean", "sd"),
                                          c("first", "second"), NULL))
fixed.array.params["mean", , ] <- rbind(rep(0, 12), rep(5, 12))
fixed.array.params["sd", , ] <- 20
balance.matrix <- zoo::merge.zoo(first=balance, second=balance)
fixed.array <- spei(balance.matrix, scale=1, distribution="norm",
                    params=fixed.array.params, warn=FALSE)
fixed.array.expected <- cbind(zoo::coredata(balance) / 20,
                              (zoo::coredata(balance) - 5) / 20)
colnames(fixed.array.expected) <- c("first", "second")
stopifnot(isTRUE(all.equal(zoo::coredata(fixed.array),
                          fixed.array.expected,
                          tolerance=1e-7)))

start.calls <- 0L
monthly.start <- function(x, distr) {
  start.calls <<- start.calls + 1L
  c(mean=mean(x), sd=stats::sd(x))
}
custom.start.result <- spei(balance, scale=1, distribution="norm",
                            start.fun=monthly.start, warn=FALSE)
stopifnot(start.calls == 12L,
          sum(is.finite(custom.start.result)) == length(balance))

failed.optim <- function(par, fn, ...)
  list(par=par, convergence=1L)

fixed.start.result <- spei(
  balance, scale=1, distribution="norm",
  start.fun=monthly.start, start.fun.fix=TRUE,
  mledist.par=list(custom.optim=failed.optim), warn=FALSE
)
failed.start.result <- spei(
  balance, scale=1, distribution="norm",
  start.fun=monthly.start, start.fun.fix=FALSE,
  mledist.par=list(custom.optim=failed.optim), warn=FALSE
)
stopifnot(sum(is.finite(fixed.start.result)) == length(balance),
          all(is.na(failed.start.result)))

verbose.messages <- character()
withCallingHandlers(
  spei(balance, scale=1, distribution="norm",
       params=c(mean=0, sd=20), verbose=TRUE, warn=FALSE),
  message=function(m) {
    verbose.messages <<- c(verbose.messages, trimws(conditionMessage(m)))
    invokeRestart("muffleMessage")
  }
)
stopifnot(length(verbose.messages) >= 4L,
          all(grepl("^\\[.*\\]$", verbose.messages)))

spi.fixed.parameters <- list(
  gamma=c(shape=2, rate=0.05),
  gumbel=c(loc=20, scale=10),
  logis=c(location=20, scale=10),
  llogis=c(shape=2, scale=20),
  lnorm=c(meanlog=3, sdlog=0.5),
  norm=c(mean=20, sd=10),
  weibull=c(shape=2, scale=20)
)
spei.fixed.parameters <- list(
  genlog=c(shape=0.1, scale=20, location=0),
  gev=c(loc=0, scale=20, shape=0.1),
  norm=c(mean=0, sd=20),
  pe3=c(shape=0.1, scale=20, location=0)
)

for (distribution in names(spi.fixed.parameters)) {
  result <- spi(pcp, scale=1, distribution=distribution,
                params=spi.fixed.parameters[[distribution]],
                sci.limit=3, warn=FALSE)
  stopifnot(length(result) == length(pcp),
            all(is.finite(result)))
}

for (distribution in names(spei.fixed.parameters)) {
  result <- spei(balance, scale=1, distribution=distribution,
                 params=spei.fixed.parameters[[distribution]],
                 sci.limit=3, warn=FALSE)
  stopifnot(length(result) == length(balance),
            all(is.finite(result)))
}

stopifnot(
  inherits(try(spei(balance, scale=1, distribution="norm", params=c(mean=0)),
               silent=TRUE), "try-error"),
  inherits(try(spei(balance, scale=1, distribution="norm",
                    params=c(mean=0, sd=-1)),
               silent=TRUE), "try-error"),
  inherits(try(spei(balance, scale=1, distribution="norm",
                    params=matrix(c(0, 1), nrow=2, ncol=1)),
               silent=TRUE), "try-error"),
  inherits(try(spei(balance, scale=1, distribution="norm",
                    params=c(location=0, scale=1)),
               silent=TRUE), "try-error")
)

sample.lmoments <- getFromNamespace(".sampleDroughtLmoments", "hydroTSM")
manual.lmoments <- function(x, fit) {
  x <- sort(x)
  n <- length(x)
  rank <- seq_len(n)

  if (fit == "ub-pwm") {
    beta1 <- mean(x * (rank - 1) / (n - 1))
    beta2 <- mean(x * (rank - 1) * (rank - 2) /
                  ((n - 1) * (n - 2)))
  } else {
      probability <- (rank - 0.35) / n
      beta1 <- mean(x * probability)
      beta2 <- mean(x * probability^2)
    }

  beta0 <- mean(x)
  l2 <- 2 * beta1 - beta0
  l3 <- 6 * beta2 - 6 * beta1 + beta0
  c(L1=beta0, L2=l2, T3=l3 / l2)
}

stopifnot(isTRUE(all.equal(
            sample.lmoments(zoo::coredata(pcp), "ub-pwm"),
            manual.lmoments(zoo::coredata(pcp), "ub-pwm"),
            tolerance=1e-12)),
          isTRUE(all.equal(
            sample.lmoments(zoo::coredata(pcp), "pp-pwm"),
            manual.lmoments(zoo::coredata(pcp), "pp-pwm"),
            tolerance=1e-12)))

dllogis <- getFromNamespace(".dllogisDrought", "hydroTSM")
pllogis <- getFromNamespace(".pllogisDrought", "hydroTSM")
pwm.parameters <- getFromNamespace(".pwmParametersBase", "hydroTSM")
q <- c(0, 0.5, 1, 2, 10)
llogis.l1 <- 2 * (pi / 3) / sin(pi / 3)

stopifnot(isTRUE(all.equal(
            pllogis(q, shape=3, scale=2),
            c(0, stats::plogis(log(q[-1]), location=log(2), scale=1 / 3)),
            tolerance=1e-14)),
          abs(stats::integrate(function(x)
                dllogis(x, shape=3, scale=2),
              lower=0, upper=Inf)$value - 1) < 1e-8,
          isTRUE(all.equal(
            pwm.parameters(c(L1=4, L2=2, T3=0), "logis"),
            c(location=4, scale=2),
            tolerance=1e-14)),
          isTRUE(all.equal(
            pwm.parameters(c(L1=llogis.l1, L2=llogis.l1 / 3,
                             T3=1 / 3), "llogis"),
            c(shape=3, scale=2),
            tolerance=1e-14)))
