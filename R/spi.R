# File spi.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# spi/spei: Standardized drought indices for monthly zoo objects               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 26-Jul-2026                                                         #
# Updates:                                                                     #
################################################################################

spi <- function(x,
                scale=12,
                distribution=c("gamma", "gumbel", "logis", "llogis",
                               "lnorm", "norm", "weibull"),
                fit=c("max-lik", "ub-pwm", "pp-pwm"),
                zero.threshold=0,
                kernel=list(type="rectangular", shift=0),
                ref.start=NULL,
                ref.end=NULL,
                params=NULL,
                start.fun=NULL,
                start.fun.fix=FALSE,
                p0=TRUE,
                p0.center.mass=FALSE,
                scaling=c("sd", "no", "max"),
                sci.limit=Inf,
                na.rm=FALSE,
                out.type=c("zoo", "numeric"),
                verbose=FALSE,
                warn=TRUE,
                ...) {

  .droughtIndex(x=x,
                index.name="SPI",
                scale=scale,
                distribution=distribution,
                fit=fit,
                zero.threshold=zero.threshold,
                kernel=kernel,
                ref.start=ref.start,
                ref.end=ref.end,
                params=params,
                start.fun=start.fun,
                start.fun.fix=start.fun.fix,
                p0=p0,
                p0.center.mass=p0.center.mass,
                scaling=scaling,
                sci.limit=sci.limit,
                na.rm=na.rm,
                out.type=out.type,
                verbose=verbose,
                warn=warn,
                ...)

} # 'spi' END


spei <- function(x,
                 scale=12,
                 distribution=c("genlog", "gev", "norm", "pe3"),
                 fit=c("max-lik", "ub-pwm", "pp-pwm"),
                 kernel=list(type="rectangular", shift=0),
                 ref.start=NULL,
                 ref.end=NULL,
                 params=NULL,
                 start.fun=NULL,
                 start.fun.fix=FALSE,
                 p0=FALSE,
                 p0.center.mass=FALSE,
                 scaling=c("sd", "no", "max"),
                 sci.limit=Inf,
                 na.rm=FALSE,
                 out.type=c("zoo", "numeric"),
                 verbose=FALSE,
                 warn=TRUE,
                 ...) {

  .droughtIndex(x=x,
                index.name="SPEI",
                scale=scale,
                distribution=distribution,
                fit=fit,
                zero.threshold=NULL,
                kernel=kernel,
                ref.start=ref.start,
                ref.end=ref.end,
                params=params,
                start.fun=start.fun,
                start.fun.fix=start.fun.fix,
                p0=p0,
                p0.center.mass=p0.center.mass,
                scaling=scaling,
                sci.limit=sci.limit,
                na.rm=na.rm,
                out.type=out.type,
                verbose=verbose,
                warn=warn,
                ...)

} # 'spei' END


.droughtIndex <- function(x,
                          index.name,
                          scale,
                          distribution,
                          fit,
                          zero.threshold,
                          kernel,
                          ref.start,
                          ref.end,
                          params,
                          start.fun,
                          start.fun.fix,
                          p0,
                          p0.center.mass,
                          scaling,
                          sci.limit,
                          na.rm,
                          out.type,
                          verbose,
                          warn,
                          ...) {

  if (missing(x))
    stop("Missing argument: 'x' must be provided !")

  if (!zoo::is.zoo(x))
    stop("Invalid argument: 'x' must be a 'zoo' object !")

  if (length(x) == 0L)
    stop("Invalid argument: 'x' must contain at least one value !")

  if (length(scale) != 1L || !is.numeric(scale) || !is.finite(scale) ||
      scale < 1 || scale != as.integer(scale))
    stop("Invalid argument: 'scale' must be a positive integer !")

  scale <- as.integer(scale)

  if (!is.character(distribution) || length(distribution) == 0L ||
      anyNA(distribution))
    stop("Invalid argument: 'distribution' must contain character strings !")

  valid.distributions <- if (index.name == "SPI") {
    c("gamma", "gumbel", "logis", "llogis", "lnorm", "norm", "weibull")
  } else {
      c("genlog", "gev", "norm", "pe3")
    }

  distribution <- match.arg(distribution, valid.distributions)
  fit <- match.arg(fit, c("max-lik", "ub-pwm", "pp-pwm"))
  scaling <- match.arg(scaling, c("sd", "no", "max"))
  out.type <- match.arg(out.type, c("zoo", "numeric"))

  if (!is.list(kernel))
    stop("Invalid argument: 'kernel' must be a list !")

  kernel.type <- kernel$type
  if (is.null(kernel.type))
    kernel.type <- "rectangular"

  if (length(kernel.type) != 1L || !is.character(kernel.type))
    stop("Invalid argument: 'kernel$type' must be a character string !")

  kernel.type <- match.arg(kernel.type,
                           c("rectangular", "triangular", "circular",
                             "gaussian"))

  kernel.shift <- kernel$shift
  if (is.null(kernel.shift))
    kernel.shift <- 0

  if (length(kernel.shift) != 1L || !is.numeric(kernel.shift) ||
      !is.finite(kernel.shift) || kernel.shift < 0 ||
      kernel.shift != as.integer(kernel.shift) || kernel.shift >= scale)
    stop("Invalid argument: 'kernel$shift' must be an integer in ",
         "[0, scale-1] !")

  kernel.shift <- as.integer(kernel.shift)

  if (length(p0) != 1L || !is.logical(p0) || is.na(p0))
    stop("Invalid argument: 'p0' must be TRUE or FALSE !")

  if (length(p0.center.mass) != 1L || !is.logical(p0.center.mass) ||
      is.na(p0.center.mass))
    stop("Invalid argument: 'p0.center.mass' must be TRUE or FALSE !")

  if (!is.null(start.fun) && !is.function(start.fun))
    stop("Invalid argument: 'start.fun' must be NULL or a function !")

  if (length(start.fun.fix) != 1L || !is.logical(start.fun.fix) ||
      is.na(start.fun.fix))
    stop("Invalid argument: 'start.fun.fix' must be TRUE or FALSE !")

  if (length(na.rm) != 1L || !is.logical(na.rm) || is.na(na.rm))
    stop("Invalid argument: 'na.rm' must be TRUE or FALSE !")

  if (length(verbose) != 1L || !is.logical(verbose) || is.na(verbose))
    stop("Invalid argument: 'verbose' must be TRUE or FALSE !")

  if (length(warn) != 1L || !is.logical(warn) || is.na(warn))
    stop("Invalid argument: 'warn' must be TRUE or FALSE !")

  if (length(sci.limit) != 1L || !is.numeric(sci.limit) ||
      is.na(sci.limit) || sci.limit < 0)
    stop("Invalid argument: 'sci.limit' must be a non-negative number !")

  dates <- zoo::index(x)
  valid.date.class <- inherits(dates, "Date") ||
                      inherits(dates, "POSIXt") ||
                      inherits(dates, "yearmon")

  if (!valid.date.class)
    stop("Invalid argument: the time index of 'x' must inherit from ",
         "'Date', 'POSIXt' or 'yearmon' !")

  months    <- zoo::as.yearmon(dates)
  month.ids <- as.integer(round(12 * as.numeric(months)))

  if (length(month.ids) < 2L)
    stop("Invalid argument: 'x' must contain at least two monthly values !")

  if (any(diff(month.ids) != 1L))
    stop("Invalid argument: 'x' must have one value for every consecutive month !")

  x.data      <- zoo::coredata(x)
  x.is.vector <- is.null(dim(x.data))

  if (!is.numeric(x.data))
    stop("Invalid argument: the values in 'x' must be numeric !")

  x.matrix <- as.matrix(x.data)
  storage.mode(x.matrix) <- "double"
  provided.params <- .normaliseDroughtParams(
    params=params,
    distr=distribution,
    n.series=ncol(x.matrix),
    series.names=colnames(x.matrix)
  )

  if (scale > nrow(x.matrix))
    stop("Invalid argument: 'scale' can not be larger than the number of ",
         "monthly values in 'x' !")

  if (any(!is.finite(x.matrix) & !is.na(x.matrix)))
    stop("Invalid argument: the values in 'x' must be finite or NA !")

  if (index.name == "SPI") {
    if (length(zero.threshold) != 1L || !is.numeric(zero.threshold) ||
        !is.finite(zero.threshold) || zero.threshold < 0)
      stop("Invalid argument: 'zero.threshold' must be a non-negative number !")

    if (any(x.matrix < 0, na.rm=TRUE))
      stop("Invalid argument: precipitation values in 'x' can not be negative !")

    x.matrix[x.matrix < zero.threshold] <- 0
  } # IF end

  if (!na.rm && anyNA(x.matrix))
    stop("Invalid argument: 'x' can not contain NA values when 'na.rm=FALSE' !")

  check.ref <- function(ref, ref.name, default.id) {
    if (is.null(ref))
      return(default.id)

    if (!is.numeric(ref) || length(ref) != 2L || any(!is.finite(ref)) ||
        any(ref != as.integer(ref)) || ref[2] < 1 || ref[2] > 12)
      stop("Invalid argument: '", ref.name,
           "' must be an integer vector of the form c(year, month) !")

    as.integer(12 * ref[1] + ref[2] - 1)
  } # 'check.ref' END

  ref.start.id <- check.ref(ref.start, "ref.start", month.ids[1])
  ref.end.id   <- check.ref(ref.end, "ref.end", month.ids[length(month.ids)])

  if (ref.start.id > ref.end.id)
    stop("Invalid arguments: 'ref.start' must not be later than 'ref.end' !")

  if (ref.start.id < month.ids[1] ||
      ref.end.id > month.ids[length(month.ids)])
    stop("Invalid arguments: 'ref.start' and 'ref.end' must be within ",
         "the time period of 'x' !")

  ref <- month.ids >= ref.start.id & month.ids <= ref.end.id

  fitting.method <- if (is.null(provided.params)) {
    paste0("fit=", fit)
  } else {
      "using user-supplied parameters"
    }

  .droughtMessage(
    verbose=verbose,
    index.name=index.name,
    text=paste0("scale=", scale, "; distribution=", distribution,
                "; ", fitting.method, "; kernel=", kernel.type,
                "; shift=", kernel.shift)
  )

  if (!is.null(provided.params))
    .droughtMessage(
      verbose=verbose,
      index.name=index.name,
      text="parameter fitting and input scaling are disabled by 'params'"
    )

  .droughtMessage(
    verbose=verbose,
    index.name=index.name,
    text=paste0("reference period ",
                format(months[which(ref)[1]], "%Y-%m"), " to ",
                format(months[utils::tail(which(ref), 1)], "%Y-%m"))
  )

  # SPEI first accumulates the full series and then selects the reference period.
  # Multiplication by 'scale' makes rectangular weights equal to one and follows
  # the magnitude-preserving kernel convention used by SPEI::spi/SPEI::spei.
  kernel.values <- if (scale == 1L) {
    1
  } else {
      switch(kernel.type,
             rectangular=rep(1, scale),
             triangular=scale:1,
             circular=scale^2 + 1 - (1:scale)^2,
             gaussian=exp(-seq(0, -3, length.out=scale)^2 / 2))
    } # ELSE end

  kernel.values <- kernel.values / sum(kernel.values)

  if (kernel.shift > 0L)
    kernel.values <- c(kernel.values[(kernel.shift + 1L):2L],
                       kernel.values[seq_len(scale - kernel.shift)])

  kernel.values <- kernel.values * scale

  x.acc <- matrix(NA_real_, nrow=nrow(x.matrix), ncol=ncol(x.matrix),
                  dimnames=dimnames(x.matrix))

  for (j in seq_len(ncol(x.matrix))) {
    if (scale == 1L) {
      x.acc[, j] <- x.matrix[, j]
    } else {
        x.acc[, j] <- as.numeric(stats::filter(x.matrix[, j],
                                                filter=kernel.values,
                                                sides=1))
      } # ELSE end
  } # FOR end

  first.mon     <- as.integer(format(months[1], "%m"))
  first.ref.mon <- as.integer(format(months[which(ref)[1]], "%m"))
  spi.values    <- x.acc

  for (j in seq_len(ncol(x.acc))) {
    series.name <- colnames(x.acc)[j]
    if (is.null(series.name) || !nzchar(series.name))
      series.name <- as.character(j)

    .droughtMessage(
      verbose=verbose,
      index.name=index.name,
      text=paste0("processing series ", j, " of ", ncol(x.acc),
                  " (", series.name, ")")
    )

    if (is.null(provided.params)) {
      fitted.distribution <- .fitDroughtDistribution(
        x=x.acc[ref, j],
        first.mon=first.ref.mon,
        distr=distribution,
        fit=fit,
        p0=p0,
        p0.center.mass=p0.center.mass,
        scaling=scaling,
        start.fun=start.fun,
        start.fun.fix=start.fun.fix,
        warn=warn,
        ...
      )
    } else {
        fitted.distribution <- .providedDroughtFit(
          x=x.acc[ref, j],
          first.mon=first.ref.mon,
          distr=distribution,
          params=provided.params[, j, ],
          p0=p0,
          p0.center.mass=p0.center.mass
        )
      }

    spi.values[, j] <- .transformDroughtIndex(
      x=x.acc[, j],
      first.mon=first.mon,
      obj=fitted.distribution,
      sci.limit=sci.limit,
      warn=warn
    )
  } # FOR end

  if (x.is.vector)
    spi.values <- spi.values[, 1]

  if (out.type == "zoo")
    spi.values <- zoo::zoo(spi.values, dates)

  .droughtMessage(verbose=verbose,
                  index.name=index.name,
                  text="computation completed")

  return(spi.values)

} # '.droughtIndex' END


.emptyDroughtParameters <- function(distr) {

  switch(distr,
         gamma=c(shape=NA_real_, rate=NA_real_),
         genlog=c(shape=NA_real_, scale=NA_real_, location=NA_real_),
         gev=c(loc=NA_real_, scale=NA_real_, shape=NA_real_),
         gumbel=c(loc=NA_real_, scale=NA_real_),
         logis=c(location=NA_real_, scale=NA_real_),
         llogis=c(shape=NA_real_, scale=NA_real_),
         lnorm=c(meanlog=NA_real_, sdlog=NA_real_),
         norm=c(mean=NA_real_, sd=NA_real_),
         pe3=c(shape=NA_real_, scale=NA_real_, location=NA_real_),
         weibull=c(shape=NA_real_, scale=NA_real_))

} # '.emptyDroughtParameters' END


.droughtMessage <- function(verbose, index.name, text) {

  if (verbose)
    message("[", index.name, ": ", text, "]")

  invisible(NULL)

} # '.droughtMessage' END


.normaliseDroughtParams <- function(params,
                                    distr,
                                    n.series,
                                    series.names=NULL) {

  if (is.null(params))
    return(NULL)

  if (!is.numeric(params))
    stop("Invalid argument: 'params' must be numeric !")

  parameter.names <- names(.emptyDroughtParameters(distr))
  n.parameters <- length(parameter.names)
  params.dim <- dim(params)

  reorder.parameters <- function(x, supplied.names) {
    if (is.null(supplied.names))
      return(x)

    if (length(supplied.names) != n.parameters ||
        anyDuplicated(supplied.names) ||
        !setequal(supplied.names, parameter.names))
      stop("Invalid argument: parameter names in 'params' must be: ",
           paste(parameter.names, collapse=", "), " !")

    x[match(parameter.names, supplied.names), , drop=FALSE]
  }

  if (is.null(params.dim)) {
    if (length(params) != n.parameters)
      stop("Invalid argument: a parameter vector must have length ",
           n.parameters, " for distribution '", distr, "' !")

    if (!is.null(names(params))) {
      if (anyDuplicated(names(params)) ||
          !setequal(names(params), parameter.names))
        stop("Invalid argument: parameter names in 'params' must be: ",
             paste(parameter.names, collapse=", "), " !")
      params <- params[parameter.names]
    }

    result <- array(rep(params, n.series * 12L),
                    dim=c(n.parameters, n.series, 12L))
  } else if (length(params.dim) == 2L) {
      if (!identical(as.integer(params.dim), c(n.parameters, 12L)))
        stop("Invalid argument: a parameter matrix must have dimensions ",
             n.parameters, " x 12 for distribution '", distr, "' !")

      params <- reorder.parameters(params, rownames(params))
      result <- array(NA_real_, dim=c(n.parameters, n.series, 12L))
      for (j in seq_len(n.series))
        result[, j, ] <- params
    } else if (length(params.dim) == 3L) {
        expected.dim <- c(n.parameters, n.series, 12L)
        if (!identical(as.integer(params.dim), expected.dim))
          stop("Invalid argument: a parameter array must have dimensions ",
               paste(expected.dim, collapse=" x "), " !")

        supplied.names <- dimnames(params)[[1L]]
        if (!is.null(supplied.names)) {
          if (length(supplied.names) != n.parameters ||
              anyDuplicated(supplied.names) ||
              !setequal(supplied.names, parameter.names))
            stop("Invalid argument: parameter names in 'params' must be: ",
                 paste(parameter.names, collapse=", "), " !")
          params <- params[match(parameter.names, supplied.names), , ,
                           drop=FALSE]
        }
        result <- params
      } else {
          stop("Invalid argument: 'params' must be a vector, matrix, ",
               "or three-dimensional array !")
        }

  if (any(!is.finite(result)))
    stop("Invalid argument: all values in 'params' must be finite !")

  dimnames(result) <- list(parameter.names,
                           if (is.null(series.names))
                             as.character(seq_len(n.series))
                           else series.names,
                           paste0("M", seq_len(12L)))

  positive.parameters <- switch(
    distr,
    gamma=c("shape", "rate"),
    genlog="scale",
    gev="scale",
    gumbel="scale",
    logis="scale",
    llogis=c("shape", "scale"),
    lnorm="sdlog",
    norm="sd",
    pe3="scale",
    weibull=c("shape", "scale")
  )

  if (any(result[positive.parameters, , , drop=FALSE] <= 0))
    stop("Invalid argument: parameter(s) ",
         paste(positive.parameters, collapse=", "),
         " in 'params' must be positive !")

  result

} # '.normaliseDroughtParams' END


.providedDroughtFit <- function(x,
                                first.mon,
                                distr,
                                params,
                                p0,
                                p0.center.mass) {

  months <- (seq_along(x) + first.mon - 2L) %% 12L + 1L
  params <- as.matrix(params)
  flags <- integer(12L)

  if (p0) {
    p.zero <- n.zero <- n.data <- rep(NA_real_, 12L)

    for (mm in seq_len(12L)) {
      data <- x[months == mm]
      data <- data[is.finite(data)]
      n.data[mm] <- length(data)

      if (n.data[mm] > 0L) {
        n.zero[mm] <- sum(data == 0)
        p.zero[mm] <- if (p0.center.mass) {
          n.zero[mm] / (n.data[mm] + 1)
        } else {
            n.zero[mm] / n.data[mm]
          }
      }
    }

    params <- rbind(params, P0=p.zero)
    if (p0.center.mass)
      params <- rbind(params, N.P0=n.zero, N=n.data)
  }

  structure(list(dist.para=params,
                 dist.para.flag=flags,
                 time.scale=1L,
                 distr=distr,
                 p0=p0,
                 p0.center.mass=p0.center.mass,
                 fit="user-supplied",
                 scaling=1),
            class="droughtIndexFit")

} # '.providedDroughtFit' END


.sampleDroughtLmoments <- function(x, fit) {

  x <- sort(x[is.finite(x)])
  n <- length(x)

  if (n < 4L)
    return(c(L1=NA_real_, L2=NA_real_, T3=NA_real_))

  ranks <- seq_len(n)

  if (fit == "ub-pwm") {
    beta0 <- mean(x)
    beta1 <- mean(x * (ranks - 1) / (n - 1))
    beta2 <- mean(x * (ranks - 1) * (ranks - 2) /
                  ((n - 1) * (n - 2)))
  } else {
      # Plotting positions used by SPEI: A=-0.35 and B=0.
      probabilities <- (ranks - 0.35) / n
      beta0 <- mean(x)
      beta1 <- mean(x * probabilities)
      beta2 <- mean(x * probabilities^2)
    }

  l1 <- beta0
  l2 <- 2 * beta1 - beta0
  l3 <- 6 * beta2 - 6 * beta1 + beta0
  t3 <- l3 / l2

  if (!all(is.finite(c(l1, l2, t3))) || l2 <= 0 || abs(t3) >= 1)
    return(c(L1=NA_real_, L2=NA_real_, T3=NA_real_))

  c(L1=l1, L2=l2, T3=t3)

} # '.sampleDroughtLmoments' END


.startingValuesBase <- function(x, distr) {

  lmoments <- .sampleDroughtLmoments(x=x, fit="ub-pwm")
  start <- .pwmParametersBase(lmoments=lmoments, distr=distr)

  if (all(is.finite(start)))
    return(start)

  x <- x[is.finite(x)]
  empty <- .emptyDroughtParameters(distr)

  if (length(x) < 2L || stats::sd(x) == 0)
    return(empty)

  x.mean <- mean(x)
  x.sd   <- stats::sd(x)

  start <- switch(
    distr,
    gamma={
      if (any(x <= 0))
        empty
      else
        c(shape=x.mean^2 / stats::var(x),
          rate=x.mean / stats::var(x))
    },
    genlog=c(shape=0,
             scale=x.sd * sqrt(3) / pi,
             location=x.mean),
    gev=c(loc=x.mean - 0.5772156649015329 * x.sd * sqrt(6) / pi,
          scale=x.sd * sqrt(6) / pi,
          shape=0),
    gumbel=c(loc=x.mean - 0.5772156649015329 * x.sd * sqrt(6) / pi,
             scale=x.sd * sqrt(6) / pi),
    logis=c(location=x.mean, scale=x.sd * sqrt(3) / pi),
    llogis={
      if (any(x <= 0) || stats::sd(log(x)) == 0)
        empty
      else
        c(shape=pi / (sqrt(3) * stats::sd(log(x))),
          scale=exp(mean(log(x))))
    },
    lnorm={
      if (any(x <= 0))
        empty
      else
        c(meanlog=mean(log(x)), sdlog=stats::sd(log(x)))
    },
    norm=c(mean=x.mean, sd=x.sd),
    pe3={
      skewness <- mean((x - x.mean)^3) / x.sd^3
      c(shape=max(min(skewness, 1.75), -1.75),
        scale=x.sd,
        location=x.mean)
    },
    weibull={
      if (any(x <= 0) || stats::var(log(x)) == 0)
        empty
      else {
        shape <- 1.2 / sqrt(stats::var(log(x)))
        c(shape=shape, scale=exp(mean(log(x)) + 0.572 / shape))
      }
    }
  )

  if (length(start) != length(empty) || any(!is.finite(start))) {
    empty
  } else {
      start[names(empty)]
    }

} # '.startingValuesBase' END


.pwmParametersBase <- function(lmoments, distr) {

  empty <- .emptyDroughtParameters(distr)

  if (any(!is.finite(lmoments)))
    return(empty)

  l1  <- unname(lmoments["L1"])
  l2  <- unname(lmoments["L2"])
  t3  <- unname(lmoments["T3"])
  lcv <- l2 / l1

  parameters <- tryCatch(
    switch(
      distr,
      gamma={
        if (l1 <= 0 || lcv <= 0 || lcv >= 1)
          stop("Invalid Gamma L-moments")

        if (lcv >= 0.5) {
          tt <- 1 - lcv
          shape <- tt * (0.7213 - 0.5947 * tt) /
                   (1 + tt * (-2.1817 + 1.2113 * tt))
        } else {
            tt <- pi * lcv^2
            shape <- (1 - 0.308 * tt) /
                     (tt * (1 + tt * (-0.05812 + 0.01765 * tt)))
          }

        c(shape=shape, rate=shape / l1)
      },
      genlog={
        shape <- -t3
        if (abs(shape) < 1e-6) {
          c(shape=0, scale=l2, location=l1)
        } else {
            kk <- shape * pi / sin(shape * pi)
            scale <- l2 / kk
            location <- l1 - scale * (1 - kk) / shape
            c(shape=shape, scale=scale, location=location)
          }
      },
      gev={
        kappa <- .gevKappaFromT3(t3)
        if (!is.finite(kappa))
          stop("Invalid GEV L-skewness")

        if (abs(kappa) < 1e-6) {
          scale <- l2 / log(2)
          c(loc=l1 - 0.5772156649015329 * scale,
            scale=scale,
            shape=0)
        } else {
            gamma.value <- gamma(1 + kappa)
            scale <- l2 * kappa /
                     (gamma.value * (1 - 2^(-kappa)))
            loc <- l1 - scale * (1 - gamma.value) / kappa
            c(loc=loc, scale=scale, shape=-kappa)
          }
      },
      gumbel={
        scale <- l2 / log(2)
        c(loc=l1 - 0.5772156649015329 * scale, scale=scale)
      },
      logis=c(location=l1, scale=l2),
      llogis={
        if (l1 <= 0 || lcv <= 0 || lcv >= 1)
          stop("Invalid log-logistic L-moments")

        shape <- 1 / lcv
        angle <- pi / shape
        scale <- l1 * sin(angle) / angle
        c(shape=shape, scale=scale)
      },
      lnorm={
        if (l1 <= 0 || lcv <= 0 || lcv >= 1)
          stop("Invalid log-normal L-moments")

        sdlog <- sqrt(2) * stats::qnorm((1 + lcv) / 2)
        c(meanlog=log(l1) - 0.5 * sdlog^2, sdlog=sdlog)
      },
      norm=c(mean=l1, sd=l2 * sqrt(pi)),
      pe3={
        t3.abs <- abs(t3)

        if (t3.abs <= 1e-6) {
          c(shape=0, scale=l2 * sqrt(pi), location=l1)
        } else {
            if (t3.abs >= 1 / 3) {
              tt <- 1 - t3.abs
              alpha <- tt * (0.36067 + tt *
                       (-0.59567 + 0.25361 * tt)) /
                       (1 + tt * (-2.78861 + tt *
                       (2.56096 - 0.77045 * tt)))
            } else {
                tt <- 3 * pi * t3.abs^2
                alpha <- (1 + 0.2906 * tt) /
                         (tt * (1 + tt * (0.1882 + 0.0442 * tt)))
              }

            beta <- sqrt(pi) * l2 *
                    exp(lgamma(alpha) - lgamma(alpha + 0.5))
            shape <- sign(t3) * 2 / sqrt(alpha)
            c(shape=shape, scale=beta * sqrt(alpha), location=l1)
          }
      },
      weibull={
        if (l1 <= 0 || lcv <= 0 || lcv >= 1)
          stop("Invalid Weibull L-moments")

        shape <- -log(2) / log(1 - lcv)
        scale <- l1 / gamma(1 + 1 / shape)
        c(shape=shape, scale=scale)
      }
    ),
    error=function(e) empty
  )

  if (length(parameters) != length(empty) ||
      any(!is.finite(parameters))) {
    empty
  } else {
      parameters[names(empty)]
    }

} # '.pwmParametersBase' END


.gevKappaFromT3 <- function(t3) {

  tau3 <- function(kappa) {
    if (abs(kappa) < 1e-7)
      return(2 * log(3) / log(2) - 3)

    2 * (1 - 3^(-kappa)) / (1 - 2^(-kappa)) - 3
  }

  objective <- function(kappa)
    tau3(kappa) - t3

  lower <- -0.99
  upper <- 60

  if (objective(lower) * objective(upper) > 0)
    return(NA_real_)

  tryCatch(stats::uniroot(objective,
                          lower=lower,
                          upper=upper,
                          tol=1e-10)$root,
           error=function(e) NA_real_)

} # '.gevKappaFromT3' END


.fitDroughtDistribution <- function(x,
                                    first.mon,
                                    distr,
                                    fit,
                                    p0,
                                    p0.center.mass,
                                    scaling,
                                    mledist.par=list(),
                                    start.fun=NULL,
                                    start.fun.fix=FALSE,
                                    warn=TRUE,
                                    ...) {

  if (!is.list(mledist.par))
    stop("Invalid argument: 'mledist.par' must be a list !")

  if (!is.null(start.fun) && !is.function(start.fun))
    stop("Invalid argument: 'start.fun' must be NULL or a function !")

  if (length(start.fun.fix) != 1L || !is.logical(start.fun.fix) ||
      is.na(start.fun.fix))
    stop("Invalid argument: 'start.fun.fix' must be TRUE or FALSE !")

  scale.val <- switch(scaling,
                      no=1,
                      max=max(x, na.rm=TRUE),
                      sd=stats::sd(x, na.rm=TRUE))

  if (!is.finite(scale.val) || scale.val == 0)
    stop("The reference-period scaling value is zero or non-finite !")

  x      <- x / scale.val
  months <- (seq_along(x) + first.mon - 2L) %% 12L + 1L
  params <- vector("list", 12L)
  flags  <- integer(12L)

  empty.parameters <- .emptyDroughtParameters(distr)
  empty.fit <- empty.parameters

  if (p0) {
    empty.fit <- c(empty.fit, P0=NA_real_)
    if (p0.center.mass)
      empty.fit <- c(empty.fit, N.P0=NA_real_, N=NA_real_)
  }

  for (mm in seq_len(12L)) {
    data <- x[months == mm]
    data <- data[is.finite(data)]

    if (length(data) == 0L) {
      params[[mm]] <- empty.fit
      flags[mm] <- 4L
      if (warn)
        warning("All values in month ", mm, " are NA.")
      next
    }

    if (all(data == data[1L])) {
      params[[mm]] <- empty.fit
      flags[mm] <- 5L
      if (warn)
        warning("All values in month ", mm,
                " are constant; the distribution is not defined.")
      next
    }

    if (p0) {
      n.zero <- sum(data == 0)
      n.data <- length(data)
      p.zero <- if (p0.center.mass) {
        n.zero / (n.data + 1)
      } else {
          n.zero / n.data
        }

      if (p.zero > 0) {
        data <- data[data > 0]
        if (fit == "max-lik")
          data <- c(data, 0.01 * min(data))
      }
    }

    if (fit == "max-lik") {
      if (is.null(start.fun)) {
        start <- .startingValuesBase(x=data, distr=distr)
      } else {
          start <- tryCatch(unlist(start.fun(x=data, distr=distr)),
                            error=function(e) empty.parameters)
        }
    } else {
        lmoments <- .sampleDroughtLmoments(x=data, fit=fit)
        start <- .pwmParametersBase(lmoments=lmoments, distr=distr)
      }

    expected.names <- names(empty.parameters)
    if (!is.numeric(start) || length(start) != length(empty.parameters)) {
      start <- empty.parameters
    } else if (is.null(names(start))) {
        names(start) <- expected.names
      } else if (anyDuplicated(names(start)) ||
                 !setequal(names(start), expected.names)) {
          start <- empty.parameters
        } else {
            start <- start[expected.names]
          }

    if (any(!is.finite(start))) {
      fitted <- start
      flags[mm] <- 1L
      if (warn)
        warning("Starting values in month ", mm,
                " could not be estimated; parameters are NA.")
    } else {
        if (fit == "max-lik") {
          fitted <- .mleDroughtIndex(data=data,
                                     distr=distr,
                                     start=start,
                                     mledist.par=mledist.par)
        } else {
            fitted <- start
          }

        if (is.null(fitted) || any(!is.finite(fitted))) {
          flags[mm] <- 2L
          if (start.fun.fix) {
            fitted <- start
            if (warn)
              warning("Maximum-likelihood estimation failed for month ", mm,
                      "; starting values are used.")
          } else {
              fitted <- start
              fitted[] <- NA_real_
              if (warn)
                warning("Maximum-likelihood estimation failed for month ", mm,
                        "; parameters are NA.")
            }
        }
      }

    if (p0) {
      fitted <- c(fitted, P0=p.zero)
      if (p0.center.mass)
        fitted <- c(fitted, N.P0=n.zero, N=n.data)
    }

    if (anyNA(fitted))
      fitted[] <- NA_real_

    params[[mm]] <- fitted
  }

  names(params) <- paste0("M", seq_len(12L))
  names(flags)  <- names(params)

  structure(list(dist.para=do.call(cbind, params),
                 dist.para.flag=flags,
                 time.scale=1L,
                 distr=distr,
                 p0=p0,
                 p0.center.mass=p0.center.mass,
                 fit=fit,
                 scaling=scale.val),
            class="droughtIndexFit")

} # '.fitDroughtDistribution' END


.mleDroughtIndex <- function(data, distr, start, mledist.par) {

  density.fun <- switch(distr,
                        gamma=stats::dgamma,
                        genlog=.dgenlogDrought,
                        gev=.dgevDrought,
                        gumbel=.dgumbelDrought,
                        logis=stats::dlogis,
                        llogis=.dllogisDrought,
                        lnorm=stats::dlnorm,
                        norm=stats::dnorm,
                        pe3=.dpe3Drought,
                        weibull=stats::dweibull)

  fixed <- mledist.par$fix.arg
  if (is.null(fixed))
    fixed <- numeric()

  fixed <- unlist(fixed)
  estimate <- start[!names(start) %in% names(fixed)]

  if (length(estimate) == 0L)
    return(c(start[0L], fixed[names(start)]))

  objective <- function(par) {
    all.par <- c(par, fixed)
    log.density <- suppressWarnings(
      tryCatch({
        do.call(density.fun,
                c(list(data), as.list(all.par), list(log=TRUE)))
      }, error=function(e) rep(NA_real_, length(data)))
    )

    if (length(log.density) != length(data) ||
        any(!is.finite(log.density)))
      return(.Machine$integer.max)

    -mean(log.density)
  }

  custom.optim <- mledist.par$custom.optim
  method       <- mledist.par$optim.method
  lower        <- mledist.par$lower
  upper        <- mledist.par$upper

  if (is.null(method) || identical(method, "default"))
    method <- if (length(estimate) > 1L) "Nelder-Mead" else "BFGS"

  if (is.null(lower))
    lower <- -Inf

  if (is.null(upper))
    upper <- Inf

  excluded <- c("data", "distr", "start", "fix.arg", "custom.optim",
                "optim.method", "lower", "upper", "silent", "gradient",
                "checkstartfix", "calcvcov", "weights")
  optim.args <- mledist.par[!names(mledist.par) %in% excluded]

  fit <- tryCatch({
    if (is.null(custom.optim)) {
      if ((any(is.finite(lower)) || any(is.finite(upper))) &&
          method %in% c("Nelder-Mead", "BFGS", "CG"))
        method <- "L-BFGS-B"

      do.call(stats::optim,
              c(list(par=estimate, fn=objective, method=method,
                     lower=lower, upper=upper),
                optim.args))
    } else {
        do.call(custom.optim,
                c(list(par=estimate, fn=objective), optim.args))
      }
  }, error=function(e) NULL)

  if (is.null(fit) || is.null(fit$convergence) || fit$convergence != 0L)
    return(NULL)

  fitted <- fit$par
  if (is.null(fitted))
    fitted <- fit$estimate

  if (is.null(fitted))
    return(NULL)

  fitted <- c(fitted, fixed)
  fitted[names(start)]

} # '.mleDroughtIndex' END


.transformDroughtIndex <- function(x,
                                   first.mon,
                                   obj,
                                   sci.limit,
                                   warn) {

  x      <- as.numeric(x) / obj$scaling
  months <- (seq_along(x) + first.mon - 2L) %% 12L + 1L
  p.fun  <- switch(obj$distr,
                   gamma=stats::pgamma,
                   genlog=.pgenlogDrought,
                   gev=.pgevDrought,
                   gumbel=.pgumbelDrought,
                   logis=stats::plogis,
                   llogis=.pllogisDrought,
                   lnorm=stats::plnorm,
                   norm=stats::pnorm,
                   pe3=.ppe3Drought,
                   weibull=stats::pweibull)

  if (obj$p0) {
    p.zero <- obj$dist.para["P0", ]
    if (obj$p0.center.mass) {
      n.zero <- obj$dist.para["N.P0", ]
      n.data <- obj$dist.para["N", ]
      distribution.params <- obj$dist.para[
        !rownames(obj$dist.para) %in% c("P0", "N.P0", "N"), ,
        drop=FALSE
      ]
    } else {
        distribution.params <- obj$dist.para[
          rownames(obj$dist.para) != "P0", , drop=FALSE
        ]
      }
  } else {
      distribution.params <- obj$dist.para
    }

  probabilities <- rep(NA_real_, length(x))

  for (mm in seq_len(12L)) {
    selected <- months == mm
    values   <- x[selected]
    params   <- distribution.params[, mm]

    if (anyNA(params)) {
      if (warn)
        warning("Parameters for month ", mm, " are NA.")
      next
    }

    transformed <- suppressWarnings(
      tryCatch(do.call(p.fun, c(list(values), as.list(params))),
               error=function(e) rep(NA_real_, length(values)))
    )

    if (obj$p0) {
      transformed <- p.zero[mm] + (1 - p.zero[mm]) * transformed
      if (obj$p0.center.mass)
        transformed[values == 0] <- (n.zero[mm] + 1) /
                                     (2 * (n.data[mm] + 1))
    }

    probabilities[selected] <- transformed
  }

  values <- stats::qnorm(probabilities)
  values[values > sci.limit]  <- sci.limit
  values[values < -sci.limit] <- -sci.limit

  values

} # '.transformDroughtIndex' END


.dllogisDrought <- function(x, shape, scale, log=FALSE) {

  if (length(shape) != 1L || !is.finite(shape) || shape <= 0 ||
      length(scale) != 1L || !is.finite(scale) || scale <= 0)
    return(rep(NaN, length(x)))

  log.density <- rep(-Inf, length(x))
  valid <- !is.na(x) & x > 0
  log.x <- log(x[valid])
  log.density[valid] <- stats::dlogis(log.x,
                                      location=log(scale),
                                      scale=1 / shape,
                                      log=TRUE) - log.x
  log.density[is.na(x)] <- NA_real_

  if (log)
    return(log.density)

  exp(log.density)

} # '.dllogisDrought' END


.pllogisDrought <- function(q, shape, scale) {

  if (length(shape) != 1L || !is.finite(shape) || shape <= 0 ||
      length(scale) != 1L || !is.finite(scale) || scale <= 0)
    return(rep(NaN, length(q)))

  probabilities <- rep(0, length(q))
  valid <- !is.na(q) & q > 0
  probabilities[valid] <- stats::plogis(log(q[valid]),
                                        location=log(scale),
                                        scale=1 / shape)
  probabilities[is.na(q)] <- NA_real_

  probabilities

} # '.pllogisDrought' END


.dgenlogDrought <- function(x,
                            shape,
                            scale,
                            location,
                            log=FALSE) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(x)))

  support <- 1 - shape * (x - location) / scale
  valid <- !is.na(support) & support > 0
  y <- rep(NA_real_, length(x))

  if (abs(shape) < sqrt(.Machine$double.eps)) {
    y[valid] <- (x[valid] - location) / scale
  } else {
      y[valid] <- -log(support[valid]) / shape
    }

  log1pexp <- function(z)
    pmax(z, 0) + log1p(exp(-abs(z)))

  log.density <- rep(-Inf, length(x))
  log.density[valid] <- -log(scale) - (1 - shape) * y[valid] -
                        2 * log1pexp(-y[valid])
  log.density[is.na(x)] <- NA_real_

  if (log)
    return(log.density)

  exp(log.density)

} # '.dgenlogDrought' END


.pgenlogDrought <- function(q, shape, scale, location) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(q)))

  support <- 1 - shape * (q - location) / scale
  valid <- !is.na(support) & support > 0
  y <- rep(NA_real_, length(q))

  if (abs(shape) < sqrt(.Machine$double.eps)) {
    y[valid] <- (q[valid] - location) / scale
  } else {
      y[valid] <- -log(support[valid]) / shape
    }

  probabilities <- rep(NA_real_, length(q))
  probabilities[valid] <- stats::plogis(y[valid])
  outside <- !is.na(support) & support <= 0
  probabilities[outside & shape > 0] <- 1
  probabilities[outside & shape < 0] <- 0

  probabilities

} # '.pgenlogDrought' END


.dpe3Drought <- function(x,
                         shape,
                         scale,
                         location,
                         log=FALSE) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(x)))

  if (abs(shape) < sqrt(.Machine$double.eps))
    return(stats::dnorm(x, mean=location, sd=scale, log=log))

  alpha <- 4 / shape^2
  beta  <- 0.5 * scale * abs(shape)
  xi    <- location - 2 * scale / shape
  transformed <- if (shape > 0) x - xi else xi - x
  log.density <- stats::dgamma(transformed / beta,
                               shape=alpha,
                               log=TRUE) - log(beta)

  if (log)
    return(log.density)

  exp(log.density)

} # '.dpe3Drought' END


.ppe3Drought <- function(q, shape, scale, location) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(q)))

  if (abs(shape) < sqrt(.Machine$double.eps) || 4 / shape^2 > 170)
    return(stats::pnorm(q, mean=location, sd=scale))

  alpha <- 4 / shape^2
  beta  <- 0.5 * scale * abs(shape)
  xi    <- location - 2 * scale / shape

  if (shape > 0) {
    stats::pgamma((q - xi) / beta, shape=alpha)
  } else {
      stats::pgamma((xi - q) / beta, shape=alpha, lower.tail=FALSE)
    }

} # '.ppe3Drought' END


.dgumbelDrought <- function(x, loc=0, scale=1, log=FALSE) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0)
    return(rep(NaN, length(x)))

  z <- (x - loc) / scale
  log.density <- -log(scale) - z - exp(-z)

  if (log)
    return(log.density)

  exp(log.density)

} # '.dgumbelDrought' END


.pgumbelDrought <- function(q, loc=0, scale=1) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0)
    return(rep(NaN, length(q)))

  exp(-exp(-(q - loc) / scale))

} # '.pgumbelDrought' END


.dgevDrought <- function(x, loc=0, scale=1, shape=0, log=FALSE) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(x)))

  if (abs(shape) < sqrt(.Machine$double.eps))
    return(.dgumbelDrought(x=x, loc=loc, scale=scale, log=log))

  z <- (x - loc) / scale
  support <- 1 + shape * z
  log.density <- rep(-Inf, length(x))
  valid <- is.finite(support) & support > 0
  log.density[valid] <- -log(scale) -
                        (1 / shape + 1) * log(support[valid]) -
                        support[valid]^(-1 / shape)

  if (log)
    return(log.density)

  exp(log.density)

} # '.dgevDrought' END


.pgevDrought <- function(q, loc=0, scale=1, shape=0) {

  if (length(scale) != 1L || !is.finite(scale) || scale <= 0 ||
      length(shape) != 1L || !is.finite(shape))
    return(rep(NaN, length(q)))

  if (abs(shape) < sqrt(.Machine$double.eps))
    return(.pgumbelDrought(q=q, loc=loc, scale=scale))

  z <- (q - loc) / scale
  support <- 1 + shape * z
  probabilities <- rep(NA_real_, length(q))
  valid <- !is.na(support) & support > 0
  probabilities[valid] <- exp(-support[valid]^(-1 / shape))
  outside <- !is.na(support) & support <= 0
  probabilities[outside & shape > 0] <- 0
  probabilities[outside & shape < 0] <- 1

  probabilities

} # '.pgevDrought' END
