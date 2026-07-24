# File subhourly2nhourly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                            subhourly2nhourly                                  #
################################################################################
# This function transform a SUB-HOURLY or HOURLY time series into an N-HOURLY one

# 'x'        : sub-hourly or hourly values that will be aggregated into n-hourly
#              ones. class(x) must be 'zoo'
# 'FUN'      : Function that have to be applied for transforming from sub-hourly
#              or hourly into n-hourly time step. E.g., for precipitation FUN
#              MUST be "sum". For temperature and flow time series, FUN MUST
#              be "mean"
# 'n.hours'  : Positive integer indicating the length of the aggregation period,
#              in hours.
# 'na.rm'    : Logical. Should missing values be removed?
# 'na.rm.max': Numeric in [0, 1], indicating the maximum percentage of missing
#              values allowed in each n-hour period.
# 'start'    : character, indicating the starting time used for anchoring the
#              n-hour aggregation periods.
# 'start.fmt': character indicating the format in which the time is provided
#              in 'start'.
# 'tz'       : character, with the specification of the time zone used in 'x'
#              and 'start'.

subhourly2nhourly <-function(x, ...) UseMethod("subhourly2nhourly")


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jul-2026                                                         #
################################################################################
subhourly2nhourly.default <- function(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                                      start="00:00:00", start.fmt="%H:%M:%S",
                                      tz, ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo'")

  if (missing(tz)) {
    subhourly2nhourly.zoo(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                          na.rm.max=na.rm.max, start=start,
                          start.fmt=start.fmt, ...)
  } else {
      subhourly2nhourly.zoo(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                            na.rm.max=na.rm.max, start=start,
                            start.fmt=start.fmt, tz=tz, ...)
    } # ELSE end

} # 'subhourly2nhourly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jul-2026                                                         #
################################################################################
subhourly2nhourly.zoo <- function(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                                  start="00:00:00", start.fmt="%H:%M:%S",
                                  tz, ...) {

    # Checking that the user provided a valid class for 'x'
    if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !!")

    # Checking the user provided a valid value for 'FUN'
    if ( missing(FUN) || !is.function(FUN) )
      stop("Missing argument: 'FUN' must contain a valid function for aggregating the sub-hourly or hourly values")

    # Checking 'n.hours'
    if (missing(n.hours))
      stop("Missing argument: 'n.hours' must be provided !")

    if ( !is.numeric(n.hours) || (length(n.hours) < 1) ||
         !is.finite(n.hours[1]) || (n.hours[1] < 1) )
      stop("Invalid argument: 'n.hours' must be a positive integer !")

    if (length(n.hours) > 1) {
      warning("[ length(n.hours) > 1 -> only the first element will be used ]")
      n.hours <- n.hours[1]
    } # IF end

    if (abs(n.hours - round(n.hours)) > sqrt(.Machine$double.eps))
      stop("Invalid argument: 'n.hours' must be a positive integer !")

    n.hours <- as.integer(round(n.hours))

    # Checking 'na.rm'
    if ( !is.logical(na.rm) || (length(na.rm) < 1) || is.na(na.rm[1]) )
      stop("Invalid argument: 'na.rm' must be a logical value !")

    if (length(na.rm) > 1) {
      warning("[ length(na.rm) > 1 -> only the first element will be used ]")
      na.rm <- na.rm[1]
    } # IF end

    # Checking that 'na.rm.max' is in [0, 1]
    if ( !is.numeric(na.rm.max) || (length(na.rm.max) < 1) ||
         !is.finite(na.rm.max[1]) || (na.rm.max[1] < 0) ||
         (na.rm.max[1] > 1) )
      stop("Invalid argument: 'na.rm.max' must be in [0, 1] !")

    if (length(na.rm.max) > 1) {
      warning("[ length(na.rm.max) > 1 -> only the first element will be used ]")
      na.rm.max <- na.rm.max[1]
    } # IF end

    # Getting the time index
    tx <- time(x)

    # Checking that the time index belongs to a suitable class
    if ( !inherits(tx, "POSIXt") )
      stop("Invalid argument: 'time(x)' must be of class 'POSIXct', 'POSIXlt' or 'POSIXt' !")

    if (NROW(x) < 2)
      stop("Invalid argument: 'x' must have at least two time steps !")

    # Automatic detection of 'tz'
    if (missing(tz)) {
      tz <- attr(tx, "tzone")
      if ( is.null(tz) || (length(tz) < 1) || is.na(tz[1]) )
        tz <- ""
    } else {
        # For the Date/Time of 'x' to be in the time zone specified by 'tz'
        tx.new  <- timechange::time_force_tz(tx, tz=tz)
        time(x) <- tx.new
      } # ELSE end

    tx <- as.POSIXct(time(x), tz=tz)

    # Checking the temporal order and frequency of 'x'
    dt.secs <- as.numeric(diff(tx), units="secs")

    if ( any(!is.finite(dt.secs)) || any(dt.secs <= 0) )
      stop("Invalid argument: 'time(x)' must be strictly increasing and must not contain duplicated values !")

    dt.ref <- min(dt.secs)
    dt.tol <- max(1, abs(dt.ref)) * sqrt(.Machine$double.eps)

    if (dt.ref > (3600 + dt.tol))
      stop("Invalid argument: 'x' must have an hourly or sub-hourly temporal frequency !")

    period.secs <- n.hours * 3600
    ratio       <- period.secs / dt.ref

    if (abs(ratio - round(ratio)) > sqrt(.Machine$double.eps))
      stop("Invalid argument: 'n.hours' must be an integer multiple of the temporal resolution of 'x' !")

    if ( !zoo::is.regular(x, strict=FALSE) )
      warning("'x' is not a regular '", sfreq(x), "' time series !. (see the 'izoo2rzoo' function)")

    # Computing the time difference between 00:00:00 and the user-provided 'start'
    start.full <- as.POSIXct(paste("2000-01-01", start),
                             format=paste("%Y-%m-%d", start.fmt), tz=tz)
    nstart     <- as.POSIXct("2000-01-01 00:00:00",
                             format="%Y-%m-%d %H:%M:%S", tz=tz)

    if (is.na(start.full))
      stop("Invalid argument: 'start' could not be interpreted using 'start.fmt' !")

    delta <- as.numeric(difftime(start.full, nstart, units="secs"))
    if ( !is.finite(delta) || (delta < 0) || (delta >= 86400) )
      stop("Invalid argument: 'start' must define a time within a single day !")

    # Shifting datetimes so that 'start' becomes the origin of the n-hour periods
    datetime.new <- tx - delta

    # Anchoring the first temporal sequence at midnight of the first shifted date
    anchor.date <- format(min(datetime.new), "%Y-%m-%d", tz=tz)
    anchor      <- as.POSIXct(paste(anchor.date, "00:00:00"),
                              format="%Y-%m-%d %H:%M:%S", tz=tz)

    elapsed <- as.numeric(difftime(datetime.new, anchor, units="secs"))
    ind     <- floor((elapsed + sqrt(.Machine$double.eps)) / period.secs) + 1L

    periods.id <- unique(ind)
    periods    <- anchor + (periods.id - 1L) * period.secs + delta
    periods    <- as.POSIXct(periods, origin="1970-01-01", tz=tz)

    # Computing the N-hourly time series
    tmp <- aggregate(x, by=ind, FUN=FUN, na.rm=na.rm, ...)
    tmp <- zoo::zoo(zoo::coredata(tmp), periods)

    # Removing values in the output object for n-hour periods with more than
    # 'na.rm.max' percentage of NAs.
    if ( na.rm ) {

      # Computing the total number of observed elements in each n-hour period
      ndata <- aggregate(x, by=ind, FUN=length)

      # Computing the number of missing values in each n-hour period
      smv <- function(x) {
        na.index <- is.na(x)
        return( sum(na.index) )
      } # 'smv' END

      nNA <- aggregate(x, by=ind, FUN=smv)

      # Computing the percentage of missing values in each n-hour period
      na.pctg <- nNA/ndata

      # Identifying n-hour periods with more missing values than 'na.rm.max'
      na.pctg.index <- which( na.pctg > na.rm.max)

      # Setting as NA all values with a percentage of missing values higher
      # than 'na.rm.max'
      tmp[na.pctg.index] <- NA
    } # IF end

    # Replacing the NaNs by 'NA.
    # mean(NA:NA, na.rm=TRUE) == NaN
    nan.index <- which(is.nan(tmp))
    if ( length(nan.index) > 0 ) tmp[nan.index] <- NA

    # Replacing all the Inf and -Inf by NA's
    # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
    inf.index <- which(is.infinite(tmp))
    if ( length(inf.index) > 0 ) tmp[inf.index] <- NA

    return(tmp)

} # 'subhourly2nhourly.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jul-2026                                                         #
################################################################################
# 'dates'   : "numeric", "factor", "POSIXct" or "POSIXt" indicating how to
#             obtain the dates and times corresponding to each row in 'x'.
# 'date.fmt': format in which the dates are stored in 'dates'.
#             ONLY required when class(dates)=="factor", "character" or
#             "numeric".
# 'out.fmt' : character, for selecting if the result will be 'numeric' or 'zoo'.
#             Valid values are: c('numeric', 'zoo')
# 'verbose' : logical; if TRUE, progress messages are printed
subhourly2nhourly.data.frame <- function(x, FUN, n.hours, na.rm=TRUE,
                                         na.rm.max=0, start="00:00:00",
                                         start.fmt="%H:%M:%S", tz,
                                         dates=1,
                                         date.fmt="%Y-%m-%d %H:%M:%S",
                                         out.fmt="zoo", verbose=TRUE, ...) {

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
      stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values !!")

  # Checking that the user provided a valid argument for 'out.fmt'
  if (is.na(match( out.fmt, c("numeric", "zoo") ) ) )
      stop("Invalid argument: 'out.fmt' must be in c('numeric', 'zoo')")

  # Checking that the user provided a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
     if (FALSE && (class(dates) %in% c("numeric", "factor", "character", "POSIXct", "POSIXt")) )
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'character', 'POSIXct' or 'POSIXt'")

  tz.tmp <- if (missing(tz)) "" else tz

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then subtracted from 'x' for easing the further computations
  if ( TRUE && ( inherits(dates, "numeric") ) ) {
    tmp   <- dates[1]
    dates <- as.POSIXct(x[, tmp], format=date.fmt, tz=tz.tmp)
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor or character, it has to be converted into POSIXct class,
  # using the date format specified by 'date.fmt'
  if ( TRUE && ( inherits(dates, "factor") || inherits(dates, "character") ) )
    dates <- as.POSIXct(dates, format=date.fmt, tz=tz.tmp)

  # If 'dates' is already POSIXct or POSIXt, the following line verifies that
  # the number of days in 'dates' is equal to the number of rows in 'x'
  if ( inherits(dates, "POSIXt") && (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # Transforming 'x' into a zoo object
  x <- zoo::zoo(x, dates)

  ##############################################################################

  if (missing(tz)) {
    z <- subhourly2nhourly.zoo(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                               na.rm.max=na.rm.max, start=start,
                               start.fmt=start.fmt, ...)
  } else {
      z <- subhourly2nhourly.zoo(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                                 na.rm.max=na.rm.max, start=start,
                                 start.fmt=start.fmt, tz=tz, ...)
    } # ELSE end

  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- zoo::coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab
  } # IF end

  return( z )

 } #'subhourly2nhourly.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jul-2026                                                         #
################################################################################
subhourly2nhourly.matrix  <- function(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                                      start="00:00:00", start.fmt="%H:%M:%S",
                                      tz, dates=1,
                                      date.fmt="%Y-%m-%d %H:%M:%S",
                                      out.fmt="zoo", verbose=TRUE, ...) {

   x <- as.data.frame(x)

   if (missing(tz)) {
     subhourly2nhourly.data.frame(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                                  na.rm.max=na.rm.max, start=start,
                                  start.fmt=start.fmt, dates=dates,
                                  date.fmt=date.fmt, out.fmt=out.fmt,
                                  verbose=verbose, ...)
   } else {
       subhourly2nhourly.data.frame(x=x, FUN=FUN, n.hours=n.hours, na.rm=na.rm,
                                    na.rm.max=na.rm.max, start=start,
                                    start.fmt=start.fmt, tz=tz, dates=dates,
                                    date.fmt=date.fmt, out.fmt=out.fmt,
                                    verbose=verbose, ...)
     } # ELSE end

} # 'subhourly2nhourly.matrix  ' END
