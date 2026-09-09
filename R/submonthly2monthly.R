# File submonthly2monthly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          submonthly2monthly                                                  #
################################################################################
# Generic function for transforming an hourly/daily/weekly/pseudo-weekly       #
# zoo object into a MONTHLY one                                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################
submonthly2monthly <- function(x, ...) UseMethod("submonthly2monthly")

.submonthly2monthly_check_time <- function(x) {

  dates <- time(x)

  if ( !(inherits(dates, "Date") | inherits(dates, "POSIXt")) )
    stop("Invalid argument: 'time(x)' must inherit from 'Date' or 'POSIXt' !")

  if (length(dates) > 1) {
    if (inherits(dates, "POSIXt")) {
      delta.days <- as.numeric(diff(dates), units="days")
    } else delta.days <- as.numeric(diff(dates))

    delta.days <- delta.days[is.finite(delta.days) & (delta.days > 0)]

    if (length(delta.days) <= 0)
      stop("Invalid argument: 'time(x)' must have at least one positive time step !")

    if (min(delta.days) >= 28)
      stop("Invalid argument: 'x' is not a sub-monthly ts. The shortest positive time step is ",
           round(min(delta.days), 3), " days !")
  } # IF end

  invisible(TRUE)

} # '.submonthly2monthly_check_time' end

.submonthly2monthly_na_pctg <- function(x, months) {

  smv <- function(x) sum(is.na(x))

  ndata <- aggregate(x, by=months, FUN=length)
  nNA   <- aggregate(x, by=months, FUN=smv)

  nNA / ndata

} # '.submonthly2monthly_na_pctg' end

.submonthly2monthly_parse_dates <- function(dates, date.fmt, tz=NULL) {

  if (inherits(dates, "Date") | inherits(dates, "POSIXt"))
    return(dates)

  if (inherits(dates, "factor"))
    dates <- as.character(dates)

  if (is.null(tz))
    tz <- ""

  if (inherits(dates, "character")) {
    has.time <- grepl("%H|%I|%M|%S|%OS|%p", date.fmt)

    if (has.time) {
      dates <- as.POSIXct(dates, format=date.fmt, tz=tz)
    } else dates <- as.Date(dates, format=date.fmt)

    return(dates)
  } # IF end

  stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'character', 'Date' or 'POSIXt' !")

} # '.submonthly2monthly_parse_dates' end

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################
submonthly2monthly.default <- function(x, FUN, na.rm=TRUE, na.rm.max=0, ... ) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

  submonthly2monthly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)

} # 'submonthly2monthly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################
submonthly2monthly.zoo <- function(x, FUN, na.rm=TRUE, na.rm.max=0, ... ) {

  # Checking the user provide a valid value for 'FUN'
  if (missing(FUN))
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")

  # Checking the user provide a valid value for 'x'
  .submonthly2monthly_check_time(x)

  # Checking that 'na.rm.max' is in [0, 1]
  if ( (na.rm.max < 0) | (na.rm.max > 1) )
    stop("Invalid argument: 'na.rm.max' must be in [0, 1] !") 

  # Monthly index for 'x'
  dates  <- time(x)
  months <- as.Date( as.yearmon(dates) ) # zoo::as.Date ; zoo::as.yearmon

  # Computing the Monthly time series 
  tmp <- aggregate(x, by=months, FUN, na.rm=na.rm, ...)

  # Removing monthly values in the output object for months with 
  # more than 'na.rm.max' percentage of NAs in a given month
  if ( na.rm ) {

    # Computing the percentage of missing values in each month
    na.pctg <- .submonthly2monthly_na_pctg(x=x, months=months)

    # identifying months with a percentage of missing values higher than 'na.rm.max'
    na.pctg.index <- which( na.pctg > na.rm.max)

    # Setting as NA all the months with a percentage of missing values higher than 'na.rm.max'
    tmp[na.pctg.index] <- NA 
  } # IF end

  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(tmp))
  if ( length(nan.index) > 0 )  tmp[nan.index] <- NA 

  # Replacing all the Inf and -Inf by NA's
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(tmp))
  if ( length(inf.index) > 0 ) tmp[inf.index] <- NA 

  if (NCOL(tmp) == 1) tmp <- zoo(as.numeric(tmp), time(tmp))

  return(tmp)

} # 'submonthly2monthly.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################
submonthly2monthly.data.frame <- function(x, FUN, na.rm=TRUE, na.rm.max=0,
                                          dates=1, date.fmt="%Y-%m-%d",
                                          tz=NULL,
                                          out.type="data.frame",
                                          out.fmt="numeric",
                                          verbose=TRUE, ...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
    stop("Invalid argument: 'out.type' must be in c('data.frame', 'db')")

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values !")

  # Checking that the user provied a valid argument for 'out.fmt'
  if (is.na(match( out.fmt, c("numeric", "zoo") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('numeric', 'zoo')")

  # Checking that the user provied a valid argument for 'dates'
  if ( !(inherits(dates, "numeric") | inherits(dates, "factor") |
         inherits(dates, "character") | inherits(dates, "Date") |
         inherits(dates, "POSIXt")) )
    stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'character', 'Date' or 'POSIXt' !")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( inherits(dates, "numeric") ) {
    tmp   <- dates
    dates <- .submonthly2monthly_parse_dates(x[, dates], date.fmt=date.fmt, tz=tz)
    x     <- x[-tmp]
  } else dates <- .submonthly2monthly_parse_dates(dates, date.fmt=date.fmt, tz=tz)

  # If 'dates' is already of Date or POSIXt class, the following line verifies that
  # the number of time steps in 'dates' be equal to the number of element in 'x'
  if ( length(dates) != nrow(x) )
    stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  if ( any(is.na(dates)) )
    stop("Invalid argument: 'dates' contains values that could not be converted using 'date.fmt' !")

  # Transforming 'x' into a zoo object
  x <- zoo(x, dates)

  ##############################################################################
  if (out.type == "data.frame") {

    z <- submonthly2monthly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)

    if (out.fmt == "numeric") {
      snames <- colnames(x)
      if (is.null(snames)) snames <- paste("V", 1:ncol(x), sep="")

      months.lab  <- format(time(z), "%b-%Y")
      z           <- coredata(z)
      if (is.null(dim(z))) z <- matrix(z, ncol=1)
      colnames(z) <- snames
      rownames(z) <- months.lab        
    } # IF end

  } else if (out.type == "db") {

    if (verbose) message("[Starting computations...]" )

    # Amount of stations in 'x'
    nstations <- ncol(x)

    # ID of all the stations in 'x'
    snames <- colnames(x)
    if (is.null(snames)) snames <- paste("V", 1:nstations, sep="")

    # Computing the amount of months with data within the desired period
    ntime   <- length(dates)
    tmp     <- vector2zoo(rep(0, ntime), dates)
    tmp     <- submonthly2monthly.default(x=tmp, FUN=FUN, na.rm=na.rm)
    nmonths <- length(tmp)

    # Creating a vector with the names of the field that will be used for storing the results
    field.names <- c("StationID", "Year", "Month", "Value" )

    # Creating the data.frame that will store the computed averages for each subcatchment
    z <- as.data.frame(matrix(data = NA, nrow = nmonths*nstations, ncol = 4,
                              byrow = TRUE, dimnames = NULL) )
    colnames(z) <- field.names

    for (j in 1:nstations) {

      if (verbose) message( "Station: ", format(snames[j], width=10, justify="left"),
                            " : ",format(j, width=3, justify="left"), "/",
                            nstations, " => ",
                            format(round(100*j/nstations,2), width=6, justify="left"),
                            "%" )

      # Computing the monthly values
      m     <- submonthly2monthly.zoo(x= x[, j], FUN=FUN, ..., na.rm=na.rm,
                                      na.rm.max=na.rm.max)
      dates <- time(m)

      if (out.fmt == "numeric") m <- coredata(m)

      # Putting the monthly values in the output data.frame
      row.ini <- (j-1)*nmonths + 1
      row.fin <-  j*nmonths

      z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeated 'nmonths' times
      z[row.ini:row.fin, 2] <- format(as.Date(dates), "%Y") # zoo::as.Date
      z[row.ini:row.fin, 3] <- format(as.Date(dates), "%b") # zoo::as.Date
      z[row.ini:row.fin, 4] <- m

    } # FOR end

  } # IF end

  return( z )

} # 'submonthly2monthly.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################
submonthly2monthly.matrix  <- function(x, FUN, na.rm=TRUE, na.rm.max=0,
                                       dates=1, date.fmt="%Y-%m-%d",
                                       tz=NULL,
                                       out.type="data.frame",
                                       out.fmt="numeric",
                                       verbose=TRUE,...) {

  x <- as.data.frame(x)
  submonthly2monthly.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                                na.rm.max=na.rm.max,
                                dates=dates, date.fmt=date.fmt, tz=tz,
                                out.type=out.type,
                                out.fmt=out.fmt,
                                verbose=verbose,...)

} # 'submonthly2monthly.matrix  ' END
