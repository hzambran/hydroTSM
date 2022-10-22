# File subhourly2nminutes.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2022-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                          subhourly2nminutes                                  #
################################################################################
# This function transform a SUB-HOURLY time series into a n-minutes one

# 'x'   : sub-hourly values that will be agreggated into n-minutes ones.
#         class(x) must be 'zoo'
# 'FUN' : Function that have to be applied for transforming from sub-hourly into 
#         n-minutes time step. E.g., for precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

subhourly2nminutes <-function(x, ...) UseMethod("subhourly2nminutes")


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-Oct-2022                                                         #
# Updates:                                                                     #
################################################################################
subhourly2nminutes.default <- function(x, nminutes, FUN, na.rm=TRUE, start, ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo'")

  subhourly2nminutes.zoo(x=x, nminutes=nminutes, FUN=FUN, na.rm=na.rm, ...)

} # 'subhourly2nminutes.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 30-Jun-2021                                                         #
# Updates: 08-Oct-2022 ; 09-Oct-2022                                           #
################################################################################
subhourly2nminutes.zoo <- function(x, nminutes, FUN, na.rm=TRUE, start, ...) {

    # testing the existence of 'na.rm' argument
    #args <- list(...)
    #exist <- "na.rm" %in% names(args)
    #exist

    # Checking that the user provied a valid class for 'x'   
    if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !!")

    # Checking that the user provied a sub-hourly 'x' object
    if ( !(sfreq(x) == "minute" ) ) stop("Invalid argument: 'x' is not a sub-hourly ts !!")

    # Checking the user provide a valid value for 'FUN'
    if (missing(FUN))
      stop("Missing argument: 'FUN' must contain a valid function for aggregating the sub-daily values")

    # Getting the amount of minutes between each 'x' value, which might be irregulary spaced
    x.nmin       <- diff(time(x))
    x.nmin.units <- units(x.nmin)
    x.nmin       <- min(x.nmin)
    if (x.nmin.units == "secs") {
      x.nmin <- as.numeric(x.nmin/60)
    } else x.nmin <- as.numeric(x.nmin)

    # Checking that the amount of minutes used for aggregation ('nminutes') is provided and larger than 'x.nmin'
    if (missing(nminutes)) {
     stop("Missing argument: 'nminutes' must be provided !")
    } else if ( nminutes <= x.nmin) 
             stop("Invalid argument: 'nminutes' must be larger than the time frequency of 'x' !!")

    lstart <- start(x)
    if ( !missing(start) )
      if (start < lstart) 
        x <- izoo2rzoo(x, from=lstart)

    # Getting the time zone of 'x'
    ltz <- format(time(x), "%Z")[1]

    lstart <- start(x)
    lend   <- end(x)
    temp   <- seq( lstart, lend, by=paste0(nminutes, " min") )

    # temporal aggregation from 'x.nmin' minutes to 'nminutes' minutes
    h <- aggregate(x, by= function(tt) cut(tt, breaks=temp), FUN=FUN, na.rm= na.rm, ...)

    # Restoring time(x) to a complete POSIX format
    h <- zoo(coredata(h), as.POSIXct(time(h), format="%Y-%m-%d %H:%M", tz=ltz ))

    # Replacing the NaNs by 'NA.
    # mean(NA:NA, na.rm=TRUE) == NaN
    nan.index <- which(is.nan(h))
    if ( length(nan.index) > 0 ) h[nan.index] <- NA
  
    # Replacing all the Inf and -Inf by NA's
    # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
    inf.index <- which(is.infinite(h))
    if ( length(inf.index) > 0 ) h[inf.index] <- NA      

    return(h)

} # 'subhourly2nminutes.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-Oct-2022                                                         #
# Updates:                                                                     #
################################################################################
# 'dates'   : "numeric", "factor", "Date" indicating how to obtain the
#             dates for correponding to the 'sname' station
#             If 'dates' is a number, it indicates the index of the column in
#                'x' that stores the dates
#             If 'dates' is a factor, it have to be converted into 'Date' class,
#                using the date format  specified by 'date.fmt'
#             If 'dates' is already of Date class, the following line verifies that
#                the number of days in 'dates' be equal to the number of element in the
#                time series corresponding to the 'st.name' station
# 'date.fmt': format in which the dates are stored in 'dates'.
#             ONLY required when class(dates)=="factor" or "numeric"
# 'out.fmt' : character, for selecting if the result will be 'numeric' or 'zoo'. Valid values are: c('numeric', 'zoo')
# 'verbose'      : logical; if TRUE, progress messages are printed
subhourly2nminutes.data.frame <- function(x, nminutes, FUN, na.rm=TRUE, start, 
                                          dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				                                  out.fmt="zoo", verbose=TRUE,...) {

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
      stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values !!")

  # Checking that the user provied a valid argument for 'out.fmt'
  if (is.na(match( out.fmt, c("numeric", "zoo") ) ) )
      stop("Invalid argument: 'out.fmt' must be in c('numeric', 'zoo')")

  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
     # Checking that the user provied a valid argument for 'dates'
     if (FALSE && (class(dates) %in% c("numeric", "factor", "POSIXct", "POSIXt")) )
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'POSIXct', 'POSIXt'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( TRUE && ( inherits(dates, "numeric") ) ) {
    tmp   <- dates
    dates <- as.POSIXct(x[, dates], format= date.fmt) 
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( TRUE && ( inherits(dates, "factor") ) ) dates <- as.POSIXct(dates, format= date.fmt) 

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( (TRUE && (class(dates) %in% c("POSIXct", "POSIXt")) ) & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  x <- zoo::zoo(x, dates)
  
  ##############################################################################
  
  z <- subhourly2nminutes.zoo(x=x, nminutes=nminutes, FUN=FUN, na.rm=na.rm, ...)
    
  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab        
  } # IF end

  return( z )

 } #'subhourly2nminutes.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-Oct-2022                                                         #
# Updates:                                                                     #
################################################################################
subhourly2nminutes.matrix  <- function(x, nminutes, FUN, na.rm=TRUE, start,
                                       dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				                               out.fmt="zoo",
                                       verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2monthly.default' instead of 'daily2monthly.data.frame'....
   subhourly2nminutes.data.frame(x=x, nminutes=nminutes, FUN=FUN, na.rm=na.rm,
                                 dates=dates, date.fmt=date.fmt,
			                           out.fmt=out.fmt, verbose=verbose,...)

} # 'subhourly2nminutes.matrix  ' END
