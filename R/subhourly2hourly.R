# File subhourly2hourly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2021-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                            subhourly2hourly                                    #
################################################################################
# This function transform a SUB-HOURLY time series into a HOURLY one

# 'x'   : sub-hourly values that will be converted into hourly ones.
#         class(x) must be 'xts'
# 'FUN' : Function that have to be applied for transforming from sub-hourly into 
#         hourly time step. E.g., for precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

subhourly2hourly <-function(x, ...) UseMethod("subhourly2hourly")


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 30-Jun-2021                                                         #
# Updates: 08-Oct-2022 ; 15-Oct-2022                                           #
#          27-Nov-2023                                                         #
################################################################################
subhourly2hourly.default <- function(x, FUN, na.rm=TRUE, na.rm.max=0, ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo'")

  subhourly2hourly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)

} # 'subhourly2hourly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 30-Jun-2021                                                         #
# Updates: 08-Oct-2022 ; 09-Oct-2022 ; 15-Oct-2022                             #
#          27-Nov-2023                                                         #
#          23-Jul-2024                                                         # 
#          05-Feb-2025                                                         #
################################################################################
subhourly2hourly.zoo <- function(x, FUN, na.rm=TRUE, na.rm.max=0, tz, ...) {

    # testing the existence of 'na.rm' argument
    #args <- list(...)
    #exist <- "na.rm" %in% names(args)
    #exist

    # Checking that the user provied a valid class for 'x'   
    if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !!")

    # Checking the user provide a valid value for 'FUN'
    if (missing(FUN))
      stop("Missing argument: 'FUN' must contain a valid function for aggregating the sub-hourly values")   

    # Automatic detection of 'tz'
    #ltz <- format(time(x), "%Z")[1]
    #ltz <- ""
    tx <- time(x)
    missingTZ <- FALSE
    if (missing(tz)) {
      missingTZ <- TRUE
      tz        <- attr(tx, "tzone")
    } else {
        # For the Date/Time of 'x' to be in the time zone specified by 'tz'
        tx.new  <- timechange::time_force_tz(tx, tz=tz)
        time(x) <- tx.new
      } # ELSE end

    # Computing the Hourly time series 
    tmp <- aggregate(x, by= function(tt) format(tt, "%Y-%m-%d %H"), FUN=FUN, na.rm= na.rm, ...)

    # Restoring time(x) to a complete POSIX format
    tmp <- zoo(coredata(tmp), as.POSIXct(time(tmp), format="%Y-%m-%d %H", tz=tz ))


    # Removing annual values in the output object for days with 
    # more than 'na.rm.max' percentage of NAs in a given day
    if ( na.rm & (na.rm.max != 0) ) {

      # Checking that 'na.rm.max' is in [0, 1]
      if ( (na.rm.max < 0) | (na.rm.max > 1) )
        stop("Invalid argument: 'na.rm.max' must be in [0, 1] !")

      # Computing the percentage of missing values in each hour
      na.pctg <- cmv(x, tscale="hourly", tz=tz)

      # identifying hours with a percentage of missing values higher than 'na.rm.max'
      na.pctg.index <- which( na.pctg >= na.rm.max)

      # Setting as NA all the days with a percentage of missing values higher than 'na.rm.max'
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

} # 'subhourly2hourly.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 30-Jun-2021                                                         #
# Updates: 23-Aug-2022 ; 08-Oct-2022 ; 15-Oct-2022                             #
#          27-Nov-2023                                                         #
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
subhourly2hourly.data.frame <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
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
  
  z <- subhourly2hourly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)
    
  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab        
  } # IF end

  return( z )

 } #'subhourly2hourly.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 30-Jun-2021                                                         #
# Updates: 09-Oct-2022 ; 15-Oct-2022                                           #
#          27-Nov-2023                                                         #
################################################################################
subhourly2hourly.matrix  <- function(x, FUN, na.rm=TRUE, na.rm.max=0,
                                     dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				                     out.fmt="zoo", verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2monthly.default' instead of 'daily2monthly.data.frame'....
   subhourly2hourly.data.frame(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max,
                               dates=dates, date.fmt=date.fmt,
			                   out.fmt=out.fmt, verbose=verbose,...)

} # 'subhourly2hourly.matrix  ' END
