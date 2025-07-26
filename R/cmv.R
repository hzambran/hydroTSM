# File cmv.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                         cmv: Counting Missing Values                         #
################################################################################
# This function counts the percentage/amount of missing data in a zoo object, 
# using a user-defined temporal scale

# 'x'        :  zoo object to be analised
# 'tscale'   :  character with the temporal scale to be used for analysing the mssing data.
#               Valid values for 'tscale' are: 
#             -) "hourly"    :  the percentage/amount of missing values will be given for each 
#                               hour and ,therefore, the expected time frequency of 'x' must 
#                               be sub-hourly.
#             -) "daily"     :  the percentage/amount of missing values will be given for each 
#                               day and, therefore, the expected time frequency of 'x' must 
#                               be sub-daily (i.e., hourly or sub-hourly).
#             -) "weekly"    :  the percentage/amount of missing values will be given for each 
#                               week (starting on Monday) and, therefore, the expected time 
#                               frequency of 'x' must be sub-weekly (i.e., daily, (sub)hourly).
#             -) "monthly"   :  the percentage/amount of missing values will be given for each 
#                               month and, therefore, the expected time frequency of 'x' must 
#                               be sub-monthly (i.e., daily, hourly or sub-hourly).
#             -) "quarterly" :  the percentage/amount of missing values will be given for each 
#                               quarter and, therefore, the expected time frequency of 'x' must 
#                               be sub-quarterly (i.e., monthly, daily, hourly or sub-hourly).
#             -) "seasonal"  :  the percentage/amount of missing values will be given for each 
#                               weather season (see ?time2season) and, therefore, the expected 
#                               time frequency of 'x' must be sub-seasonal (i.e., monthly, daily, 
#                               hourly or sub-hourly).
#             -) "annual"    :  the percentage/amount of missing values will be given for each 
#                               year and, therefore, the expected time frequency of 'x' must 
#                               be sub-annual (i.e., seasonal, monthly, daily, hourly or sub-hourly).
#             -) 'out.type'  :  character indicating how should be returned the missing values 
#                               for each temporal scale. Valid values for 'out'type' are: 
#             -) "percentage":  the missing values are returned as an real value, representing
#                               the percentage of missing values in each temporal scale. 
#             -) "amount"    :  the missing values are returned as an integer value, representing
#                               the absolute amount of missing values in each temporal scale. 
#             -) 'dec'       :  integer indicating the amount of decimal places included in the output. 
#                               It is only used when "out.type=='percentage'"
#             -) 'start'     :  character, indicating the starting time used for aggregating sub-daily time 
#                               series into daily ones. 
#                               It MUST be provided in the format specified by \code{start.fmt}. \cr
#                               This value is used to define the time when a new day begins (e.g., for some 
#                               rain gauge stations). \cr
#                               -) All the values of \code{x} with a time attribute before \code{start} are
#                                  considered as belonging to the day before the one indicated in the time 
#                                  attribute of those values. \cr
#                               -) All the values of \code{x} with a time attribute equal to \code{start} 
#                                  are considered to be equal 
#                                  to \code{"00:00:00"} in the output zoo object. \cr
#                               -) All the values of \code{x} with a time attribute after \code{start} are 
#                                  considered as belonging to the same day as the one indicated in the time 
#                                  attribute of those values. \cr
#                                  It is useful when the daily values start at a time different from
#                                  \code{"00:00:00"}. Use with caution. See examples.
#             -) 'start.fmt' :  character indicating the format in which the time is provided in \code{start}.
#                               By default \code{date.fmt=\%H:\%M:\%S}. See \code{format} in 
#                               \code{\link[base]{as.POSIXct}}.
#             -) 'tz'        :  character, with the specification of the time zone used in both 
#                               \code{x} and \code{start}. 
#                               System-specific (see time zones), but \code{""} is the current time zone,
#                               and \code{"GMT"} is UTC (Universal Time, Coordinated). 
#                               See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
#                               If \code{tz} is missing (the default), it is automatically set to the
#                               time zone used in \code{time(x)}. \cr
#                               This argument can be used to force using the local time zone or any other 
#                               time zone instead of UTC as time zone.
#             -) 'start.month': numeric in [1,..,12], representing the starting month (1:Jan, ..., 12:Dec) 
#                               to be used in the computation of annual values. By default 'start.month=1'.


cmv <-function(x, ...) UseMethod("cmv")


################################################################################
#                              cmv.default                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates: 28-Jul-2023 ; 03-Aug-2023 ; 27-Nov-2023                             #   
#          03-May-2025 (EGU 2025)                                              #
################################################################################
cmv.default <- function(x, 
	                      tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                      out.type=c("percentage", "amount"),
	                      dec=3, 
                        start="00:00:00", 
                        start.fmt= "%H:%M:%S", 
                        tz,
                        start.month=1,
                        ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

  # checking 'tscale'
  tscale <- match.arg(tscale)

  # checking 'out.type'
  out.type <- match.arg(out.type)

  cmv.zoo(x, tscale=tscale, out.type=out.type, dec=dec,
          start=start, start.fmt=start.fmt, ...)
     
} # 'cmv.default' end



################################################################################
#                              cmv.zoo                                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates: 28-Jul-2023 ; 03-Aug-2023 ; 27-Nov-2023                             #
#          03-May-2025 (EGU 2025) ; 26-Jul-2025                                #
################################################################################
cmv.zoo <- function(x, 
	                tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                out.type=c("percentage", "amount"),
	                dec=3,
                    start="00:00:00", 
                    start.fmt= "%H:%M:%S", 
                    tz,
                    start.month=1,
                    ...) {

  # checking 'tscale'
  tscale <- match.arg(tscale)

  # checking 'out.type'
  out.type <- match.arg(out.type)

  # Checking 'dec'
  if ( trunc(abs(dec) -  abs(dec) ) != 0 )
    stop("Invalid argument: 'dec' must be integer !")

  # Checking that 'na.rm.max' is in [1, 12]
  if ( (start.month < 1) | (start.month > 12) | 
       ( trunc(abs(start.month) -  abs(start.month) ) != 0 ) )
    stop("Invalid argument: 'start.month' must be an integer in [1, 12] !")

  # Automatic detection of 'tz'
  #if (missing(tz)) tz <- ""
  if (missing(tz)) tz <- format(time(x), "%Z")[1]

  # Getting the original time index for each element in 'x'
  time.old  <- time(x)

  # Analysis of days different from 00:00 to 23:59 hrs
  if ( start != "00:00:00" ) {
    # Converting the new starting time provided by the user into a POSIXct object
    start <- as.POSIXct(start, format=start.fmt, tz=tz)

    # normal staring time for a day
    nstart <- as.POSIXct("00:00:00", format="%H:%M:%S", tz=tz)

    # time difference between the desired starting time 'strat' and the "normal"
    # starting time 'nstart', [s]
    delta <- difftime(start, nstart, units="secs")

    # Computing teh time difference between 'start' and the "normal" starting time, [s]
    #time.new <- as.POSIXct(time.old, tz=tz) - delta
    time.new <- time.old - delta

    # Changing the time in 'x' in 'delta' seconds
    time(x)  <- time.new
  } # IF end

  # Getting the year corresponding to each element in 'x'
  years  <- format( time.old, "%Y")

  # Shifting backwards the year each element in 'x', 
  # only when start.month != 1
  if ( start.month != 1 )
    years <- shiftyears(ltime=time.old, lstart.month=start.month)

  # Checking that the time frequency of 'x' is compatible with 'tscale'
  if ( tscale == "hourly") {
  	if ( !(sfreq(x) %in% c("minute") ) )
  	  stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
  } else if ( tscale == "daily") {
  	  if ( !(sfreq(x) %in% c("minute", "hourly") ) )
  	    stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
    } else if ( tscale == "weekly") {
  	    if ( !(sfreq(x) %in% c("minute", "hourly", "daily") ) )
  	      stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
      } else if ( tscale == "monthly") {
  	      if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly") ) )
  	        stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
        } else if ( tscale == "quarterly") {
  	        if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly") ) )
  	          stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
          } else if ( tscale == "seasonal") {
  	          if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly") ) )
  	            stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
            } else if ( tscale == "annual") {
  	            if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly", "quarterly", "seasonal") ) )
  	              stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
              } # else END

  
  # Computing the total amount of data at the desired temporal scale
  fun <- length
  ndata <- switch( tscale,  
    "hourly"    = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d %H"), FUN=fun),  
    "daily"     = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d")   , FUN=fun),  
    "weekly"    = aggregate(x, by= function(tt) format(tt, "%Y-%W")      , FUN=fun),  # week starting on Monday 
    "monthly"   = aggregate(x, by= function(tt) format(tt, "%Y-%m")      , FUN=fun),  
    "quarterly" = aggregate(x, by= function(tt) zoo::format.yearqtr(tt)  , FUN=fun),  
    "seasonal"  = aggregate(x, by= function(tt) paste0(format(tt, "%Y"), "-", time2season(tt)), FUN=fun),  
    #"annual"    = aggregate(x, by= function(tt) format(tt, "%Y")         , FUN=fun)
    "annual"    = aggregate(x, by= years                                 , FUN=fun)
  ) # 'md' END

  # Function for obtaining the amount of missing values in 'x'
  smv <- function(x) {
    na.index <- is.na(x)
    return( sum(na.index) )
  } # 'smv' END

  # Computing the amount of missing values at the desired temporal scale
  fun <- smv
  nNA<- switch( tscale,  
    "hourly"    = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d %H"), FUN=fun),  
    "daily"     = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d")   , FUN=fun),  
    "weekly"    = aggregate(x, by= function(tt) format(tt, "%Y-%W")      , FUN=fun),  # week starting on Monday 
    "monthly"   = aggregate(x, by= function(tt) format(tt, "%Y-%m")      , FUN=fun),  
    "quarterly" = aggregate(x, by= function(tt) zoo::format.yearqtr(tt)  , FUN=fun),  
    "seasonal"  = aggregate(x, by= function(tt) paste0(format(tt, "%Y"), "-", time2season(tt)), FUN=fun),  
    #"annual"    = aggregate(x, by= function(tt) format(tt, "%Y")         , FUN=fun)
    "annual"    = aggregate(x, by= years                                 , FUN=fun)
  ) # 'md' END

  if (out.type=="percentage") {
  	out <- nNA/ndata
  	out <- round(out, dec)
  } else out <- nNA  # out.type=="amount"

  return(out)

} # 'cmv.zoo' END



################################################################################
#                              cmv.data.frame                                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates: 28-Jul-2023 ; 03-Aug-2023 ; 27-Nov-2023                             #
#          03-May-2025 (EGU 2025)                                              #
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
# 'date.fmt': character indicating the format in which the dates are stored in 'dates'.
#             ONLY required when class(dates)=="factor" or "numeric"
# 'verbose' : logical; if TRUE, progress messages are printed
cmv.data.frame <- function(x, tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                         out.type=c("percentage", "amount"),
	                         dec=3,
                           start="00:00:00", 
                           start.fmt= "%H:%M:%S", 
                           tz,
                           start.month=1,
                           dates=1, 
                           date.fmt="%Y-%m-%d", ...) {
                                    
  # Checking that the user provied a valid argument for 'dates'
  if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
    stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( inherits(dates, "numeric") ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt) # zoo::as.Date
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( inherits(dates, "factor") ) dates <- as.Date(dates, format= date.fmt) # zoo::as.Date

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( inherits(dates, "Date") ) & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  tryCatch(
          #this is the chunk of code we want to run
          { x <- zoo(x, dates)
          #when it throws an error, the following block catches the error
          }, error = function(msg){return(NA)}
          )
  
  cmv.zoo(x=x, tscale=tscale, out.type=out.type, dec=dec, 
          start=start, start.fmt=start.fmt, tz=tz,
          start.month=start.month)

 } #'cmv.data.frame' END


################################################################################
#                                cmv.matrix                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates: 28-Jul-2023 ; 03-Aug-2023 ; 27-Nov-2023                             #
#          03-May-2025 (EGU 2025)                                              #
################################################################################
cmv.matrix  <- function(x, tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                      out.type=c("percentage", "amount"),
	                      dec=3,
                        start="00:00:00", 
                        start.fmt= "%H:%M:%S", 
                        tz,
                        start.month=1,
                        dates=1, 
                        date.fmt="%Y-%m-%d", ...) {

   x <- as.data.frame(x)
   cmv.data.frame(x=x, tscale=tscale, out.type=out.type, dec=dec,
                  start=start, start.fmt=start.fmt, tz=tz,
                  start.month=start.month,
                  dates=dates, date.fmt=date.fmt)

} # 'cmv.matrix  ' END
