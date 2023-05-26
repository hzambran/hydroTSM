# File subdaily2monthly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2013-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          subdaily2monthly                                                    #
################################################################################
# This function transform a (sub)DAILY regular time series into a MONTHLY one

# 'x'   : daily values that will be converted into monthly ones.
#         class(x) must be zoo/xts
# 'FUN' : Function that have to be applied for transforming from daily into 
#         monthly time step
#         For precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

subdaily2monthly <-function(x, ...) UseMethod("subdaily2monthly")

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates: 25-May-2033                                                         # 
################################################################################
subdaily2monthly.default <- function(x, FUN, na.rm=TRUE, start="00:00:00", 
                                     start.fmt= "%H:%M:%S", tz, ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

  # Automatic detection of 'tz'
  if (missing(tz))
    tz <- format(time(x), "%Z")[1]

  subdaily2monthly.zoo(x=x, FUN=fun, na.rm=na.rm, start=start, start.fmt=start.fmt, tz=tz, ...)

} # 'subdaily2monthly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates: 25-May-2023                                                         # 
################################################################################
subdaily2monthly.zoo <- function(x, FUN, na.rm=TRUE, start="00:00:00", 
                                 start.fmt= "%H:%M:%S", tz, ...) {

  # testing the existence of 'na.rm' argument
  #args <- list(...)
  #exist <- "na.rm" %in% names(args)
  #exist

#  # Checking that the user provied a valid class for 'x'   
#  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")
#
#  # Automatic detection of 'tz'
#  if (missing(tz))
#    tz <- format(time(x), "%Z")[1]
#
#  d <- subdaily2daily.zoo(x=x, FUN=fn, na.rm=na.rm, start=start, start.fmt=start.fmt, tz=tz, ...)
#
#  daily2monthly.zoo(x=d, FUN=fn, na.rm=na.rm, ...)

    # testing the existence of 'na.rm' argument
    #args <- list(...)
    #exist <- "na.rm" %in% names(args)
    #exist

    # Checking that the user provied a valid class for 'x'   
    if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !!")

    # Checking the user provide a valid value for 'FUN'
    if (missing(FUN))
      stop("Missing argument: 'FUN' must contain a valid function for aggregating the sub-daily values")

    # Automatic detection of 'tz'
    if (missing(tz)) tz <- ""

    # Transforming the original time into a POSIXct object
    time.old <- time(x)

    # Converting the new staring time provided by the user into a POSIXct object
    start <- as.POSIXct(start, format=start.fmt, tz=tz)

    # normal staring time for a day
    nstart <- as.POSIXct("00:00:00", format="%H:%M:%S", tz=tz)

    # time difference between the desired starting time 'strat' and the "normal"
    # starting time 'nstart', [s]
    delta <- difftime(start, nstart, units="secs")

    # Computing teh time difference between 'start' and the "normal" starting time, [s]
    time.new <- as.POSIXct(time.old, tz=tz) - delta

    # Changing the time in 'x' in 'delta' seconds
    time(x)  <- time.new
     
    # Making sure that the time serie is complete before aggregation
    #if ( (format(start(x), "%H:%M:%S") != "00:00:00") | (format(end(x), "%H:%M:%S") != "00:00:00"))
    st <- paste(format(start(x), "%Y-%m-%d"), "00:00:00")
    et <- paste(format(end(x), "%Y-%m-%d"), "23:59:59")
    x <- izoo2rzoo(x, from=st, to=et, tz=tz)

    # 'as.numeric' is necessary for being able to change the names to the output
    m <- aggregate(x, by= function(tt) format(tt, "%Y-%m"), FUN=FUN, na.rm= na.rm, ...)

    # Removing subdaily time attibute, but not the dates
    if (NCOL(m) == 1) {
      m <- zoo(as.numeric(m), as.yearmon(time(m), format="%Y-%m") ) 
    } else m <- zoo(coredata(m), as.yearmon(time(m), format="%Y-%m") ) 

    # Replacing the NaNs by 'NA.
    # mean(NA:NA, na.rm=TRUE) == NaN
    nan.index <- which(is.nan(m))
    if ( length(nan.index) > 0 ) m[nan.index] <- NA
  
    # Replacing all the Inf and -Inf by NA's
    # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
    inf.index <- which(is.infinite(m))
    if ( length(inf.index) > 0 ) m[inf.index] <- NA      

    return(m)
} # 'subdaily2monthly.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates: 25-May-2023                                                         # 
################################################################################
subdaily2monthly.data.frame <- function(x, FUN, na.rm=TRUE, start="00:00:00", 
                                        start.fmt= "%H:%M:%S", tz, 
                                        dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				                                out.fmt="zoo",
				                                verbose=TRUE,...) {

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
     if ( !( inherits(dates, "numeric") | inherits(dates, "factor") | inherits(dates, "POSIXt")) )
         stop("Invalid argument: 'class(dates)' must be in c('numeric', 'factor', 'POSIXct', 'POSIXt') !")

  # Automatic detection of 'tz'
  if (missing(tz)) tz <- ""

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( TRUE && ( inherits(dates, "numeric") ) ) {
    tmp   <- dates
    dates <- as.POSIXct(x[, dates], format= date.fmt, tz=tz) 
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( TRUE && ( inherits(dates, "factor") ) ) dates <- as.POSIXct(dates, format= date.fmt) 

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( (TRUE && ( inherits(dates, "POSIXt") ) ) & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  x <- zoo::zoo(x, dates)
  

  ##############################################################################
  
  z <- subdaily2monthly.zoo(x=x, FUN=FUN, na.rm=na.rm, start=start, start.fmt=start.fmt, tz=tz, ...)
    
  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab        
  } # IF end

  return( z )

} # 'subdaily2monthly.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates: 25-May-2023                                                         # 
################################################################################
subdaily2monthly.matrix <- function(x, FUN, na.rm=TRUE, start="00:00:00", 
                                    start.fmt= "%H:%M:%S", tz, 
                                    dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				                            out.fmt="zoo",
				                            verbose=TRUE,...) {

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
     if ( !( inherits(dates, "numeric") | inherits(dates, "factor") | inherits(dates, "POSIXt")) )
         stop("Invalid argument: 'class(dates)' must be in c('numeric', 'factor', 'POSIXct', 'POSIXt') !")

  # Automatic detection of 'tz'
  if (missing(tz)) tz <- ""

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2monthly.default' instead of 'daily2monthly.data.frame'....
   subdaily2monthly.data.frame(x=x, FUN=FUN, na.rm=na.rm, start=start, 
                             start.fmt=start.fmt, tz=tz,
                             dates=dates, date.fmt=date.fmt,
			                       out.fmt=out.fmt,
                             verbose=verbose,...)

} # 'subdaily2monthly.matrix' end
