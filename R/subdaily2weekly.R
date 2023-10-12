# File subdaily2weekly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          subdaily2weekly                                                    #
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

subdaily2weekly <-function(x, ...) UseMethod("subdaily2weekly")

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Oct-2023                                                         #
# Updates:                                                                     # 
################################################################################
subdaily2weekly.default <- function(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                                    start.fmt= "%H:%M:%S", tz, ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

  # Automatic detection of 'tz'
  #if (missing(tz)) tz <- ""
  if (missing(tz)) tz <- format(time(x), "%Z")[1]

  subdaily2weekly.zoo(x=x, FUN=FUN, na.rm=na.rm, start=start, start.fmt=start.fmt, tz=tz, ...)

} # 'subdaily2weekly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Oct-2023                                                         #
# Updates:                                                                     # 
################################################################################
subdaily2weekly.zoo <- function(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
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
    #if (missing(tz)) tz <- ""
    if (missing(tz)) tz <- format(time(x), "%Z")[1]

    # Analysis of days different from 00:00 to 23:59 hrs
    if ( start != "00:00:00" ) {
      # Storing the original time
      time.old <- time(x)

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

     
    # Making sure that the time serie is complete before aggregation
    # This is useful when the first element of 'x' is not given at the time defined by 'start'.
    # For example, if the first element of 'x' starts at 08:00:00 hrs, but 'start=00:00:00', 
    # what happens with all the values from 00:00:00 to 07:59:59 hrs?
    # The following lines of code makes sure that the missing elements in a day are actually 
    # considered as missing

    st <- paste(format(start(x), "%Y-%m-%d"), "00:00:00", tz)
    et <- paste(format(end(x), "%Y-%m-%d"), "23:59:59", tz)
    x  <- izoo2rzoo(x, from=st, to=et, tz=tz)


    # Computing the Weekly time series 
    tmp <- aggregate(x, by= function(tt) format(tt, "%Y-%W"), FUN=FUN, na.rm= na.rm, ...)

    # Removing weekly values in the output object for weeks with 
    # more than 'na.rm.max' percentage of NAs in a given week
    if ( na.rm & (na.rm.max != 0) ) {

      # Checking that 'na.rm.max' is in [0, 1]
      if ( (na.rm.max < 0) | (na.rm.max > 1) )
        stop("Invalid argument: 'na.rm.max' must be in [0, 1] !")

      # Computing the percentage of missing values in each week
      na.pctg <- cmv(x, tscale="weekly")

      # identifying weeks with a percentage of missing values higher than 'na.rm.max'
      na.pctg.index <- which( na.pctg >= na.rm.max)

      # Setting as NA all the weeks with a percentage of missing values higher than 'na.rm.max'
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

    # Removing subdaily time attibute, but not the dates
    if (NCOL(tmp) == 1) {
      tmp <- zoo(as.numeric(tmp), as.yearmon(time(tmp), format="%Y-%W") ) 
    } else tmp <- zoo(coredata(tmp), as.yearmon(time(tmp), format="%Y-%W") )    

    return(tmp)
} # 'subdaily2weekly.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Oct-2023                                                         #
# Updates:                                                                     # 
################################################################################
subdaily2weekly.data.frame <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
                                       start="00:00:00", start.fmt= "%H:%M:%S", tz, 
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
  
  z <- subdaily2weekly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, 
                           start=start, start.fmt=start.fmt, tz=tz, ...)
    
  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab        
  } # IF end

  return( z )

} # 'subdaily2weekly.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Oct-2023                                                         #
# Updates:                                                                     # 
################################################################################
subdaily2weekly.matrix <- function(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
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
   subdaily2weekly.data.frame(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, 
                              start=start, start.fmt=start.fmt, tz=tz,
                              dates=dates, date.fmt=date.fmt,
			                        out.fmt=out.fmt,
                              verbose=verbose,...)

} # 'subdaily2weekly.matrix' end
