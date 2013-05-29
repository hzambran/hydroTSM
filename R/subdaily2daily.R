# File subdaily2daily.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                            subdaily2daily                                    #
################################################################################
# This function transform a SUB-DAILY time series into a DAILY one

# 'x'   : usb-daily values that will be converted into daily ones.
#         class(x) must be 'xts'
# 'FUN' : Function that have to be applied for transforming from sub-daily into 
#         daily time step. E.g., for precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

subdaily2daily <-function(x, ...) UseMethod("subdaily2daily")


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates:                                                                     #
################################################################################
subdaily2daily.default <- function(x, FUN, na.rm=TRUE, ... ) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

  subdaily2daily.zoo(x=x, FUN=FUN, na.rm=na.rm,...)

} # 'subdaily2daily.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Mar-2013                                                         #
# Updates: 26-Mar-2013 ; 08-Apr-2013 ; 09-Apr-2013                             #
################################################################################
subdaily2daily.zoo <- function(x, FUN, na.rm=TRUE, ... ) {

     # Checking that the user provied a valid class for 'x'   
     if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'xts' !!")

     # Checking the user provide a valid value for 'FUN'
     if (missing(FUN))
       stop("Missing argument: 'FUN' must contain a valid function for aggregating the sub-daily values")

     # Daily aggregation. 
     d <- apply.daily(x=x, FUN=FUN, na.rm=na.rm) # xts::apply.daily

     # Removing time attibute, but not the dates
     if (NCOL(d) == 1) {
       d <- zoo(as.numeric(d), as.Date(format(time(d), "%Y-%m-%d") ) )
     } else d <- zoo(coredata(d), as.Date(format(time(d), "%Y-%m-%d") ) )

     # Replacing the NaNs by 'NA.
     # mean(NA:NA, na.rm=TRUE) == NaN
     nan.index <- which(is.nan(d))
     if ( length(nan.index) > 0 ) d[nan.index] <- NA
  
     # Replacing all the Inf and -Inf by NA's
     # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
     inf.index <- which(is.infinite(d))
     if ( length(inf.index) > 0 ) d[inf.index] <- NA      

     return(d)

} # 'subdaily2daily.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
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
subdaily2daily.data.frame <- function(x, FUN, na.rm=TRUE,
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
     if (FALSE && (class(dates) %in% c("numeric", "factor", "POSIXct", "POSIXt")) )
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'POSIXct', 'POSIXt'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( TRUE && (class(dates) == "numeric") ) {
    tmp   <- dates
    dates <- as.POSIXct(x[, dates], format= date.fmt) 
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( TRUE && (class(dates) == "factor") ) dates <- as.POSIXct(dates, format= date.fmt) 

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( (TRUE && (class(dates) %in% c("POSIXct", "POSIXt")) ) & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  x <- xts(x, dates)
  
  ##############################################################################
  
  z <- subdaily2daily.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)
    
  if (out.fmt == "numeric") {
     snames      <- colnames(z)
     dates.lab   <- as.character(time(z))
     z           <- coredata(z)
     colnames(z) <- snames
     rownames(z) <- dates.lab        
  } # IF end

  return( z )

 } #'subdaily2daily.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Apr-2013                                                         #
# Updates:                                                                     #
################################################################################
subdaily2daily.matrix  <- function(x, FUN, na.rm=TRUE,
                                  dates=1, date.fmt="%Y-%m-%d %H:%M:%S",
				  out.fmt="zoo",
                                  verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2monthly.default' instead of 'daily2monthly.data.frame'....
   subdaily2daily.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                            dates=dates, date.fmt=date.fmt,
			    out.fmt=out.fmt,
                            verbose=verbose,...)

} # 'subdaily2daily.matrix  ' END
