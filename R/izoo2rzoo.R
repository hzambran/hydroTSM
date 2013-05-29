# File izoo2rzoo.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################ 
#         Irregular Zoo -> Regular Zoo                                         #
################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################  

# This function takes a time series of (very likely) irregular (with
# missing dates) daily time series and then transforms it into a variable
# regulary spaced, filling the voids with some value (by default: NA)

# x         : time series of type zoo (very likely with some missing days)
# date.fmt  : character indicating the format in which the dates are stored in 
#             'dates', e.g. "%Y-%m-%d"
#             ONLY required when class(dates)=="factor" or "numeric"
# from      : starting date for the merged output
# to	    : ending date for the merged output
# tstep     : time step in which are stored the values of 'x'

izoo2rzoo <-function(x, ...) UseMethod("izoo2rzoo")


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
################################################################################ 
izoo2rzoo.default <- function(x, from= start(x), to= end(x), 
                              date.fmt="%Y-%m-%d", tstep ="days", ...) {

  # Checking that the user provied a valid class for 'x'   
  valid.class <- c("xts", "zoo")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
     stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

  izoo2rzoo.zoo(x=x, from=from, to=to, date.fmt=date.fmt, tstep=tstep, ...)

} # 'izoo2rzoo.default' END


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          07-May-2012                                                         #
#          16-Oct-2012                                                         #
#          29-May-2013                                                         #
################################################################################ 

izoo2rzoo.zoo <- function(x, from= start(x), to= end(x), 
                          date.fmt="%Y-%m-%d", tstep ="days", ... ) {

  if (!is.zoo(x)) stop("Invalid argument: 'x' must be of class 'zoo'")
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE),
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )
  
  if(subdaily.date.fmt) {
    from <- as.POSIXct(from, format=date.fmt)
    to   <- as.POSIXct(to, format=date.fmt)
  } else {
      from <- as.Date(from, format=date.fmt)
      to   <- as.Date(to, format=date.fmt)
    } # ELSE end
    
  # If the index of 'x' is character, it is converted into a Date object
  if ( class(time(x))[1] %in% c("factor", "character") )
    ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(dates, format=date.fmt),
                              time(x) <- as.Date(dates, format=date.fmt) )

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)
        
  # checking that date.fmt and the sampling frequency of 'x' are compatible 
  if (x.freq %in% c("minute","hourly") ) {
    if (!subdaily.date.fmt) stop("Invalid argument: 'date.fmt' (", date.fmt, ") is not compatible with a sub-daily time series !!")
  } else
       if (subdaily.date.fmt) {
          time(x) <- as.POSIXct(time(x))
          warning("'date.fmt' (", date.fmt, ") is sub-daily, while 'x' is a '", x.freq, "' ts => 'time(x)=as.POSIXct(time(x))'")
       } # IF end
 
  # Creating a regular time series with NA's in all dates in [from, to]
  dates  <- seq(from=from, to=to, by= tstep)
  na.zoo <- zoo(rep(NA, length(dates)), dates)

  # Selecting only those data within the time period between 'from' and 'to'
  #x.sel <- x[ time(x) %in% dates, ]
  x.sel <- window(x, start=from, end=to)

  # Creating a regular time series with NA's in dates in which 'x' has no data
  x.merged <- merge(na.zoo, x.sel)
  
  # Removing the fictitious column corresponding to 'na.zoo'
  x.merged <- x.merged[,-1]
  
  # Giving the same column names than the original 'x'
  if ( is.matrix(x) | is.data.frame(x) )
    colnames(x.merged) <- colnames(x)

  # Returning only the column containing the Regular ts with NA's in the empty records
  return( x.merged )

} # 'izoo2rzoo.zoo' end
