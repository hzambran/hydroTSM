# File izoo2rzoo.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2009-2022 Mauricio Zambrano-Bigiarini
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
#             \code{from} and \code{to}, e.g. "%Y-%m-%d"
#             ONLY required when class(dates)=="factor" or "numeric"
# from      : starting date for the merged output
# to	      : ending date for the merged output
# tstep     : time step in which are stored the values of 'x'
# tz        : specification of the desired time zone yo be used. 
#             System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} (the default value) is UTC (Universal Time, Coordinated). 
#             See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
#             This argument can be used when working with subdaily zoo objects to force using the local time zone instead of GMT as time zone.

izoo2rzoo <-function(x, ...) UseMethod("izoo2rzoo")


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          09-Oct-2022                                                         #
################################################################################ 
izoo2rzoo.default <- function(x, from= start(x), to= end(x), 
                              date.fmt, tstep, tz="UTC", ...) {

  # Checking that the user provied a valid class for 'x'   
  valid.class <- c("xts", "zoo")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
     stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

  izoo2rzoo.zoo(x=x, from=from, to=to, date.fmt=date.fmt, tstep=tstep, tz=tz, ...)

} # 'izoo2rzoo.default' END


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          07-May-2012                                                         #
#          16-Oct-2012                                                         #
#          29-May-2013                                                         #
#          17-Jun-2022 ; 20-Jun-2022 ; 08-Oct-2022 ; 09-Oct-2022               #
################################################################################ 

izoo2rzoo.zoo <- function(x, from= start(x), to= end(x), 
                          date.fmt, tstep, tz="UTC", ... ) {

  if (!is.zoo(x)) stop("Invalid argument: 'x' must be of class 'zoo'")

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)
        
  # Cheking if 'x is a sub-daily zoo object
  if (x.freq %in% c("minute","hourly") ) {
    subdaily.ts <- TRUE
    tstep <- "hours"
  } else {
      subdaily.ts <- FALSE
      tstep <- "days"
    } # ELSE end

  # Automatic detection of 'date.fmt'
  if ( missing(date.fmt) ) {
    if ( subdaily.ts ) {
      date.fmt <- "%Y-%m-%d %H:%M:%S"
    } else date.fmt <- "%Y-%m-%d"
  } # IF end
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE), 
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )

  # If the index of 'x' is character, it is converted into a Date object
  if ( class(time(x))[1] %in% c("factor", "character") )
    ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt, tz=tz),
                              time(x) <- as.Date(time(x), format=date.fmt) )

  # If 'from' was given as Date, but 'x' is sub-daily
  if (!missing(from)) {
    if (from > to) stop("Invalid argument: 'from > end(x)' OR'from > to' !")

    if ( subdaily.date.fmt & !(grepl(":", from, fixed=TRUE) ) )
      from <- paste(from, "00:00:00")
  } # IF end

  # If 'to' was given as Date, but 'x' is sub-daily
  if (!missing(to)) {
    if (to < from ) stop("Invalid argument: 'to < start(x)' |OR 'to < from' !")

    if ( subdaily.date.fmt & !(grepl(":", to, fixed=TRUE) ) )
      to <- paste(to, "00:00:00")
  } # IF end
        
  # checking that date.fmt and the sampling frequency of 'x' are compatible 
  if ( subdaily.ts ) {
    if (!subdaily.date.fmt) 
      stop("Invalid argument: 'date.fmt' (", date.fmt, 
           ") is not compatible with a sub-daily time series !!")
  } else {
           if (subdaily.date.fmt) {
             time(x) <- as.POSIXct(time(x), tz=tz)
             warning("'date.fmt' (", date.fmt, ") is sub-daily, while 'x' is a '", 
                     x.freq, "' ts => 'time(x)=as.POSIXct(time(x), tz)'")
           } # IF end    
          } # ELSE end

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(from, format=date.fmt, tz=tz))
  } else dt <- try(as.Date(from, format=date.fmt))
  if("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'from' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      from <- as.POSIXct(from, format=date.fmt, tz=tz)
    } else from <- as.Date(from, format=date.fmt)

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(to, format=date.fmt, tz=tz))
  } else dt <- try(as.Date(to, format=date.fmt))
  if("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'to' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      to <- as.POSIXct(to, format=date.fmt, tz=tz)
    } else to <- as.Date(to, format=date.fmt)

  # Creating a regular time series with NA's in all dates in [from, to]
  dates  <- seq(from=from, to=to, by= tstep)
  na.zoo <- zoo(rep(NA, length(dates)), dates)

  # Selecting only those data within the time period between 'from' and 'to'
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
