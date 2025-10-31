# File izoo2rzoo.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2009-2025 Mauricio Zambrano-Bigiarini
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
# tz        : specification of the time zone used for 'time(x)', 'from', 'to'
#             System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} (the default value) is UTC (Universal Time, Coordinated). 
#             See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
#             This argument can be used when working with subdaily zoo objects to force using UTC instead of the local time zone
# na.action: character, indicating whether to keep 'NA' values (default) or interpolate them with linear or spline interpolation

izoo2rzoo <-function(x, ...) UseMethod("izoo2rzoo")


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          09-Oct-2022                                                         #
#          03-Aug-2023                                                         #
################################################################################ 
izoo2rzoo.default <- function(x, from= start(x), to= end(x), 
                              date.fmt, tstep, tz, 
                              na.action=c("keep", "linear", "spline"), ... ) {

  # Checking that the user provied a valid class for 'x'    
  if ( !is.zoo(x) )  
     stop("Invalid argument: 'class(x)' must be 'zoo' !")

  izoo2rzoo.zoo(x=x, from=from, to=to, date.fmt=date.fmt, tstep=tstep, tz=tz, 
                na.action=na.action, ... )

} # 'izoo2rzoo.default' END


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          07-May-2012                                                         #
#          16-Oct-2012                                                         #
#          29-May-2013                                                         #
#          17-Jun-2022 ; 20-Jun-2022 ; 08-Oct-2022 ; 09-Oct-2022 ; 11-Oct-2022 #
#          12-Oct-2022                                                         #
#          25-May-2023 ; 03-Aug-2023 ; 04-Nov-2023 ; 16-Nov-2023 ; 17-Nov-2023 #
#          25-Nov-2023                                                         #
#          28-Apr-2024                                                         #
#          29-Oct-2025                                                         #
################################################################################ 

izoo2rzoo.zoo <- function(x, from= start(x), to= end(x), 
                          date.fmt, tstep, tz, 
                          na.action=c("keep", "linear", "spline"), ... ) {

  if (!is.zoo(x)) stop("Invalid argument: 'x' must be of class 'zoo' !")

  # Checking 'na.action' argument
  na.action <- match.arg(na.action)

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)

  # Date/Time of 'x'
  tx <- time(x)

  # Cheking if 'x is a sub-daily zoo object  
  if (x.freq %in% c("minute","hourly") ) {
    subdaily.ts <- TRUE
  } else subdaily.ts <- FALSE

  # Defining the 'tstep' value
  if ( missing(tstep) )
    switch(x.freq,
           minute = {tstep <- "1 min"},
           hourly = {tstep <- "1 hour"},
           daily = {tstep <- "1 day"},
           weeekly = {tstep <- "1 week"},
           monthky = {tstep <- "1 month"},
           quarterly = {tstep <- "1 quarter"},
           annual = {tstep <- "1 year"}
           )

  # Automatic detection of 'date.fmt'
  if ( missing(date.fmt) ) {
    if ( subdaily.ts ) {
      date.fmt <- "%Y-%m-%d %H:%M:%S"
    } else date.fmt <- "%Y-%m-%d"
  } # IF end

  # Automatic detection of 'tz'
  missingTZ <- FALSE
  if (missing(tz)) {
    missingTZ <- TRUE
    tz        <- attr(tx, "tzone")
  } else {
      # For the Date/Time of 'x' to be in the time zone specified by 'tz'
      tx.new  <- timechange::time_force_tz(tx, tz=tz)
      time(x) <- tx.new
    } # ELSE end
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE), 
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )

  # If the index of 'x' is character, it is converted into a Date object
  if ( class(time(x))[1] %in% c("factor", "character") )
    #ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt, tz=tz),
    ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt),
                              time(x) <- as.Date(time(x), format=date.fmt) )

  # If 'from' was given as Date, but 'x' is sub-daily
  if (!missing(from)) {
    if (from > to) stop("Invalid argument: 'from > to' !")

    if (from > end(x)) stop("Invalid argument: 'from > end(x)' !")

    if ( subdaily.date.fmt & !(grepl(":", from, fixed=TRUE) ) )
      from <- paste(from, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      from <- as.POSIXct(from, tz=tz)
  } # IF end

  # If 'to' was given as Date, but 'x' is sub-daily
  if (!missing(to)) {
    if (to < from ) stop("Invalid argument: 'to < from' !")

    if (to < start(x) ) stop("Invalid argument: 'to < start(x)' !")

    if ( subdaily.date.fmt & !(grepl(":", to, fixed=TRUE) ) )
      to <- paste(to, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      to <- as.POSIXct(to, tz=tz)
  } # IF end
        
  # checking that date.fmt and the sampling frequency of 'x' are compatible 
  if ( subdaily.ts ) {
    if (!subdaily.date.fmt) 
      stop("Invalid argument: 'date.fmt' (", date.fmt, 
           ") is not compatible with a sub-daily time series !!")
  } else {
           if (subdaily.date.fmt) {
             #time(x) <- as.POSIXct(time(x), tz=tz)
             time(x) <- as.POSIXct(time(x))
             warning("'date.fmt' (", date.fmt, ") is sub-daily, while 'x' is a '", 
                     x.freq, "' ts => 'time(x)=as.POSIXct(time(x), tz)'")
           } # IF end    
          } # ELSE end

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(from, format=date.fmt, tz=tz))
    #dt <-  try(as.POSIXct(from, format=date.fmt))
  } else dt <- try(as.Date(from, format=date.fmt))

  if("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'from' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      from <- as.POSIXct(from, format=date.fmt, tz=tz)
      #from <- as.POSIXct(from, format=date.fmt)
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
  dates.reg <- seq(from=from, to=to, by= tstep)
  #na.zoo    <- zoo(rep(NA, length(dates.reg)), dates.reg)
  x.reg     <- merge(x, zoo(, dates.reg))

  # If the user wants to fill in NA values
  if (na.action == "linear") {
    x.reg <- zoo::na.approx(x.reg, na.rm=FALSE)
  } else if (na.action == "spline") {
      x.reg <- zoo::na.spline(x.reg, na.rm=FALSE)
    } # ELSE end

  # Selecting only those data within the time period between 'from' and 'to'
  x.sel <- window(x.reg, start=from, end=to)

  ## Creating a regular time series with NA's in dates.reg in which 'x' has no data
  #x.merged <- merge(na.zoo, x.sel)
  
  ## Removing the fictitious column corresponding to 'na.zoo'
  #x.merged <- x.merged[,-1]
  
  # Giving the same column names than the original 'x'
  if ( is.matrix(x) | is.data.frame(x) )
    colnames(x.sel) <- colnames(x)

  # Returning only the column containing the Regular ts with NA's in the empty records
  return( x.sel )

} # 'izoo2rzoo.zoo' end
