# File shiftyears.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# .check_from_and_to_and_subset:                                               #
################################################################################
# This function checks the validity of the 'from' and 'to' arguments before    # 
# being used to subset a zoo object                                            #
################################################################################
# This is an internal function, just developed to be used by some functions    #
# that needs to perform this checking                                          #
################################################################################
# Output: zoo object obtined from window(x, start=from, end=to), with 'from' 
#         and 'to' checked in relation to 'x' and 'date.fmt'

check_from_and_to_and_subset <- function(x, from= start(x), to= end(x), 
                   date.fmt=NULL, tz=NULL) {

  # Date/Time of 'x'
  tx <- time(x)

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)

  # Cheking if 'x is a sub-daily zoo object  
  if (x.freq %in% c("minute","hourly") ) {
    subdaily.ts <- TRUE
  } else subdaily.ts <- FALSE

  # Automatic detection of 'date.fmt'
  if ( is.null(date.fmt) ) {
    if ( subdaily.ts ) {
      date.fmt <- "%Y-%m-%d %H:%M:%S"
    } else date.fmt <- "%Y-%m-%d"
  } # IF end

  # Automatic detection of 'tz'
  missingTZ <- FALSE
  if (is.null(tz)) {
    missingTZ <- TRUE
    tz        <- attr(tx, "tzone")
  } else {
      if ( subdaily.ts ) {
        # For the Date/Time of 'x' to be in the time zone specified by 'tz'
        tx.new  <- timechange::time_force_tz(tx, tz=tz)
        time(x) <- tx.new
      } # IF end
    } # ELSE end
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE), 
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )

  # If the index of 'x' is character, it is converted into a Date object
  if ( is.factor(x) | is.character(x) )
    ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt),
                              time(x) <- as.Date(time(x), format=date.fmt) )
      
  
  # If 'from' was given as Date, but 'x' is sub-daily
  if (!missing(from)) {

    if ( from > to ) stop("Invalid argument: 'from > to' !")

    if ( from < start(x) ) stop("Invalid argument: 'from < start(x)' !")

    if ( from > end(x) ) stop("Invalid argument: 'from > end(x)' !")

    if ( subdaily.date.fmt & !(grepl(":", from, fixed=TRUE) ) )
      from <- paste(from, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      from <- as.POSIXct(from, tz=tz)
  } # IF end

  # If 'to' was given as Date, but 'x' is sub-daily
  if (!missing(to)) {
    if ( to < from ) stop("Invalid argument: 'to < from' !")

    if ( to > end(x) ) stop("Invalid argument: 'to > end(x)' !")

    if ( to < start(x) ) stop("Invalid argument: 'to < start(x)' !")

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
  } else dt <- try(as.Date(from, format=date.fmt))

  if ("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'from' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      from <- as.POSIXct(from, format=date.fmt, tz=tz)
    } else from <- as.Date(from, format=date.fmt)

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(to, format=date.fmt, tz=tz))
  } else dt <- try(as.Date(to, format=date.fmt))

  if ("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'to' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      to <- as.POSIXct(to, format=date.fmt, tz=tz)
    } else to <- as.Date(to, format=date.fmt)

  return ( window(x, start=from, end=to) )

} # END 'check_from_and_to_and_subset'
