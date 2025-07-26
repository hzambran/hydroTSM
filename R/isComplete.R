# File isComplete.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2025-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# isComplete: Generic function for identifiying whether a zoo object with a    #
#             regular frequency (e.g., hourly, daily, monthly, annual) has a   #
#             value in each time step or not, from the beggining to the end    #
################################################################################
# Author     : Mauricio Zambrano-Bigiarini                                     #
# Contributor:                                                                 #
################################################################################
# It is completly based on a function with the same name included since 2024 in#
# the hydroRTS R package                                                       #
################################################################################
# Started: 26-Jul-20245                                                        #
# Updates:                                                                     #
################################################################################
# 'x'     : zoo object. The time attribute should have a regular time frequency 
#           (e.g., hourly, daily, monthly, annual)
# 'start' : Starting date/time for the time window used to subset \code{x}.
# 'end'   : Ending date/time for the time window used to subset \code{x}.
# 'format': Format used with \code{start} and \code{end}
# 'tz'    : character, indicating the time zone in which the date of \code{x} is located.

isComplete <- function(x, ...) UseMethod("isComplete")

isComplete.zoo <- function(x, tz, out.type=c("single", "all"), verbose=TRUE, ...) {
  
  # Checking the 'out.type' argument
  out.type <- match.arg(out.type)
  
  # Getting the time attribute of 'x'
  datetimes <- time(x)

  # if 'time(x)' is a yearmon object but stored as real numbers (terra way)
  # it is transformed into a real yearmon object
  if ( inherits(datetimes, "numeric") & !all( datetimes == trunc(datetimes) ) )
    datetimes <- zoo::as.yearmon(datetimes)

   # Automatic detection of 'tz'
  missingTZ <- FALSE
  if (missing(tz)) {
    missingTZ <- TRUE
    tz        <- attr(datetimes, "tzone")
  } else {
      # For the Date/Time of 'x' to be in the time zone specified by 'tz'
      tx.new     <- timechange::time_force_tz(datetimes, tz=tz)
      time(x)    <- tx.new
       datetimes <- time(x)
    } # ELSE end

  # Getting the total amount of elements in 'datetimes'
  nDateTimes <- length(datetimes)

  # Getting the starting DateTime object in 'x'
  dt.ini <- datetimes[1]

  # Getting the ending DateTime object in 'x'
  dt.end <- datetimes[nDateTimes]

  # Creating a temporal zoo object with the same DateTime as 'x'
  temp <- zoo::zoo(rep(1, nDateTimes), datetimes)

  # Checking whether 'x' is:
  # i)   stictly regular (regular, without missing vlaues), 
  # ii)  approximately regular (regular, with some missing values) or 
  # iii) not regular at all
  isStrictlyRegular <- zoo::is.regular(temp, strict=TRUE) # It fails for monthly objects with Date time attribute
  isApproximRegular <- zoo::is.regular(temp, strict=FALSE)

  if ( !isApproximRegular )         
    stop("[ 'x' does not have a regular time frequency => is not possible to assess whether it is complete or not ! ]") 

  ##############################################################################
  # Getting the time frequency of 'x'                                          #
  ##############################################################################

  # Detecting ANNUAL 2-annual, 3-annual (years as numeric integers, not as yearmon)
  if ( ( inherits(datetimes, "numeric") & all( datetimes == trunc(datetimes) ) ) & 
       !inherits(datetimes, "yearmon")
      ) {

    # years as integer numbers
    if ( all( datetimes == trunc(datetimes) ) ) { 

      dt.ini    <- as.Date(paste0(dt.ini, "-01-01"))
      dt.end    <- as.Date(paste0(dt.end, "-01-01"))
      dt.stg    <- "years"
      dt.val    <- round(median(diff(datetimes), na.rm=TRUE))
      if ( dt.val == 1 ) {
        x.freq    <- "annual"
      } else x.freq <- paste0(dt.val, "-annual")

      datetimes <- as.Date(paste0(datetimes, "-01-01"))

    } # IF end

  # Detecting MONTHLY objects (years as real numbers (not integers) or as yearmon objects)
  # Monthly objects can be: yearmon (zoo objects), numeric (terra objects) or Date (base R objects)
  } else if ( ( inherits(datetimes, "numeric") & !all( datetimes == trunc(datetimes) ) ) |
              inherits(datetimes, "yearmon") | 
              ( (hydroTSM::sfreq(temp) == "monthly") & (median(diff(as.Date(datetimes) ), na.rm=TRUE) > 28) )
            ) {

              if ( inherits(datetimes, "Date") ) { # Date objects
                dt.ini <- as.Date(dt.ini)
                dt.end <- as.Date(dt.end)
                dt.val <- round( median(diff(as.Date(datetimes) ), na.rm=TRUE) / 30 ) # division by 30 is for transforming from "days" to "months"
              } else if ( !inherits(datetimes, "yearmon") ) { # real number monthly objects
                  if ( dt.ini == trunc(dt.ini) ) {
                    dt.ini  <-  as.Date(paste0(dt.ini, "-01-01"))
                  } else dt.ini  <- as.Date( paste0( format(zoo::as.yearmon(dt.ini), "%Y-%m"), "-01") )
                  if ( dt.end == trunc(dt.end) ) {
                    dt.end  <-  as.Date(paste0(dt.end, "-01-01"))
                  } else dt.end <- as.Date( paste0( format(zoo::as.yearmon(dt.end), "%Y-%m"), "-01") )
                  datetimes <- zoo::as.yearmon(datetimes)
                  dt.val    <- round( median( diff(datetimes), na.rm=TRUE) * 12 )
                } else { # yearmon objects
                     dt.ini <- as.Date( paste0( format(dt.ini, "%Y-%m"), "-01") )
                     dt.end <- as.Date( paste0( format(dt.end, "%Y-%m"), "-01") )
                     dt.val <- round( median( diff(datetimes), na.rm=TRUE) * 12 ) 
                  } # ELSE end

              dt.stg    <- "months"
              
              if ( dt.val == 1 ) {
                x.freq    <- "monthly"
              } else x.freq <- paste0(dt.val, "-monthly")

    } else { # Detecting objects with time frequencies different from Annual and Monthly

        dt     <- diff(datetimes)
        dt.val <- median(dt, na.rm=TRUE)
        dt.stg <- attr(dt, "units")
        if (verbose) message("[ The exact time frequency of 'x' is: ", dt.val, " ", dt.stg )

        # Getting the time frequency of 'x', considering the particular case of the well known
        # hourly, daily, weekly, monthly, annual
        x.freq <- paste(dt.val, dt.stg)

        if ( (x.freq == "1 hours") | ( (dt.stg=="mins") & ( dt.val == 60 ) ) ) 
          x.freq <- "hourly"

        if ( (x.freq == "1 days") | ( (dt.stg=="hours") & ( dt.val == 24 ) ) )
          x.freq <- "daily"

        if ( (x.freq == "1 weeks") | ( (dt.stg=="days") & ( dt.val == 7 ) ) )
          x.freq <- "weekly"

        if ( (x.freq == "1 months") | ( (dt.stg=="days") & ( dt.val %in% c(28, 29, 30, 31) ) ) )
          x.freq <- "monthly"

        if ( (x.freq == "1 years") | ( (dt.stg=="days") & ( dt.val == 365 ) ) | ( (dt.stg=="months") & ( dt.val == 12 ) ) )
          x.freq <- "annual"

        # Some well known time frequencies
        if ( (x.freq == "hourly") & (dt.val > 1) ) 
          x.freq <- paste0(dt.val, "-hourly")

        if ( (x.freq == "daily") & (dt.val > 1) ) 
          x.freq <- paste0(dt.val, "-day")

        if ( (x.freq == "monthly") & (dt.val > 1) ) 
          x.freq <- paste0(dt.val, "-monthly")

        if (hydroTSM::sfreq(temp) == "quarterly") 
          x.freq <- "quarterly"

      } # ELSE end
  
  ##############################################################################
  # Getting the complete time elements that should be present in 'x' from 'dt.ini' to 'dt.end'
  ##############################################################################
  if ( (x.freq %in% c("hourly", "daily", "weekly", "monthly", "quarterly", "annual") ) ) {
    completeTime <- switch(x.freq, "hourly"    = hydroTSM::hip(from=dt.ini, to=dt.end, tz=tz),
                                   "daily"     = hydroTSM::dip(from=dt.ini, to=dt.end),
                                   "weekly"    = seq(from=as.Date(dt.ini), to=as.Date(dt.end), by="1 week"),
                                   "monthly"   = hydroTSM::mip(from=dt.ini, to=dt.end),
                                   "quarterly" = seq(from=as.Date(dt.ini), to=as.Date(dt.end), by="1 quarter"),
                                   "annual"    = hydroTSM::yip(from=dt.ini, to=dt.end)
                          )
    if ( (x.freq == "monthly") & (inherits(datetimes, "yearmon")) )
      completeTime <- zoo::as.yearmon(completeTime) 

  } else { # Identifying sub-daily time frequencies

          if ( (x.freq %in% c("minute", "hourly")) | grepl("hour", x.freq) | grepl("min", x.freq) ) {
             subdaily.ts <- TRUE
             completeTime <- seq(from=as.POSIXct(dt.ini, tz=tz), to=as.POSIXct(dt.end, tz=tz), by=x.freq )

          } else {
              subdaily.ts  <- FALSE
              x.freq       <- paste(dt.val, dt.stg)
              if ( inherits(datetimes, "numeric") & all( datetimes == trunc(datetimes) ) ) {
                completeTime <- seq(from=as.numeric(format(dt.ini, "%Y")), to=as.numeric(format(dt.end, "%Y")), by=x.freq )  
              } else completeTime <- seq(from=as.Date(dt.ini), to=as.Date(dt.end), by=x.freq )
            } # ELSE end

         } # ELSE end


  ##############################################################################
  # Identifying any eventual missing time steps 
  ##############################################################################
  missingDT.index <- which(is.na(pmatch(completeTime, datetimes))) 
  missingDT.n     <- length(missingDT.index)
  if ( missingDT.n > 0 )
    missingDateTimes <- completeTime[missingDT.index]

  ##############################################################################
  # Identifying whether 'x' is complete or not                                 #
  ##############################################################################
  out <- FALSE
  if ( isStrictlyRegular | (length(missingDT.index) == 0) ) {

    if (verbose) message("                                                       ")
    if (verbose) message("[ 'x' (", x.freq, ") is complete from '", dt.ini, "' to '", dt.end, "' (", nDateTimes, " elements) ! ]")

    out <- TRUE

  } else {  

      if ( (missingDT.n > 0) & verbose ) 
        message("[ There are ", missingDT.n, " missing date/times in 'x' (", x.freq, ") ]" )
      

      if ( (missingDT.n > 0) & verbose ) 
        message("[ Missing date/times are: ", paste(missingDateTimes, collapse=" ; "), " ]" )
          
    } # ELSE end


  # creating the final output
  if (out.type=="all") 
    out <- list(isComplete=out, NumberMissingDT=missingDT.n, missingDateTimes=missingDateTimes)
  
  return(out)
  
} # 'isComplete.SpatRaster' END

