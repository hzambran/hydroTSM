# File dwdays.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# dwdays:  average amount of dry/wet days per each month                       #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jan-2010                                                         #
# Updates: 29-May-2013                                                         #
################################################################################
# Given a daily time series of precipitation, this function computes the average amount
# of dry/wet days (pcp > thr or pcp < thr for wet and dry days, respectively) on each month

# 'x'  : zoo. Daily time series of precipitation.
# 'thr': numeric. Value of daily precipitation used as threshold for classifying a day as dry/wet or not.
#        Days with a precipitation value larger or equal to 'thr' are classified as 'wet days',
#        whereas precipitation values lower or equal to 'thr' are classified as 'dry days'
# 'type': character, indicating if the daily values have to be calssified as dry or wet days. It works liked to the values specified in 'thr'. Valid values aer c('wet', dry')
dwdays <-function(x, ...) UseMethod("dwdays")

dwdays.default <- function(x, thr=0, type="wet", na.rm=TRUE, ... ) {

  # Checking the user provide a valid value for 'x'
  if (is.na(match(class(x), c("zoo"))))
    stop("Invalid argument: 'x' must be of class 'zoo'")

  # Checking the user provide a valid value for 'x'
  if (is.na(match(sfreq(x), c("daily")))) {
    stop(paste("Invalid argument: 'x' is not a daily ts, it is a ", sfreq(x), " ts", sep="") ) }

  # Checking the user provide a valid value for 'type'
  if ( is.na(match(type, c("dry", "wet"))) )
    stop("Invalid argument: 'type' must be in c('dry', 'wet'")

  # getting the dates of 'x'
  dates <- time(x)

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
  Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

  # Total amount of Years of 'x'
  nyears <- Ending.Year - Starting.Year + 1

  # Array with the average monthly amount of wet days
  wdays <- rep(NA, 12)

  for (m in 1:12) {
    #Extracts all the days of 'x' belonging to the month 'm'
    pcp.m <- extract(x, m)

    if (type=="wet") {
      wdays[m] <- length(which(pcp.m > thr )) / nyears
    } else if (type=="dry") {
        wdays[m] <- length(which(pcp.m < thr )) / nyears
      } # ELSE end
  } # FOR m end

  names(wdays) <- month.abb

  return(wdays)

} # 'dwdays.default' end



################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jan-2010                                                         #
# Updates: 29-May-2013                                                         #
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
# 'verbose' : logical; if TRUE, progress messages are printed
dwdays.data.frame <- function(x, thr=0, type="wet", na.rm=TRUE,
                              dates=1,
                              date.fmt="%Y-%m-%d",
							  verbose=TRUE,...) {


  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
     # Checking that the user provied a valid argument for 'dates'
     if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( class(dates) == "numeric" ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt)
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt)

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # Amount of stations in 'x'
  nstations <- ncol(x)

  # ID of all the stations in 'x'
  snames <- colnames(x)

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
  Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

  # Amount of Years belonging to the desired period
  nyears <- Ending.Year - Starting.Year + 1

  if (verbose) message("[Starting the computations...]")

  # Creating the data.frame that will store the computed averages for each station
  z <- as.data.frame(matrix(data = NA, ncol = 12, nrow = nstations,
                     byrow = TRUE, dimnames = NULL) )

  colnames(z) <- month.abb
  rownames(z) <- snames

  y = x

  for (j in 1:nstations) {

      if (verbose) message( paste("Station: ", format(snames[j], width=10, justify="left"),
                                " : ",format(j, width=3, justify="left"), "/",
                                nstations, " => ",
                                format(round(100*j/nstations,2), width=6, justify="left"),
                                "%", sep="") )

      # Transforming the column of 'x' into a zoo object,
      # using the dates provided by the user
      tmp <- vector2zoo(x=y[,j], dates=dates, date.fmt=date.fmt)

      # Computing the monthly values
      z[j, ] <- as.numeric(dwdays.default(x=tmp, thr=thr, type=type, na.rm=na.rm, ...))


  } # FOR end


  return( z )

 } #'dwdays.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 24-Jan-2010                                                         #
# Updates: 29-May-2013                                                         #
################################################################################
dwdays.matrix  <- function(x, thr=0, type="wet", na.rm=TRUE,
                           dates=1,
                           date.fmt="%Y-%m-%d",
			   verbose=TRUE,...) {

   x <- as.data.frame(x)

   NextMethod("dwdays", x, thr=thr, type=type,  na.rm=na.rm,
              dates=dates,
              date.fmt=date.fmt,
              verbose=verbose,...)

} # 'dwdays.matrix  ' END
