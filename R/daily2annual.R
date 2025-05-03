# File daily2annual.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2008-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          daily2annual                                                        #
################################################################################
# Generic function for transforming a DAILY (sub-daily, weekly, monthly, quarterly) 
# regular time series into an ANNUAL one

# 'x'      : zoo/xts object which values will be converted into annual ones
# 'FUN'    : Function that have to be applied for aggregating into Annual time step
#            For Precipitation FUN MUST be 'sum'
#            For Temperature and Flow time series, FUN MUST be 'mean'
# 'na.rm'  : TRUE : the annual mean  value is computed considering only those values different from NA
#            FALSE: if there is AT LEAST one NA within a year, the monthly mean value is NA
# 'out.fmt': character indicating the format for the output time series. Possible values are:
#            -) "%Y"      : only the year will be used for the time. Default option. (e.g., "1961" "1962"...)
#            -) "%Y-%m-%d": a complete date format will be used for the time. Default option. (e.g., "1961" "1962"...)

daily2annual <-function(x, ...) UseMethod("daily2annual")

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: XX-XXX-2008                                                         #
# Updates: 09-Aug-2011                                                         #
#          08-Apr-2013                                                         #
#          30-Jul-2023                                                         #
#          02-may-2025 (EGU 2025)                                              #
################################################################################
daily2annual.default <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
                                 out.fmt="%Y", start.month=1, ...) {

     # Checking that 'x' is a zoo object
     if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

     daily2annual.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, 
                      out.fmt=out.fmt, start.month=start.month, ...)
     
} # 'daily2annual.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Aug-2011                                                         #
# Updates: 09-Aug-2011                                                         #
#          04-Jun-2012                                                         #
#          08-Apr-2013                                                         #
#          21-Jul-2015                                                         #
#          21-May-2022 ; 25-May-2022 ; 23-Dic-2022 ; 27-Dec-2022               #
#          20-Jun-2023 ; 30-Jul-2023                                           #
#          05-Feb-2025 (EGU 2025)                                              #
################################################################################
daily2annual.zoo <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
                             out.fmt="%Y-%m-%d", start.month=1, ...) {


  get.dates <- function(x, years, fun, fn.name) {

    is.subdaily <- ( (inherits(time(x), "POSIXct")) | (inherits(time(x), "POSIXlt")) )
    is.POISXct  <- inherits(time(x), "POSIXct")

    years.unique <- as.numeric(unique(years))
    nyears       <- length(years.unique)

    if ( (fn.name=="max") | (fn.name=="min")) {

      if (fn.name=="max") {
        datetimes.pos <- aggregate(x, by=years, FUN=which.max)
      } else datetimes.pos <- aggregate(x, by=years, FUN=which.min)

      datetimes.pos <- as.numeric(datetimes.pos)

      # Getting the datetime where the min/max value occurs for each year in 'x'.
      # this is a list object
      xMaxMin <- sapply(1:nyears, function(i, x, datetimes.pos) { 
                          all.dates.inyear <- x[ years == years.unique[i] ]
                          all.dates.inyear[ as.numeric( datetimes.pos[i] ) ]
                   }, x=x, datetimes.pos=datetimes.pos, simplify=FALSE)   

      # unblisting (and preserving datetime attrribute)
      xMaxMin <- do.call("c", xMaxMin)

      datetimes <- time(xMaxMin)

    } else if (is.subdaily) {
        datetimes <- paste0(years.unique, "-01-01 00:00:00")
        if (is.POISXct) {
          datetimes <- as.POSIXct(datetimes)
        } else datetimes <- as.POSIXlt(datetimes)
      } else datetimes <- as.Date(paste0(years.unique, "-01-01"))

    return( datetimes )
    
  } # 'get.dates' END


  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN)) 
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")
           
   # Checking the user provide a valid value for 'x'
  if (sfreq(x) %in% c("annual"))
    stop("Invalid argument: 'x' is already an annual ts !!" ) 

  # Checking 'out.fmt'
  if ( is.na(match(out.fmt, c("%Y", "%Y-%m-%d") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('%Y', '%Y-%m-%d')" )	

  # Checking that 'na.rm.max' is in [0, 1]
  if ( (na.rm.max < 0) | (na.rm.max > 1) )
    stop("Invalid argument: 'na.rm.max' must be in [0, 1] !")

  # Checking that 'na.rm.max' is in [1, 12]
  if ( (start.month < 1) | (start.month > 12) | 
       ( ( trunc(start.month) -  start.month ) != 0 ) )
    stop("Invalid argument: 'start.month' must be an integer in [1, 12] !")
	   
  # Getting the time index for each element in 'x'
  ltimes  <- time(x)

  # Getting the year corresponding to each element in 'x'
  years  <- format( ltimes, "%Y")

  # Shifting backwards the year each element in 'x', 
  # only when start.month != 1
  if ( start.month != 1 )
    years <- .shiftyears(ltime=ltimes, lstart.month=start.month)

  # Computing Annual time series
  tmp <- aggregate(x, by=years, FUN, na.rm=na.rm, ...)


  # Removing annual values in the output object for months with 
  # more than 'na.rm.max' percentage of NAs in a given year
  if ( na.rm ) {

    # Computing the percentage of missing values in each year
    na.pctg <- cmv(x, tscale="annual", start.month=start.month)

    # identifying years with a percentage of missing values higher than 'na.rm.max'
    na.pctg.index <- which( na.pctg > na.rm.max)

    # Setting as NA all the years with a percentage of missing values higher than 'na.rm.max'
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

  # getting the name of the function as character
  fn.name <- substitute(FUN)

  # date format for the output annual series:
  if ( (fn.name=="max") | (fn.name=="min")) {
    if (NCOL(tmp) == 1) {
      ltimes <- get.dates(x, fun=FUN, years=years, fn.name=fn.name)
      out    <- zoo::zoo(tmp, ltimes)
    } else { # NCOL(tmp) > 1
        out   <- vector("list", NCOL(tmp))
        for (i in 1:NCOL(tmp)) {
          ltimes   <-  get.dates(x, fun=FUN, years=years, fn.name=fn.name)
          out[[i]] <-  zoo(tmp[,i], ltimes)
        } # FOR end
      } # ELSE end
  } else out <- tmp

  return(out)

} # 'daily2annual.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: XX-XXX-2008                                                         #
# Updates: 09-Aug-2011                                                         #
#          04-Jun-2012                                                         #
#          29-May-2013                                                         #      
#          22-Aug-2022                                                         #
#          20-Jun-2023 ; 30-Jul-2023                                           #
#          02-May-2025 (EGU 2025)                                              #
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
# 'out.type': string that define the desired type of output. Possible values are
#             -) "data.frame": a data.frame, with as many columns as stations
#                              are included in 'x', and an additional column indicating the Year
#             -) "db"        : a data.frame, with 3 colums will be produced.
#                              The first column will store the Year,
#                              The second column will store the ID of the station,
#                              The third column will contain the seasonal
#                                value corresponding to that year and that station.
# 'verbose' : logical; if TRUE, progress messages are printed
daily2annual.data.frame <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
                                    out.fmt="%Y", start.month=1, 
                                    dates=1, date.fmt="%Y-%m-%d",
                                    out.type="data.frame",
                                    verbose=TRUE, ...) {
                                    
  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")

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
  x <- zoo(x, dates)
  
  ##############################################################################
  if (out.type == "data.frame") {
  
    z <- daily2annual.zoo(x=x, FUN=FUN, ..., na.rm=na.rm, 
                          na.rm.max=na.rm.max, out.fmt=out.fmt)
    
  } else if (out.type == "db") { 

       if (verbose) message("[Starting computations...]")
       
       # Amount of stations in 'x'
       nstations <- ncol(x)

       # ID of all the stations in 'x'
       snames <- colnames(x)

       # Computing the Starting and Ending Year of the analysis
       Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
       Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

       # Amount of Years belonging to the desired period
       #nyears <- Ending.Year - Starting.Year + 1

       # Computing the amount of years with data within 'x'
       ndays    <- length(dates) # number of days in the period
       tmp      <- vector2zoo(rep(0, ndays), dates)
       tmp      <- daily2annual.zoo(x= tmp, FUN=FUN, ..., na.rm=na.rm, 
                                    na.rm.max=na.rm.max, out.fmt="%Y-%m-%d", start.month=start.month)
       nyears   <- length(tmp) #number of years in the period

       # Generating a string vector with the years effectively within 'x'
       if (out.fmt != "%Y") {
          chryears <- time(tmp)
       } else chryears <- format(time(tmp), "%Y")
 
       # Creating a vector with the names of the field that will be used for storing the results
       field.names <- c("StationID", "Year", "Value" )

       # Creating the data.frame that will store the computed averages for each subcatchment
       z <- as.data.frame(matrix(data = NA, nrow = nyears*nstations, ncol = 3,
                           byrow = TRUE, dimnames = NULL) )
       colnames(z) <- field.names

       for (j in 1:nstations) {

           if (verbose) message( "Station: ", format(snames[j], width=10, justify="left"),
                                 " : ",format(j, width=3, justify="left"), "/",
                                 nstations, " => ",
                                 format(round(100*j/nstations,2), width=6, justify="left"),
                                 "%" )

          # Computing the annual values
          a <- daily2annual.zoo(x= x[,j], FUN=FUN, ..., na.rm=na.rm, 
                                na.rm.max=na.rm.max, out.fmt="%Y-%m-%d", start.month=start.month)

          # Putting the annual/monthly values in the output data.frame
          # The first column of 'x' corresponds to the Year
          row.ini <- (j-1)*nyears + 1
          row.fin <-  j*nyears

          z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 'nmonths' times
          z[row.ini:row.fin, 2] <- format(as.Date(time(a)), "%Y") # zoo::as.Date
          z[row.ini:row.fin, 3] <- a

      } # FOR end

    } # ELSE end

  return( z )

 } #'daily2annual.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: XX-XXX-2008                                                         #
# Updates: 09-Aug-2011                                                         #
#          29-May-2013                                                         # 
#          30-Jul-2023                                                         #  
################################################################################
daily2annual.matrix  <- function(x, FUN, na.rm=TRUE, na.rm.max=0, 
                                 out.fmt="%Y", start.month=1,
                                 dates=1, date.fmt="%Y-%m-%d",
                                 out.type="data.frame",
                                 verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")
   daily2annual.data.frame(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max,
                           out.fmt=out.fmt, start.month=start.month,
                           dates=dates, date.fmt=date.fmt,
                           out.type=out.type,
                           verbose=verbose, ...)

} # 'daily2annual.matrix  ' END
