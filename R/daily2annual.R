# File daily2annual.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
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
################################################################################
daily2annual.default <- function(x, FUN, na.rm=TRUE, out.fmt="%Y",...) {

     # Checking that 'x' is a zoo object
     if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

     daily2annual.zoo(x=x, FUN=FUN, na.rm=na.rm,...)
     
} # 'daily2annual.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Aug-2011                                                         #
# Updates: 09-Aug-2011                                                         #
#          04-Jun-2012                                                         #
#          08-Apr-2013                                                         #
################################################################################
daily2annual.zoo <- function(x, FUN, na.rm=TRUE, out.fmt="%Y-%m-%d", ...) {

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN)) 
    stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")
           
   # Checking the user provide a valid value for 'x'
  if (sfreq(x) %in% c("annual"))
    stop("Invalid argument: 'x' is already an annual ts !!" ) 

  # Checking 'out.fmt'
  if ( is.na(match(out.fmt, c("%Y", "%Y-%m-%d") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('%Y', '%Y-%m-%d')" )	
	   
  # Annual index for 'x'
  dates  <- time(x)
  y      <- as.numeric(format( dates, "%Y"))
  years  <- factor( y, levels=unique(y) )

  # Computing Annual time series
  tmp <- aggregate(x, by=years, FUN, na.rm= na.rm)

  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(tmp))
  if ( length(nan.index) > 0 ) tmp[nan.index] <- NA
          
  # Replacing all the Inf and -Inf by NA's
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(tmp))
  if ( length(inf.index) > 0 ) tmp[inf.index] <- NA 
	 
  # date format for the output annual series:
  if (out.fmt == "%Y") {
    time(tmp) <- format(time(tmp), "%Y")
  } else  time(tmp) <- as.Date(paste( time(tmp), "-01-01", sep="")) # zoo::as.Date

  if (NCOL(tmp) == 1) tmp <- zoo(as.numeric(tmp), time(tmp))

  return(tmp)

} # 'daily2annual.zoo' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: XX-XXX-2008                                                         #
# Updates: 09-Aug-2011                                                         #
#          04-Jun-2012                                                         #
#          29-May-2013                                                         #      
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
daily2annual.data.frame <- function(x, FUN, na.rm=TRUE, out.fmt="%Y",
                                    dates=1, date.fmt="%Y-%m-%d",
                                    out.type="data.frame",
                                    verbose=TRUE,...) {
                                    
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
  if ( class(dates) == "numeric" ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt) # zoo::as.Date
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt) # zoo::as.Date

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  x <- zoo(x, dates)
  
  ##############################################################################
  if (out.type == "data.frame") {
  
    z <- daily2annual.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)
    
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
       tmp      <- daily2annual.zoo(x= tmp, FUN=FUN, na.rm=na.rm, out.fmt="%Y-%m-%d")
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

       y = x

       for (j in 1:nstations) {

           if (verbose) message( "Station: ", format(snames[j], width=10, justify="left"),
                                 " : ",format(j, width=3, justify="left"), "/",
                                 nstations, " => ",
                                 format(round(100*j/nstations,2), width=6, justify="left"),
                                 "%" )

          # Computing the annual values
          a <- daily2annual.zoo(x= x[,j], FUN=FUN, na.rm=na.rm, out.fmt="%Y-%m-%d")

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
################################################################################
daily2annual.matrix  <- function(x, FUN, na.rm=TRUE, out.fmt="%Y",
                                 dates=1, date.fmt="%Y-%m-%d",
                                 out.type="data.frame",
                                 verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")
   daily2annual.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                           out.fmt=out.fmt,
                           dates=dates, date.fmt=date.fmt,
                           out.type=out.type,
                           verbose=verbose,...)

} # 'daily2annual.matrix  ' END
