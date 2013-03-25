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
# Started: 25-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
daily2monthly.default <- function(x, FUN, na.rm=TRUE, ... ) {

     # Checking that the user provied a valid class for 'x'   
     valid.class <- c("xts")    
     if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
        stop("Invalid argument: 'class(x)' must be 'xts' !!")

     # Requiring the xts Library (Z’s ordered observations)
     require(xts)

     apply.daily(x=x, FUN=FUN, na.rm=na.rm)

} # 'subdaily2daily.default' end


########################################
# Author : Mauricio Zambrano-Bigiarini #
# Started: 09-Aug-2011                 #
# Updates: 09-Aug-2011                 #
########################################
daily2monthly.zoo <- function(x, FUN, na.rm=TRUE, ... ) {

  # Checking the user provide a valid value for 'FUN'
  if (missing(FUN))
     stop("Missing argument value: 'FUN' must contain a valid function for aggregating the daily values")

  # Checking the user provide a valid value for the sampling frequency of 'x'
  if (sfreq(x) != "daily")
      stop(paste("Invalid argument: 'x' is not a daily ts, it is a ", sfreq(x), " ts", sep="") )
      
  # Monthly index for 'x'
  dates  <- time(x)
  months <- zoo::as.Date( as.yearmon( time(x) ) )

  # Generating a Monthly time series of Total Monthly Precipitation (Monthly sum of daily values)
  tmp <-aggregate( x, by=months, FUN, na.rm= na.rm )
  
  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(tmp))
  if ( length(nan.index) > 0 ) { tmp[nan.index] <- NA }
  
  # Replacing all the Inf and -Inf by NA's
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(tmp))
  if ( length(inf.index) > 0 ) tmp[inf.index] <- NA 

  return(tmp)

} # 'daily2monthly.zoo' end



########################################
# Author : Mauricio Zambrano-Bigiarini #
# Started: XX-XXX-2008                 #
# Updates: 09-Aug-2011                 #
#          04-Jun-2012                 #
########################################
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
# 'out.type': string that define the desired type of output. Possible values are
#             -) "data.frame": a data.frame, with as many columns as stations
#                              are included in 'x'
#             -) "db"        : a data.frame, with 4 colums will be produced.
#                              The first column stores the ID of the station,
#                              The second column stores the Year
#                              The third column stores the month
#                              The fourth colum stores the numerical values corresponding to the year and month specified in the two previous fields.
# 'out.fmt' : character, for selecting if the result will be 'numeric' or 'zoo'. Valid values are: c('numeric', 'zoo')
# 'verbose'      : logical; if TRUE, progress messages are printed
daily2monthly.data.frame <- function(x, FUN, na.rm=TRUE,
                                     dates, date.fmt="%Y-%m-%d",
				     out.type="data.frame",
				     out.fmt="numeric",
				     verbose=TRUE,...) {
  
  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db')")

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
      stop("Missing argument value: 'FUN' must contain a valid function for aggregating the daily values")

  # Checking that the user provied a valid argument for 'out.fmt'
  if (is.na(match( out.fmt, c("numeric", "zoo") ) ) )
      stop("Invalid argument: 'out.fmt' must be in c('numeric', 'zoo')")

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
    dates <- zoo::as.Date(x[, dates], format= date.fmt)
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- zoo::as.Date(dates, format= date.fmt)

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
     
  # Transforming 'x' into a zoo object
  x <- zoo(x, dates)
  
  ##############################################################################
  if (out.type == "data.frame") {
  
    z <- daily2monthly.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)
    
    if (out.fmt == "numeric") {
       snames      <- colnames(z)
       months.lab  <- format(time(z), "%b-%Y")
       z           <- coredata(z)
       colnames(z) <- snames
       rownames(z) <- months.lab        
    } # IF end
    
  } else if (out.type == "db") {  
  
          if (verbose) message("[Starting computations...]" )

          # Amount of stations in 'x'
          nstations <- ncol(x)

          # ID of all the stations in 'x'
          snames <- colnames(x)

          # Computing the Starting and Ending Year of the analysis
          Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
          Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))
          
          # Amount of Years belonging to the desired period
          nyears <- Ending.Year - Starting.Year + 1

          # Computing the amount of months with data within the desired period
          ndays   <- length(dates) # number of days in the period
          tmp     <- vector2zoo(rep(0,ndays), dates)
          tmp     <- daily2monthly.default(x= tmp, FUN=FUN, na.rm=na.rm)
          nmonths <- length(tmp)

          # Creating a vector with the names of the field that will be used for storing the results
          field.names <- c("StationID", "Year", "Month", "Value" )

          # Creating the data.frame that will store the computed averages for each subcatchment
          z <- as.data.frame(matrix(data = NA, nrow = nmonths*nstations, ncol = 4,
                            byrow = TRUE, dimnames = NULL) )
          colnames(z) <- field.names

          y = x

          for (j in 1:nstations) {

            if (verbose) message( "Station: ", format(snames[j], width=10, justify="left"),
                                  " : ",format(j, width=3, justify="left"), "/",
                                  nstations, " => ",
                                  format(round(100*j/nstations,2), width=6, justify="left"),
                                  "%" )

	    # Computing the monthly values
	    m     <- daily2monthly.zoo(x= x[,j], FUN=FUN, na.rm=na.rm)
            dates <- time(m)
            
	    if (out.fmt == "numeric") m <- coredata(m)

	# Putting the annual/monthly values in the output data.frame
        # The first column of 'x' corresponds to the Year
        row.ini <- (j-1)*nmonths + 1
        row.fin <-  j*nmonths

        z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeated 'nmonths' times
        z[row.ini:row.fin, 2] <- format(zoo::as.Date(dates), "%Y")
        z[row.ini:row.fin, 3] <- format(zoo::as.Date(dates), "%b")
        z[row.ini:row.fin, 4] <- m

        } # FOR end
        
  } # IF end

  return( z )

 } #'daily2monthly.data.frame' END


########################################
# Author : Mauricio Zambrano-Bigiarini #
# Started: XX-XXX-2008                 #
# Updates: 09-Aug-2011                 #
########################################
daily2monthly.matrix  <- function(x, FUN, na.rm=TRUE,
                                  dates, date.fmt="%Y-%m-%d",
				  out.type="data.frame",
				  out.fmt="numeric",
                                  verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2monthly.default' instead of 'daily2monthly.data.frame'....
   daily2monthly.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                            dates=dates, date.fmt=date.fmt,
			    out.type=out.type,
			    out.fmt=out.fmt,
                            verbose=verbose,...)

} # 'daily2monthly.matrix  ' END
