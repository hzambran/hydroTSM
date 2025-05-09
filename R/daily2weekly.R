# File daily2weekly.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          daily2weekly                                                        #
################################################################################
# This function transform a (sub)DAILY regular time series into a MONTHLY one

# 'x'   : daily values that will be converted into monthly ones.
#         class(x) must be zoo/xts
# 'FUN' : Function that have to be applied for transforming from daily into 
#         monthly time step
#         For precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

daily2weekly <- function(x, ...) UseMethod("daily2weekly")

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Ago-2023                                                         #
# Updates:                                                                     #
################################################################################
daily2weekly.default <- function(x, FUN, na.rm=TRUE, na.rm.max=0, ... ) {

     # Checking that 'x' is a zoo object
     if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

     daily2weekly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)

} # 'daily2weekly.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Aug-2023                                                         #
# Updates: 11-Oct-2023 ; 22-Oct-2023 ; 04-Nov-2023                             #
#          05-Feb-2025                                                         #
################################################################################
daily2weekly.zoo <- function(x, FUN, na.rm=TRUE, na.rm.max=0, ... ) {

  # Checking the user provide a valid value for 'FUN'
  if (missing(FUN))
     stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values")

  # Checking the user provide a valid value for 'x'
  if (sfreq(x) %in% c("monthly", "quarterly", "annual"))
    stop("Invalid argument: 'x' is not a (sub)daily ts. 'x' is a ", sfreq(x), " ts" )

  # Checking that 'na.rm.max' is in [0, 1]
  if ( (na.rm.max < 0) | (na.rm.max > 1) )
    stop("Invalid argument: 'na.rm.max' must be in [0, 1] !") 
      
  # Weekly index for 'x'
  dates <- time(x)
  weeks <- format(dates, "%Y-%W")
 
  # Computing the Weekly time series 
  tmp <- aggregate( x, by=weeks, FUN, na.rm= na.rm, ... ) 
  
  # Getting the weekly time attribute of the aggregated output object
  weeks.unique <- time(tmp)

  # Removing weekly values in the output object for weeks with 
  # more than 'na.rm.max' percentage of NAs in a given week
  if ( na.rm ) {

    # Computing the percentage of missing values in each week
    na.pctg <- cmv(x, tscale="weekly")

    # identifying weeks with a percentage of missing values higher than 'na.rm.max'
    na.pctg.index <- which( na.pctg > na.rm.max)

    # Setting as NA all the weeks with a percentage of missing values higher than 'na.rm.max'
    tmp[na.pctg.index] <- NA 
  } # IF end
  
  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(tmp))
  if ( length(nan.index) > 0 )  tmp[nan.index] <- NA 
  
  # Replacing all the Inf and -Inf by NA's
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(tmp))
  if ( length(inf.index) > 0 ) tmp[inf.index] <- NA 

  # Removing subdaily time attibute, but not the dates
  if (NCOL(tmp) == 1) {
    tmp <- zoo(as.numeric(tmp), weeks.unique ) 
  } else tmp <- zoo(coredata(tmp), weeks.unique )    

  return(tmp)

} # 'daily2weekly.zoo' end



################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Aug-2023                                                         #
# Updates: 22-Oct-2023                                                         #
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
daily2weekly.data.frame <- function(x, FUN, na.rm=TRUE, na.rm.max=0,
                                    dates=1, date.fmt="%Y-%m-%d",
				                            out.type="data.frame",
				                            out.fmt="numeric",
				                            verbose=TRUE, ...) {
  
  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db')")

  # Checking that the user provide a valid value for 'FUN'
  if (missing(FUN))
      stop("Missing argument value: 'FUN' must contain a valid function for aggregating the values !")

  # Checking that the user provied a valid argument for 'out.fmt'
  if (is.na(match( out.fmt, c("numeric", "zoo") ) ) )
      stop("Invalid argument: 'out.fmt' must be in c('numeric', 'zoo')")

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
  
    z <- daily2weekly.zoo(x=x, FUN=FUN, na.rm=na.rm, na.rm.max=na.rm.max, ...)
    
    if (out.fmt == "numeric") {
       snames      <- colnames(z)
       weeks.lab  <- format(time(z), "%b-%Y")
       z           <- coredata(z)
       colnames(z) <- snames
       rownames(z) <- weeks.lab        
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

          # Computing the amount of weeks with data within the desired period
          ndays   <- length(dates) # number of days in the period
          tmp     <- vector2zoo(rep(0,ndays), dates)
          tmp     <- daily2weekly.default(x= tmp, FUN=FUN, na.rm=na.rm)
          nweeks  <- length(tmp)

          # Creating a vector with the names of the field that will be used for storing the results
          field.names <- c("StationID", "Year", "Week", "Value" )

          # Creating the data.frame that will store the computed averages for each subcatchment
          z <- as.data.frame(matrix(data = NA, nrow = nweeks*nstations, ncol = 4,
                            byrow = TRUE, dimnames = NULL) )
          colnames(z) <- field.names

          y = x

          for (j in 1:nstations) {

            if (verbose) message( "Station: ", format(snames[j], width=10, justify="left"),
                                  " : ",format(j, width=3, justify="left"), "/",
                                  nstations, " => ",
                                  format(round(100*j/nstations,2), width=6, justify="left"),
                                  "%" )

	        # Computing the weekly values
	        w     <- daily2weekly.zoo(x= x[,j], FUN=FUN, ..., na.rm=na.rm, na.rm.max=na.rm.max)
          dates <- time(w)
            
	        if (out.fmt == "numeric") w <- coredata(w)

	        # Putting the annual/monthly values in the output data.frame
          # The first column of 'x' corresponds to the Year
          row.ini <- (j-1)*nweeks + 1
          row.fin <-  j*nweeks

          z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeated 'nweeks' times
          z[row.ini:row.fin, 2] <- format(as.Date(dates), "%Y") # zoo::as.Date
          z[row.ini:row.fin, 3] <- format(as.Date(dates), "%b") # zoo::as.Date
          z[row.ini:row.fin, 4] <- w

        } # FOR end
        
  } # IF end

  return( z )

 } #'daily2weekly.data.frame' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 09-Aug-2023                                                         #
# Updates:                                                                     #
################################################################################
daily2weekly.matrix  <- function(x, FUN, na.rm=TRUE, na.rm.max=0,
                                  dates=1, date.fmt="%Y-%m-%d",
				                          out.type="data.frame",
				                          out.fmt="numeric",
                                  verbose=TRUE,...) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")  # I don't know why is redirecting to 'daily2weekly.default' instead of 'daily2weekly.data.frame'....
   daily2weekly.data.frame(x=x, FUN=FUN, na.rm=na.rm, 
                            na.rm.max=na.rm.max,
                            dates=dates, date.fmt=date.fmt,
			                      out.type=out.type,
			                      out.fmt=out.fmt,
                            verbose=verbose,...)

} # 'daily2weekly.matrix  ' END
