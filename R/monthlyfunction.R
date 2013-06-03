# File monthlyfunction.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# monthlyfunction: Generic function for applying any R function to             #
#                  ALL the values in 'x' belonging to a given month            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: May 15th, 2009;                                                     #
# Updates: 31-Aug-2009 ; 25-Jul-2011 ; 08-Aug-2011                             #
#          08-Apr-2013                                                         #
################################################################################
# 'x   '    : variable of type 'zoo' or 'data.frame', with daily or monthly frequency
# 'FUN'      : Function that will be applied to ALL the values in 'x' belonging to each one of the 12 months of the year
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA
monthlyfunction <- function(x, ...) UseMethod("monthlyfunction")

monthlyfunction.default <- function(x, FUN, na.rm=TRUE,...) {

    # Checking that 'x' is a zoo object
    if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

    monthlyfunction.zoo(x=x, FUN=FUN, na.rm=na.rm,...)

} # 'monthlyfunction.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2011                                                         #
# Updates: 08-Aug-2011                                                         #
#          05-Jun-2012                                                         #
#          08-Apr-2013                                                         #
################################################################################
monthlyfunction.zoo <- function(x, FUN, na.rm=TRUE,...) {

     # Checking that the user provied a valid argument for 'FUN'
     if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")
     
     # Checking the user provide a valid value for 'x'
     if (sfreq(x) %in% c("quarterly", "annual"))
	stop("Invalid argument: 'x' is not a sub-daily, daily, weekly or monthly ts. 'x' is a ", sfreq(x), " ts" )

     # Monthly index for 'x'
     dates  <- time(x)
     m      <- as.numeric(format( dates, "%m" ))
     months <- factor( month.abb[m], levels=unique(month.abb[m]) )
     
     # 'as.numeric' is necessary for being able to change the names to the output
     totals <- aggregate(x, by= months, FUN=FUN, na.rm= na.rm ) 

     # Replacing the NaNs by 'NA.
     # NaN's are obtained when using the FUN=mean with complete NA values
     nan.index          <- which(is.nan(totals))
     if ( length(nan.index) > 0 )  totals[ nan.index] <- NA
     
     # Replacing all the Inf and -Inf by NA's
     # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
     inf.index <- which(is.infinite(totals))
     if ( length(inf.index) > 0 ) totals[inf.index] <- NA

     # Giving meaningful names to the output
     if ( (is.matrix(x)) | (is.data.frame(x)) ) {
       totals <- t(totals) # For having the months' names as column names
       colnames(totals) <- levels(months)
     } #IF end

     return(totals)

} # 'monthlyfunction.zoo' end
 
 

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2011                                                         #
# Updates: 08-Aug-2011                                                         #
#          29-May-2013 ; 03-Jun-2013                                           #
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
#             -) "data.frame": a data.frame, with 12 columns representing the months,
#                              and as many rows as stations are included in 'x'
#             -) "db"        : a data.frame, with 4 colums will be produced.
#                              The first column stores the ID of the station
#                              The second column stores the Year,
#                              The third column stores the ID of the station,
#                              The fourth column contains the monthly value corresponding to the year specified in the second column
# 'verbose'      : logical; if TRUE, progress messages are printed
monthlyfunction.data.frame <- function(x, FUN, na.rm=TRUE,
                                       dates=1, date.fmt="%Y-%m-%d",
                                       out.type="data.frame",
                                       verbose=TRUE,...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
    stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")

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

  # Transforming 'x' into a zoo object
  x <- zoo(x, dates)
  
  ##############################################################################
  if (out.type == "data.frame") {
  
    monthlyfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)
    
  } else if (out.type == "db") {
  
        # Amount of stations in 'x'
        nstations <- ncol(x)
        
        # ID of all the stations in 'x'
        snames <- colnames(x)
      
        if (is.null(snames)) snames <- paste("V", 1:nstations, sep="")
  
        # Computing the Starting and Ending Year of the analysis
        Starting.Year <- as.numeric(format(start(x), "%Y"))
        Ending.Year   <- as.numeric(format(end(x), "%Y"))

        # Amount of Years belonging to the desired period
        nyears <- Ending.Year - Starting.Year + 1
        
        # Computing the numeric index of the resulting months
        month.index <- unique(as.numeric(format( time(x), "%m" )))
     
        # Amount of different months belonging to the desired period
        nmonths <- length(month.index)
        
        # Total amount of months belonging to the desired period
        totalmonths <- nmonths*nyears

        # Creating a vector with the names of the field that will be used for storing the results
        field.names <- c("StationID", "Year", "Month", "Value" )

        # Creating the data.frame that will store the computed averages for each subcatchment
        z <- as.data.frame(matrix(data = NA, nrow = totalmonths*nstations, ncol = 4, byrow = TRUE, dimnames = NULL) )
        
        for (j in 1:nstations) {

            if (verbose) message( "[ Station: ", format(snames[j], width=10, justify="left"),
                                  " : ", format(j, width=3, justify="left"), "/",
                                  nstations, " => ",
                                  format(round(100*j/nstations,2), width=6, justify="left"),
                                  "% ]" )

	    # Computing the annual values
	    tmp <- monthlyfunction.default(x= x[,j], FUN=FUN, na.rm=na.rm, ...)

	    # Putting the annual/monthly values in the output data.frame
            # The first column of 'x' corresponds to the Year
            row.ini <- (j-1)*totalmonths + 1
            row.fin <-  j*totalmonths

            z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 'totalmonths' times
            z[row.ini:row.fin, 2] <- rep(Starting.Year:Ending.Year, each=nmonths)
            z[row.ini:row.fin, 3] <- month.abb[month.index]
            z[row.ini:row.fin, 4] <- tmp

        } # FOR end
        
        colnames(z) <- field.names
        
        return( z )
      
    } # ELSE end

 } #'monthlyfunction.data.frame' END
 
 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2011                                                         #
# Updates: 08-Aug-2011                                                         #
#          29-May-2013                                                         #
################################################################################
monthlyfunction.matrix <- function(x, FUN, na.rm=TRUE,
                                   dates=1, date.fmt="%Y-%m-%d",
                                   out.type="data.frame",
                                   verbose=TRUE,...) {
 x <- as.data.frame(x)
 #NextMethod("monthlyfunction")
 monthlyfunction.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                            dates=dates, date.fmt=date.fmt,
                            out.type=out.type,
                            verbose=verbose,...)
                                                                    
 } # 'monthlyfunction.matrix' END  
