# File seasonalfunction.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                            seasonalfunction                                  #
################################################################################
# Generic function for applying any R function to summarize the seasonal values#
# of a time series                                                             #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Sep-2009                                                         #
################################################################################
# 'x   '    : variable of type 'zoo' or 'data.frame'
# 'FUN'      : Function that will be applied to ALL the values in 'x' belonging to each one of the 4 weather seasons
#              (e.g., Fun can be some of c('mean', 'max', 'min', 'sd'))
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA
seasonalfunction <- function(x, ...) UseMethod("seasonalfunction")


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Sep-2009                                                         #
# Updates: 08-Aug-2011                                                         #
#          21-May-2013                                                         #
################################################################################
seasonalfunction.default <- function(x, FUN, na.rm=TRUE, type="default",...) {

     # Checking that 'x' is a zoo object
     if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

     seasonalfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, type=type, ...)

} # 'seasonalfunction.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 08-Aug-2011                                                         #
# Updates: 08-Aug-2011                                                         #
#          03-Abr-2013                                                         #
################################################################################
seasonalfunction.zoo <- function(x, FUN, na.rm=TRUE, type="default", ...) {

     # Checking that the user provied a valid argument for 'FUN'
     if (missing(FUN))  stop("Missing argument: 'FUN' must be provided")
     
     # Checking the user provide a valid value for 'x'
     if (is.na(match(sfreq(x), c("daily", "monthly"))))
	stop(paste("Invalid argument: 'x' is not a daily or mothly ts, it is a ", sfreq(x), " ts", sep="") )
	
     # Checking that the user provied a valid value for 'type'   
     valid.types <- c("default", "FrenchPolynesia")    
     if (length(which(!is.na(match(type, valid.types )))) <= 0)  
       stop("Invalid argument: 'type' must be in c('default', 'FrenchPolynesia')")

     # Time index of 'x'
     if (type=="default") { 
       seasons.lab <- c("DJF",  "MAM", "JJA", "SON")
     } else if (type=="FrenchPolynesia") { 
         seasons.lab <- c("DJFM", "AM",  "JJAS", "ON")
       } # ELSE end       
     dates   <- time(x)
     seasons <- factor( time2season( dates, type=type ), levels=seasons.lab )
     
     # 'as.numeric' is necessary for being able to change the names to the output
     # zoo::aggregate
     s <- aggregate(x, by= seasons, FUN=FUN, na.rm= na.rm )

     # Replacing the NaNs by 'NA.
     # NaN's are obtained when using the FUN=mean with complete NA values
     nan.index          <- which(is.nan(s))
     if ( length(nan.index) > 0 )  s[ nan.index] <- NA
     
     # Replacing all the Inf and -Inf by NA's
     # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
     inf.index <- which(is.infinite(s))
     if ( length(inf.index) > 0 ) s[inf.index] <- NA
     
     # Giving meaningful names to the output
     if ( (is.matrix(x)) | (is.data.frame(x)) ) {
       # Getting the name of the actual seasons in 's'
       cnames <- time(s)

       # Transformation needed in order to change the default names of the result
       s <- coredata(s)
     
       s <- t(s) # For having the season' names as column names

       # Giving the name of the seasons
       colnames(s) <- cnames
     } # IF end

     return(s)

} # 'seasonalfunction.zoo' end



################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Sep-2009                                                         #
# Updates: 08-Aug-2011                                                         #
#          03-Jun-2013                                                         #
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
#                              are included in 'x', and an additional column indicating the Year
#             -) "db"        : a data.frame, with 3 colums will be produced.
#                              The first column will store the Year,
#                              The second column will store the ID of the station,
#                              The third column will contain the seasonal
#                                value corresponding to that year and that station.
# 'verbose'      : logical; if TRUE, progress messages are printed
seasonalfunction.data.frame <- function(x, FUN, na.rm=TRUE, type="default",
                                        dates=1, date.fmt="%Y-%m-%d",
                                        out.type="data.frame",
                                        verbose=TRUE,...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
    stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN))
    stop("Missing argument: 'FUN' must be provided")
       
  # Checking that the user provied a valid value for 'type'   
  valid.types <- c("default", "FrenchPolynesia")    
  if (length(which(!is.na(match(type, valid.types )))) <= 0)  
     stop("Invalid argument: 'type' must be in c('default', 'FrenchPolynesia')")
  
  # Defining 'seasons.lab'
  if (type=="default") { 
          seasons.lab <- c("DJF",  "MAM", "JJA", "SON")
  } else if (type=="FrenchPolynesia") { 
       seasons.lab <- c("DJFM", "AM",  "JJAS", "ON")
    } # ELSE end 

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
  
  # Transforming 'x' into zoo
  x <- zoo(x, dates)
     
  ##############################################################################
  if (out.type == "data.frame") {
  
    z      <- seasonalfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, type=type, ...)
    
    snames      <- colnames(x)
    rownames(z) <- snames
    colnames(z) <- seasons.lab
    
  } else if (out.type == "db") {

        if (verbose) message("[Starting the computations...]")
        
        # Amount of stations in 'x'
        nstations <- ncol(x)

        # ID of all the stations in 'x'
        snames <- colnames(x)

        # Computing the Starting and Ending Year of the analysis
        Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
        Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

        # Amount of Years belonging to the desired period
        nyears <- Ending.Year - Starting.Year + 1

        # Amount of months belonging to the desired period
        nmonths <- 12*nyears 

        # Creating a vector with the names of the field that will be used for storing the results
        field.names <- c("StationID", "Season", "Value" )

        # Creating the data.frame that will store the computed averages for each subcatchment
        z <- as.data.frame(matrix(data = NA, nrow = 4*nstations, ncol = 3,
                           byrow = TRUE, dimnames = NULL) )

        for (j in 1:nstations) {

            if (verbose) message( paste("[ Station: ", format(snames[j], width=10, justify="left"),
                                      " : ", format(j, width=3, justify="left"), "/",
                                      nstations, " => ",
                                      format(round(100*j/nstations,2), width=6, justify="left"),
                                      "% ]", sep="") )

            # Computing the annual values
	    tmp <- seasonalfunction.default(x= tmp, FUN=FUN, na.rm=na.rm, type=type)

	    # Putting the annual/monthly values in the output data.frame
            # The first column of 'x' corresponds to the Year
            row.ini <- (j-1)*4 + 1
            row.fin <-  j*4

            z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 4 times
            z[row.ini:row.fin, 2] <- seasons.lab
            z[row.ini:row.fin, 3] <- tmp

        } # FOR end

        colnames(z) <- field.names

    } # IF end

  return( z )

 } #'seasonalfunction.data.frame' END
 
 
 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 11-Sep-2009                                                         #
# Updates: 08-Aug-2011                                                         #
################################################################################
 seasonalfunction.matrix <- function(x, FUN, na.rm=TRUE, type="default",
                                     dates=1, date.fmt="%Y-%m-%d",
                                     out.type="data.frame",
                                     verbose=TRUE,...) {
                                     
   x <- as.data.frame(x)
   #NextMethod("daily2annual")
   seasonalfunction.data.frame(x=x, FUN=FUN, na.rm=na.rm, type=type,
                               dates=dates, date.fmt=date.fmt,
                               out.type=out.type,
                               verbose=verbose,...)

} # 'seasonalfunction.matrix  ' END
