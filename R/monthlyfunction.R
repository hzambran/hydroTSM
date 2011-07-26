########################################################################
# monthlyfunction: Generic function for applying any R function to     #
#                  ALL the values in 'x' belonging to a given month    #
########################################################################
# Started: May 15th, 2009;                                             #
# Updates: 31-Aug-2009 ; 25-Jul-2011                                   #
########################################################################
# 'x   '    : variable of type 'zoo' or 'data.frame', with daily or monthly frequency
# 'FUN'      : Function that will be applied to ALL the values in 'x' belonging to each one of the 12 months of the year
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA
monthlyfunction <- function(x, ...) UseMethod("monthlyfunction")

monthlyfunction.default <- function(x, FUN, na.rm=TRUE,...) {

     # Checking that the user provied a valid class for 'x'   
     valid.class <- c("xts", "zoo")    
     if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
        stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

     # Checking that the user provied a valid argument for 'FUN'
     if (missing(FUN))
         stop("Missing argument: 'FUN' must be provided")

     # Checking the user provide a valid value for 'x'
     if (is.na(match(sfreq(x), c("daily", "monthly"))))
	stop(paste("Invalid argument: 'x' is not a daily or mothly ts, it is a ", sfreq(x), " ts", sep="") )

     # Requiring the Zoo Library (Z’s ordered observations)
     require(zoo)

     # 'as.numeric' is necessary for being able to change the names to the output
     totals <- aggregate( x, by= format( time(x), "%m" ), FUN=FUN, na.rm= na.rm )

     # Replacing the NaNs by 'NA.
     # NaN's are obtained when using theFUN=mean with complete NA values
     nan.index          <- which(is.nan(totals))
     totals[ nan.index] <- NA

     # Getting the position of all the years in which there were no values
     # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
     inf.index <- which(is.infinite(totals))

     # Changing all the Inf and -Inf by NA's
     if ( length(inf.index) > 0 ) totals[inf.index] <- NA
     
     # numeric index with the months really present in 'x' (when shorther than 1 year)
     month.index <- as.numeric( time(totals) )
     
     # Transformation needed in order to change the default names of the result
     totals <- as.numeric(totals)

     # Giving meaningful names to the output
     names(totals) <- month.abb[month.index]

     return(totals)

} # 'monthlyfunction.default' end


# co: numeric, with the column position in 'x' to be used for applying FUN
# x: zoo matrix
.monthlyfunctioncol <- function(col, x, FUN2, na.rm=TRUE, verbose=TRUE, snames, nstations, ...) {

  if (verbose) message( paste("Station: ", format(snames[col], width=10, justify="left"),
					    " : ",format(col, width=3, justify="left"), "/",
					    nstations, " => ",
					    format(round(100*col/nstations,2), width=6, justify="left"),
					    "%", sep="") )
					    
  monthlyfunction.default(x[,col], FUN=FUN2, na.rm=na.rm, ...)
  
} # '.monthlyfunctioncol' END

########################################
# Author : Mauricio Zambrano-Bigiarini #
# Started: 25-Jul-2011                 #
########################################
monthlyfunction.zoo <- function(x, FUN, na.rm=TRUE,
                                out.type="data.frame",
                                verbose=TRUE,...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

   # Checking that the user provied a valid argument for 'FUN'
   if (missing(FUN))
         stop("Missing argument: 'FUN' must be provided")

  # Amount of stations in 'x'
  nstations <- ncol(x)
  
  if (length(nstations) == 0) {
    z <- monthlyfunction.default(x= x, FUN=FUN, na.rm=na.rm, ...)
  } else {
      if (verbose) message("[Starting the computations...]")
      
      # ID of all the stations in 'x'
      snames <- colnames(x)
      
      if (is.null(snames)) snames <- paste("V", 1:nstations, sep="")
    
      if (out.type == "data.frame") {
        z <- sapply(1:nstations, FUN=.monthlyfunctioncol, x=x, FUN2=FUN, na.rm=na.rm, verbose=verbose, snames=snames, nstations=nstations, ...)
        z <- t(z) # I don't know WHY !!
        rownames(z) <- snames
      } else if (out.type == "db") {
  
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

            if (verbose) message( paste("Station: ", format(snames[j], width=10, justify="left"),
                                        " : ", format(j, width=3, justify="left"), "/",
                                        nstations, " => ",
                                        format(round(100*j/nstations,2), width=6, justify="left"),
                                        "%", sep="") )

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
      } # IF end
    } # ELSE end

  return( z )

 } #'monthlyfunction.zoo' END
 
 
 
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
########################################
# Author : Mauricio Zambrano-Bigiarini #
# Updates: 25-Jul-2011                 #
########################################
monthlyfunction.data.frame <- function(x, FUN, na.rm=TRUE,
                                       dates, date.fmt="%Y-%m-%d",
                                       out.type="data.frame",
                                       verbose=TRUE,...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

   # Checking that the user provied a valid argument for 'FUN'
   if (missing(FUN))
         stop("Missing argument: 'FUN' must be provided")

  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
    {
       # Checking that the user provied a valid argument for 'dates'
       if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
           stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

        # Verification that the number of days in 'dates' be equal to the number
        # of elements in 'x'
        if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
        stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
    } # ELSE end

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

  x       <- as.zoo(x)
  time(x) <- dates
  
  #NextMethod("monthlyfunction")
  monthlyfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, out.type=out.type, verbose=verbose,...)

 } #'monthlyfunction.data.frame' END
 
 
 
monthlyfunction.matrix <- function(x, FUN, na.rm=TRUE,
                                   dates, date.fmt="%Y-%m-%d",
                                   out.type="data.frame",
                                   verbose=TRUE,...) {
 x <- as.data.frame(x)
 #NextMethod("daily2annual")
 monthlyfunction.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                            dates=dates, date.fmt=date.fmt,
                            out.type=out.type,
                            verbose=verbose,...)
                                                                    
 } # 'monthlyfunction.matrix' END                                    
