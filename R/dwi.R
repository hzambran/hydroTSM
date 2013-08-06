# File dwi.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################ 
# Generic Days With Information function                                       #
################################################################################ 
dwi <-function(x, ...) UseMethod("dwi")


################################################################################ 
#            Zoo Days with Information                                         #
################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XXX-2009                                                         #
# Updates: 22-Aug-2011                                                         #
#          29-May-2013                                                         #
################################################################################ 
# This function generates a table indicating the number of days
# with information (<>NA's) within a zoo object,
# aggregated by: Year, Month or Month by Year

# x        : variable of type 'zoo'
# out.unit : aggregation time for the computation of the amount of days with info.
#	     Valid values are:
#            -) "months": monthly;
#            -) "years" : annual;
#            -) "mpy"   : month per year
# from     : Character indicating the starting date for the values stored in all the files that
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# to       : Character indicating the starting date for the values stored in all the files that
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# date.fmt : Character indicating the date format in which you provide 'from' and 'to', e.g. "%d-%m-%Y"
# 'tstep'  : since hydroTSM 0.3-0 it is not required any more, because it is not used any longer.
#            It is kept for backwards compatibility purposes only.

dwi.default <- function(x, out.unit="years", from = start(x), to = end(x), 
                        date.fmt="%Y-%m-%d", tstep="days", ...) {

    # Checking that the user provied a valid class for 'x'   
     valid.class <- c("xts", "zoo")    
     if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
        stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

     dwi.zoo(x=x, out.unit=out.unit, from=from, to=to, date.fmt=date.fmt, tstep=tstep, ...)

 } # 'dwi.default' end
 

 
 
################################################################################ 
#    dwi.zoo:  Zoo Days with Information                                       #
################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: 22-Aug-2009                                                         #
# Updates: 22-Aug-2011                                                         #
#          07-May-2012                                                         #
#          29-May-2013                                                         #
################################################################################ 
dwi.zoo <- function(x, out.unit="years", from= start(x), to= end(x), 
                    date.fmt="%Y-%m-%d", tstep="days", ...) {
                    
  # Checking the validity of the 'out.unit' argument
  if ( is.matrix(x) | is.data.frame(x) ) {  
    if ( is.na( match(out.unit, c("years", "months") ) ) )
       stop("Invalid argument value: 'out.unit' must be in c('years', 'months')" )
  } else {
    if ( is.na( match(out.unit, c("years", "months", "mpy") ) ) )
       stop("Invalid argument value: 'out.unit' must be in c('years', 'months', 'mpy')" )
    } # ELSE end
         
  # Checking 'from' and 'to'
  if (to < from) stop("Invalid argument: 'from > to')" )    
    
  from <- as.Date(from, format=date.fmt) # zoo::as.Date
  to   <- as.Date(to, format=date.fmt)   # zoo::as.Date

  # Selecting only those data that are within the time period between 'from' and 'to'
  x.sel <- window(x, start=from, end=to )
    
  .dwi <- function(x) { 
    nona.index  <- which(!is.na(x))
    return(length( nona.index ))    
  } # '.dwi' END
  
  .dwi2 <- function(trgt, x) { 
    tmp <- extract(x, trgt=trgt)
    nona.index  <- which(!is.na(tmp))
    return(length( nona.index ))    
  } # '.dwi' END
  
  dates  <- time(x)

  if (out.unit == "months")   {         
     # Monthly index for 'x'     
     m      <- as.numeric(format( dates, "%m" ))
     months <- factor( month.abb[m], levels=unique(month.abb[m]) )
     
     # 'as.numeric' is necessary for being able to change the names to the output
     a <- aggregate(x, by= months, FUN=.dwi)
     
     # changing the row names of the matrix/data.frame
     if ( is.data.frame(a) | is.matrix(a) )
       rownames(a) <- unique(months)
           
  } else if (out.unit == "years") {
         # Annual index for 'x'
         y      <- as.numeric(format( dates, "%Y" ))
         years  <- factor( y, levels=unique(y) )

         # 'FUN' is first applied to all the values of 'x' belonging to the same year
         a <- aggregate( x, by= years, FUN=.dwi)    
         
         # changing the row names of the matrix/data.frame
         if ( is.data.frame(a) | is.matrix(a) )
           rownames(a) <- unique(years)
           
     } else if (out.unit == "mpy") {
     
         # Computing the Starting and Ending Year of the analysis
         Starting.Year <- as.numeric(format(from, "%Y"))
         Ending.Year   <- as.numeric(format(to, "%Y"))

         # Amount of Years belonging to the desired period
         nyears <- Ending.Year - Starting.Year + 1
         
         # Dummy variable to know how many different months are present in 'x'. 
         # Only needed when x is shorter than a year
         m          <- as.numeric(format( dates, "%m" ))
         months     <- factor( month.abb[m], levels=unique(month.abb[m]) )
         tmp        <- aggregate(x, by= months, FUN=.dwi)
         nmonths    <- length(tmp)
         monthnames <- index(tmp)

         a <- matrix(data=NA, nrow=nyears, ncol=nmonths)

         #a <- sapply(Starting.Year:Ending.Year, function(i,y) {
         for (i in Starting.Year:Ending.Year) {

             tmp                       <- extract(x.sel, trgt= i)
                                         
             #a[i-Starting.Year+1,1:12] <-  sapply(1:12, FUN=.dwi2, x=tmp)
             dates  <- time(tmp)
             m      <- as.numeric(format( dates, "%m" ))
             months <- factor( month.abb[m], levels=unique(month.abb[m]) )
     
             # 'as.numeric' is necessary for being able to change the names to the output
             a[i-Starting.Year+1,1:nmonths] <-  aggregate(tmp, by= months, FUN=.dwi)
         
         } # FOR end

         #Change the names of the columns of the matrix
         #colnames(a) <- month.abb
         colnames(a) <- monthnames
         #Change the names of the rows of the matrix
         rownames(a) <- as.character(Starting.Year:Ending.Year)

    }  # ELSE IF end
    
    return(a)

 } # 'dwi.zoo' end


################################################################################ 
# dwi.data.frame: days with info in each station stored in a data frame        #
################################################################################ 
# Started: 21-Mar-2009                                                         #
# Updates: 29-May-2013 ; 06-Aug-2013                                           #
################################################################################ 
# This function generates a table indicating the number of days
# with information (<>NA's) within a data.frame

# 'x'         : variable of type 'data.frame'
# out.unit    : aggregation time for the computation of the amount of days with info.
#	        Valid values are:
#               -) "months": monthly;
#               -) "years" : annual;
# from        : starting date for detection of days with inormation
# to          : date format that will be used in the output variable
# date.fmt    : date format of "from" and "to". For CHE files, the format must be "%d-%m-%Y"
# 'dates'     : "numeric", "factor", "Date" indicating how to obtain the
#               dates for correponding to the 'sname' station
#               If 'dates' is a number, it indicates the index of the column in
#                 'x' that stores the dates
#               If 'dates' is a factor, it have to be converted into 'Date' class,
#                 using the date format  specified by 'date.fmt'
#               If 'dates' is already of Date class, the following line verifies that
#                 the number of days in 'dates' be equal to the number of element in the
#                 time series corresponding to the 'st.name' station
# 'verbose'  : logical; if TRUE, progress messages are printed
# 'tstep'  : since hydroTSM 0.3-0 it is not required any more, because it is not used any longer.
#            It is kept for backwards compatibility purposes only.

dwi.data.frame <- function(x, out.unit="years", from, to,
                           date.fmt="%Y-%m-%d", tstep="days", dates=1, verbose=TRUE,...) {

  # Checking the validity of the 'out.unit' argument
  if ( is.na( match(out.unit, c("years", "months") ) ) ) {
         stop("Invalid argument value: For data.frames, 'out.unit' must be in c('years', 'months')" ) }

  
  # Checking that the user provied a valid argument for 'dates'
  if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
      stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  # Verification that the number of days in 'dates' be equal to
  # the number of elements in 'x'
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
       stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( class(dates) == "numeric" ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt) # zoo::as.Date
    x     <- x[-tmp]
  }  else
      # If 'dates' is a factor, it have to be converted into 'Date' class,
      # using the date format  specified by 'date.fmt'
      if ( class(dates) == "factor" ) {
	    dates <- as.Date(dates, format= date.fmt) # zoo::as.Date
	  } else
	    # If 'dates' is already of Date class, the following line verifies that
            # the number of days in 'dates' be equal to the number of element in the
            # time series corresponding to the 'st.name' station
            if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
              stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # Checking the validity of the 'from' argument
  if (missing(from)) { from <- dates[1]
  } else if ( is.na( match(class(from), c("Date", "character") ) ) ) {
            stop("Invalid argument value: 'class(from)' must be in c('Date', 'character')" ) }

  # Checking the validity of the 'to' argument
  if (missing(to)) { to <- dates[length(dates)]
  } else if ( is.na( match(class(to), c("Date", "character") ) ) ) {
            stop("Invalid argument value: 'class(to)' must be in c('Date', 'character')" ) }
            
  # Transforming 'x' into a zoo object
  x <- zoo(x, dates)  
  
  ##############################################################################
  dwi.zoo(x=x, out.unit=out.unit, from=from, to=to, date.fmt=date.fmt, tstep=tstep, ...)

} # 'dwi.data.frame' END


################################################################################ 
dwi.matrix <- function(x, out.unit="years", from, to, 
                       date.fmt="%Y-%m-%d", tstep="days", dates=1, verbose=TRUE,...) {

 x <- as.data.frame(x)
 #NextMethod("daily2annual")
 dwi.data.frame(x=x, out.unit=out.unit, from=from, to=to, date.fmt=date.fmt, 
                tstep=tstep, dates=dates, verbose=verbose,...)

} # 'dwi.matrix' END

