###########################################################################
# annualfunction: Generic function for computing monthly totals/mean values  #
#               for a zoo object or data.frame                             #
############################################################################
#                  May 15th, 2009; Sep 01st 2009                           #
############################################################################
# 'x   '  :  daily, monthly or annual 'zoo' or 'data.frame' object
# 'FUN'   :  Function that will be applied to ALL the values in 'x' belonging to each weather season of the year
#             (e.g., Fun can be some of c('mean', 'max', 'min', 'sd'))
# 'na.rm' : Logical. Should missing values be removed?
#              TRUE : the annual values are computed considering only those values different from NA
#              FALSE: if there is AT LEAST one NA within a year, the annual values are NA
annualfunction <- function(x, FUN, na.rm=TRUE,...) UseMethod("annualfunction")

annualfunction.default <- function(x, FUN, na.rm=TRUE,...) {

     # Checking that the user provied a valid class for 'x'   
     valid.class <- c("xts", "zoo")    
     if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
         stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

     # Requiring the Zoo Library
     require(zoo)

     annualfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)

} # 'annualfunction.default' end


########################################
# Author : Mauricio Zambrano-Bigiarini #
# Started: 09-Aug-2011                 #
# Updates: 09-Aug-2011                 #
########################################
annualfunction.zoo <- function(x, FUN, na.rm=TRUE,...) {

     # If the user did not provide a title for the plots, this is created automatically
     if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")

     # Annual index for 'x'
     dates  <- time(x)
     y      <- as.numeric(format( dates, "%Y" ))
     years  <- factor( y, levels=unique(y) )

     # 'FUN' is first applied to all the values of 'x' belonging to the same year
     totals <- aggregate( x, by= years, FUN=FUN, na.rm= na.rm )
     
     #  'FUN' is applied to all the previously computed annual values to get the final result.
     if ( (is.matrix(x)) | (is.data.frame(x)) ) {
       totals <- apply(totals, MARGIN=2, FUN=FUN, an.rm=na.rm)
     } else totals <- aggregate(totals, by = rep("value", length(totals)), FUN = FUN, na.rm = na.rm)
     
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
     } # IF end

     return(totals)

} # 'annualfunction.zoo' end


############################################################################
# annualfunction: Generic function for computing monthly totals/mean values#
#                 for a zoo object or data.frame                           #
############################################################################
# Started: 2009-May-15                                                     #
# Updates: 2009-Sep-01st ; 2011-Aug-09                                     #
############################################################################
# 'dates'   : "numeric", "factor", "Date" indicating how to obtain the 
#             dates correponding to the 'sname' station
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
# 'verbose' : logical; if TRUE, progress messages are printed 
annualfunction.data.frame <- function(x, FUN, na.rm=TRUE,
                                      dates, date.fmt="%Y-%m-%d",
                                      verbose=TRUE,...) {
	  
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
  
  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  x       <- as.zoo(x)
  time(x) <- dates
  
  ##############################################################################
  annualfunction.zoo(x=x, FUN=FUN, na.rm=na.rm, ...)

  
} #'annualfunction.data.frame' END


############################################################################
# annualfunction: Generic function for computing monthly totals/mean values#
#                 for a zoo object or data.frame                           #
############################################################################
# Started: 2009-May-15                                                     #
# Updates: 2009-Sep-01st ; 2011-Aug-09                                     #
############################################################################
annualfunction.matrix <- function(x, FUN, na.rm=TRUE,
                                  dates, date.fmt="%Y-%m-%d",
                                  verbose=TRUE,...) {
 
 x <- as.data.frame(x)
 #NextMethod("daily2annual")
 annualfunction.data.frame(x=x, FUN=FUN, na.rm=na.rm,
                           dates=dates, date.fmt=date.fmt,
                           verbose=verbose,...)
                                                                 
} # 'annualfunction.matrix' END
