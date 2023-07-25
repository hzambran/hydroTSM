# File gmd.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                         gmd: Get Missing Data                                #
################################################################################
# This function get the amount/percentage of missing data in a zoo object, 
# using a user-defined temporal scale

# 'x'       : zoo object to be analised
# 'tscale'  : character witht he temporal scale to be used for analysing the mssing data.
#             Possible values for 'tscale' are: 
#             -) "hourly"  : the amount/percentage of missing values will be given for each 
#                            hour and ,therefore, the expected time frequency of 'x' must 
#                            be sub-hourly.
#             -) "daily"   : the amount/percentage of missing values will be given for each 
#                            day and, therefore, the expected time frequency of 'x' must 
#                            be sub-daily (i.e., hourly or sub-hourly).
#             -) "monthly" : the amount/percentage of missing values will be given for each 
#                            month and, therefore, the expected time frequency of 'x' must 
#                            be sub-monthly (i.e., daily, hourly or sub-hourly).
#             -) "seasonal": the amount/percentage of missing values will be given for each 
#                            weather season (see ?time2season) and, therefore, the expected 
#                            time frequency of 'x' must be sub-seasonal (i.e., monthly, daily, 
#                            hourly or sub-hourly).
#             -) "annual"  : the amount/percentage of missing values will be given for each 
#                            year and, therefore, the expected time frequency of 'x' must 
#                            be sub-annual (i.e., seasonal, monthly, daily, hourly or sub-hourly).
# 'out.type': character indicating if the Should missing values be removed?
#             Possible values for 'out'type' are: out.type=c("percentage", "amount")
# 'dec'     : integer indicating the amount of decimal places included in the output. 
#             It is only used when "out.type=='percentage'"

gmd <-function(x, ...) UseMethod("gmd")


################################################################################
#                              gmd.default                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates:                                                                     #
################################################################################
gmd.default <- function(x, 
	                    tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                    out.type=c("percentage", "amount"),
	                    dec=3) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !")

  # checking 'tscale'
  tscale <- match.arg(tscale)

  # checking 'out.type'
  out.type <- match.arg(out.type)

  gmd.zoo(x, tscale=tscale, out.type=out.type, dec=dec)
     
} # 'gmd.default' end



################################################################################
#                              gmd.zoo                                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates:                                                                     #
################################################################################
gmd.zoo <- function(x, 
	                tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                out.type=c("percentage", "amount"),
	                dec=3) {
  # checking 'tscale'
  tscale <- match.arg(tscale)

  # checking 'out.type'
  out.type <- match.arg(out.type)

  # Checking that the time frequency of 'x' is compatible with 'tscale'
  if ( tscale == "hourly") {
  	if ( !(sfreq(x) %in% c("minute") ) )
  	  stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
  } else if ( tscale == "daily") {
  	  if ( !(sfreq(x) %in% c("minute", "hourly") ) )
  	    stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
    } else if ( tscale == "weekly") {
  	    if ( !(sfreq(x) %in% c("minute", "hourly", "daily") ) )
  	      stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
      } else if ( tscale == "monthly") {
  	      if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly") ) )
  	        stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
        } else if ( tscale == "quarterly") {
  	        if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly") ) )
  	          stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
          } else if ( tscale == "seasonal") {
  	          if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly") ) )
  	            stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
            } else if ( tscale == "annual") {
  	            if ( !(sfreq(x) %in% c("minute", "hourly", "daily", "weekly", "monthly", "quarterly", "seasonal") ) )
  	              stop("Invalid argument: 'tscale' and the time frequency of 'x' are not compatible !") 
              } # else END

  
  # Computing the total amount of data at the desired temporal scale
  fun <- length
  ndata <- switch( tscale,  
    "hourly"    = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d %H"), FUN=fun),  
    "daily"     = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d")   , FUN=fun),  
    "weekly"    = aggregate(x, by= function(tt) format(tt, "%Y-%W")      , FUN=fun),  # week starting on Monday 
    "monthly"   = aggregate(x, by= function(tt) format(tt, "%Y-%m")      , FUN=fun),  
    "quarterly" = aggregate(x, by= function(tt) zoo::format.yearqtr(tt)  , FUN=fun),  
    "seasonal"  = aggregate(x, by= function(tt) paste0(format(tt, "%Y"), "-", time2season(tt)), FUN=fun),  
    "annual"    = aggregate(x, by= function(tt) format(tt, "%Y")         , FUN=fun)
  ) # 'md' END

  # Function for obtaining the amount of missing values in 'x'
  smv <- function(x) {
    na.index <- is.na(x)
    return( sum(na.index) )
  } # 'smv' END

  # Computing the amount of missing values at the desired temporal scale
  fun <- smv
  nNA<- switch( tscale,  
    "hourly"    = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d %H"), FUN=fun),  
    "daily"     = aggregate(x, by= function(tt) format(tt, "%Y-%m-%d")   , FUN=fun),  
    "weekly"    = aggregate(x, by= function(tt) format(tt, "%Y-%W")      , FUN=fun),  # week starting on Monday 
    "monthly"   = aggregate(x, by= function(tt) format(tt, "%Y-%m")      , FUN=fun),  
    "quarterly" = aggregate(x, by= function(tt) zoo::format.yearqtr(tt)  , FUN=fun),  
    "seasonal"  = aggregate(x, by= function(tt) paste0(format(tt, "%Y"), "-", time2season(tt)), FUN=fun),  
    "annual"    = aggregate(x, by= function(tt) format(tt, "%Y")         , FUN=fun)
  ) # 'md' END

  if (out.type=="percentage") {
  	out <- nNA/ndata
  	out <- round(out, dec)
  } else out <- nNA  # out.type=="amount"

  return(out)

} # 'gmd' END



################################################################################
#                              gmd.data.frame                                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates:                                                                     #
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
# 'verbose' : logical; if TRUE, progress messages are printed
gmd.data.frame <- function(x, tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                       out.type=c("percentage", "amount"),
	                       dec=3,
                           dates=1, 
                           date.fmt="%Y-%m-%d") {
                                    
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
  tryCatch(
          #this is the chunk of code we want to run
          { x <- zoo(x, dates)
          #when it throws an error, the following block catches the error
          }, error = function(msg){return(NA)}
          )
  
  gmd.zoo(x=x, tscale=tscale, out.type=out.type, dec=dec)

 } #'gmd.data.frame' END


################################################################################
#                                gmd.matrix                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 25-Jul-2023 (Buenos Aires)                                          #
# Updates:                                                                     #
################################################################################
gmd.matrix  <- function(x, tscale=c("hourly", "daily", "weekly", "monthly", "quarterly", "seasonal", "annual"), 
	                    out.type=c("percentage", "amount"),
	                    dec=3,
                        dates=1, 
                        date.fmt="%Y-%m-%d") {

   x <- as.data.frame(x)
   gmd.data.frame(x=x, tscale=tscale, out.type=out.type, dec=dec,
                  dates=dates, date.fmt=date.fmt)

} # 'gmd.matrix  ' END
