# File izoo2rzoo.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################ 
#         Irregular Zoo -> Regular Zoo                                         #
################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################  

# This function takes a time series of (very likely) irregular (with
# missing dates) daily time series and then transforms it into a variable
# regulary spaced, filling the voids with some value (by default: NA)

# x         : time series of type zoo (very likely with some missing days)
# date.fmt  : character indicating the format in which the dates are stored in 
#             'dates', e.g. "%Y-%m-%d"
#             ONLY required when class(dates)=="factor" or "numeric"
# from      : starting date for the merged output
# to	    : ending date for the merged output
# tstep     : time step in which are stored the values of 'x'

izoo2rzoo <-function(x, ...) UseMethod("izoo2rzoo")


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
################################################################################ 
izoo2rzoo.default <- function(x, from= start(x), to= end(x), 
                              date.fmt="%Y-%m-%d", tstep ="days", ...) {

  # Checking that the user provied a valid class for 'x'   
  valid.class <- c("xts", "zoo")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
     stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

  # Requiring the Zoo Library (Z’s ordered observations)
  require(zoo)

  izoo2rzoo.zoo(x=x, from=from, to=to, date.fmt=date.fmt, tstep=tstep, ...)

} # 'izoo2rzoo.default' END


################################################################################ 
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################ 
# Started: XX-XX-2009                                                          #
# Updates: 23-Aug-2011                                                         #
#          07-May-2012                                                         #
################################################################################ 

izoo2rzoo.zoo <- function(x, from= start(x), to= end(x), 
                          date.fmt="%Y-%m-%d", tstep ="days", ... ) {

  if (is.na(match(class(x), c("zoo"))))
     stop("Invalid argument: 'x' must be of class 'zoo'")

  # Requiring the Zoo Library (Zoo's ordered observations)
  require(zoo)

  # Generating a daily-regular time series of NA's,
  # just for being merged with the real daily-irregular time series
  dates <- seq( from=as.Date(from, format=date.fmt),
            to=as.Date(to, format=date.fmt), by= tstep ) 

  na.zoo <- zoo(rep(NA, length(dates)), dates)

  # Selecting only those data that within the time period between 'from' and 'to'
  x.sel <- x[ time(x) %in% dates, ]

  # Creating a daily-regular time series with the read Precipitatoin values and 
  # NA's in those days without information
  x.merged <- merge(na.zoo, x.sel)
  
  # Giving the same column names than the original 'x'
  if ( is.matrix(x) | is.data.frame(x) )
    colnames(x.merged) <- colnames(x)

  # Returning as result only the column containing the Regular Time Series with NA's in the empy days
  return( x.merged[,-1] )

} # 'izoo2rzoo.zoo' end
