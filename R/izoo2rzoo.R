#########################################################################
#         Irregular Zoo -> Regular Zoo                                  #
#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  #
# Started: XX-XX-2009                                                   #
# Updates: 23-Aug-2011                                                  #
#########################################################################

# This function takes a time series of (very likely) irregular (with
# missing dates) daily time series and then transforms it into a variable
# regulary spaced, filling the voids with some value (by default: NA)

# x         : time series of type zoo (very likely with some missing days)
# date.fmt  : character indicating the format in which the dates are stored in 'dates', e.g. "%Y-%m-%d"
#             ONLY required when class(dates)=="factor" or "numeric"
# from      : starting date for the merged output
# to	    : ending date for the merged output
# tstep     : time step in which are stored the values of 'x'

izoo2rzoo <-function(x, ...) UseMethod("izoo2rzoo")


#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  #
# Started: XX-XX-2009                                                   #
# Updates: 23-Aug-2011                                                  #
#########################################################################
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


#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  #
# Started: XX-XX-2009                                                   #
# Updates: 23-Aug-2011                                                  #
#########################################################################

izoo2rzoo.zoo <- function(x, from= start(x), to= end(x), 
                          date.fmt="%Y-%m-%d", tstep ="days", ... ) {

     if (is.na(match(class(x), c("zoo"))))
            stop("Invalid argument: 'x' must be of class 'zoo'")

     # Requiring the Zoo Library (Zoo's ordered observations)
     require(zoo)

     # Generating a daily-regular time series of NA's,
     # just for being merged with the real daily-irregular time series
     z <- seq( from=as.Date(from, format=date.fmt),
               to=as.Date(to, format=date.fmt), by= tstep ) # the default name of the column with the dates is "X_data"

     #z.zoo       <- as.zoo( rep(NA, length(z)) )
     #time(z.zoo) <- z 	#class(z) = Date
     z.zoo <- zoo(rep(NA, length(z)), z)

     # Selecting only those data that are within the time period between 'from' and 'to'
     x.sel <- x[ time(x) %in% z, ]

     # Creating a daily-regular time series with the read Precipitatoin values and NA's in those days without information
     x.merged <- merge(x.sel,z.zoo)

     # Returning as result only the column containing the Daily-Regular Time Series of Precipitation with NA's in the empy days
     return( x.merged[,1] )

} # 'izoo2rzoo.zoo' end
