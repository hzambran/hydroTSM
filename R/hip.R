# File hip.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2013-2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                                    'hip'                                     #  
################################################################################       
# Purpose: Given any starting and ending date/time objects, it generates:      #
#        1) a vector with all the hours between the two dates, OR              #
#	 2) the amount of hours between the two date/time objects                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 2008                                                                #
# Updates: 29-May-2013                                                         #
#          07-Jul-2022 ; 09-Oct-2022                                           #
#          27-Nov-2023                                                         #
################################################################################

# 'from'    : Character indicating the starting date/time object for computing the number of hours.
#             It MUST have the date/time format specified by 'date.fmt'
# 'to'	    : Character indicating the ending date/time object for computing the number of dyas.
#             It MUST have the date/time format specified by 'date.fmt'
# 'date.fmt': Character indicating the date format in which you provide 'from' and 'to'. (e.g., "%d-%m-%Y %H")
# 'out.type': Character indicating the type of result that is given by this function.
#             Valid values are:
#		      -) type= "seq"  => a vectorial sequence with all the hours within the given time period
#		      -) type= "nmbr" => the number of hours in the vectorial sequence with all the hourswithin the given time period
# 'tz'      : specification of the desired time zone yo be used. 
#             System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} (the default value) is UTC (Universal Time, Coordinated). 
#             See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
#             This argument can be used when working with subdaily zoo objects to force using the local time zone instead of GMT as time zone.
hip <- function(from, to, date.fmt="%Y-%m-%d %H", out.type="seq", tz="UTC") {

     # Checking 'out.type'
     if (is.na(match(out.type, c("seq", "nmbr"))))
        stop("Invalid argument: 'out.type' must be of class 'seq' or 'nmbr'")
        
     # Converting 'from' into a Date object (if necessary)
     if ( !(is(from, "POSIXct")) | !(is(from, "POSIXt")) ) {
       from.bak <- from
       from     <- as.POSIXct(from, format=date.fmt, tz=tz)
     } # IF end
     
     # Checking that 'from' is a valid Date object
     if (is.na(from)) 
       stop("Invalid argument: 'from' (", from.bak, 
            ") is not compatible with 'date.ftm' (", date.fmt, ") !")
     
     # Converting 'to' into a Date object (if necessary)
     if ( !(is(to, "POSIXct")) | !(is(to, "POSIXt")) ) {
       to.bak <- to
       to     <- as.POSIXct(to, format=date.fmt, tz=tz)
     } # IF end
     
     # Checking that 'to' is a valid Date object
     if (is.na(to)) 
       stop("Invalid argument: 'to' (", to.bak, 
            ") is not compatible with 'date.ftm' (", date.fmt, ") !")
     
     # Checking that 'from' is lower or equal to 'to'
     if (to < from) stop("Invalid argument: 'from > to' (", from, " > ", to, ")")

     # Generating an Annual-regular time series of Dates.
     vec.hrs <- seq( from=from, to=to, by="hours" )

     if (out.type=="seq") return(vec.hrs)
     else if (out.type=="nmbr") return ( length(vec.hrs) )

} # 'hip' END
