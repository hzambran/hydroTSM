# File time2season.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2009-2020 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# time2season : This function transforms a vector of dates or date-time into a # 
#               vector of seasons (summer, winter, autumn, spring),            #
#               considering that, by default:                                  #
#               winter = DJF: December, January, February                      #
#               spring = MAM: March, April, May                                #
#               summer = JJA: June, July, August                               #
#               autumn = SON: September, October, November                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 18-Mar-2009                                                         #
# Updates: 09-Aug-2011                                                         #
#          05-Apr-2013                                                         #
#          10-Mar-2020                                                         #
################################################################################

# 'x'       : vector with the dates that have to be transformed. class(x) must be "Date"
# 'out.fmt' : format of the output seasons. Possible values are:
#             -) 'seasons' =>  "winter", "spring",  "summer", "autumn"
#             -) 'months'  =>  "DJF", "MAM",  "JJA", "SON"

# 'result': vector with the wheater season to which each date in 'x' belongs

time2season <- function(x, out.fmt="months", type="default") {

 # Checking that 'class(x)==Date'
 #if ( ( !( class(x) %in% c("Date", "POSIXct", "POSIXt") ) ) && TRUE ) 
 if (!( is(x, "Date") | is(x, "POSIXct") | is(x, "POSIXt") )) 
     stop("Invalid argument: 'x' must be in c('Date', 'POSIXct', 'POSIXt') !")

 # Checking the class of out.fmt
 if (is.na(match(out.fmt, c("seasons", "months") ) ) )
     stop("Invalid argument: 'out.fmt' must be in c('seasons', 'months')")
     
 # Checking that the user provied a valid value for 'type'   
 valid.types <- c("default", "FrenchPolynesia")    
 if (length(which(!is.na(match(type, valid.types )))) <= 0)  
     stop("Invalid argument: 'type' must be in c('default', 'FrenchPolynesia')")

 ####################
 months <- format(x, "%m")
 
 if (type=="default") {
   winter <- which( months %in% c("12", "01", "02") )
   spring <- which( months %in% c("03", "04", "05") )
   summer <- which( months %in% c("06", "07", "08") )
   autumn <- which( months %in% c("09", "10", "11") ) 
 } else if (type=="FrenchPolynesia") {
   winter <- which( months %in% c("12", "01", "02", "03") )
   spring <- which( months %in% c("04", "05") )
   summer <- which( months %in% c("06", "07", "08", "09") )
   autumn <- which( months %in% c("10", "11") ) 
 } # ELSE end

 # Creation of the output, with the same length of the 'x' input
 seasons <- rep(NA, length(x))

 if (out.fmt == "seasons") {
 
    seasons[winter] <- "winter"
    seasons[spring] <- "spring"
    seasons[summer] <- "summer"
    seasons[autumn] <- "autumn"
    
 } else { # out.fmt == "months"
 
    if (type=="default") {
      seasons[winter] <- "DJF"
      seasons[spring] <- "MAM"
      seasons[summer] <- "JJA"
      seasons[autumn] <- "SON"
    } else  if (type=="FrenchPolynesia") {
       seasons[winter] <- "DJFM"
       seasons[spring] <- "AM"
       seasons[summer] <- "JJAS"
       seasons[autumn] <- "ON"
      } # IF end
   
 } # IF end

 return(seasons)

} # 'time2season' END
