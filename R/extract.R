# File extract.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

#########################################################################
# extract: Extracts from a zoo object, all the values belonging to a    #
#             given month, year or weather season                       #
#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  #
# Started: 16-Apr-2009                                                  #
# Updates: 15-May-2009; 30-Ago-2009 ; 10-Aug-2011                       #
#########################################################################

extract <-function(x, ...) UseMethod("extract")

# This function was called 'extractzoo' up to hydroTSM 0.2-2
extractzoo <-function(x, ...) UseMethod("extract")

# 'x'    : variable of type 'zoo'
# 'trgt' : numeric or character indicating elements to extract from 'x'
#          Valid values are:
#          1) integer from 1 to 12: 'trgt' is considered as a month, and
#             all the vaues in 'x' belonging to the month specified by 'trgt'
#             will be extracted  (1=JAN, 2=FEB,...., 12=DEC)
#          2) integer > 12: 'trgt' is considered as a year, and all the
#             values in 'x' belonging to the year specified by 'trgt'
#             will be extracted
#          3) character: 'trgt' is considered as a weather season, and
#             all the values in 'x' belonging to the season specified by
#             'trgt' will be extracted. Valid values are:
#              -) "DJF": December, January, February
#              -) "MAM": March, April, May
#              -) "JJA": June, July, August
#              -) "SON": September, October, November


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 16-Apr-2009                                                         #
# Updates: 15-May-2009; 30-Ago-2009 ; 10-Aug-2011                              #
#          29-May-2013                                                         #
################################################################################
extract.default <- function(x, trgt, ...) {

  # Checking that the user provied a valid class for 'x'   
     valid.class <- c("xts", "zoo")    
     if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
        stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

     extract.zoo(x=x, trgt=trgt, ...)

} # 'extract.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 22-Aug-2011                                                         #
# Updates:                                                                     #
################################################################################
extract.zoo <- function(x, trgt, ...) {

  # Checking that the user provied a valid argument for 'trgt'
  if ( is.na( match( class(trgt), c("integer", "numeric", "character") ) ) )
      stop("Invalid argument: class('trgt') must be in c('integer', 'numeric','character')")

  # If 'trgt' is a month or a year
  if ( (class(trgt)=="numeric") | (class(trgt)=="integer")) {

    # Checking that 'trgt' is integer
    if ( trgt - trunc(trgt) != 0 )
        stop("Invalid argument: 'trgt' must be integer")

	if (trgt %in% 1:12) {
	   index <- which( as.numeric(format.Date(time(x), "%m")) == trgt )
	} else index <- which( as.numeric(format.Date(time(x), "%Y")) == trgt )

  } # if END

    # if 'trgt' is a weather season
    else if (class(trgt)=="character") {
    
          seasons.default         <- c("DJF",  "MAM", "JJA",  "SON")
          seasons.FrenchPolynesia <- c("DJFM", "AM",  "JJAS", "ON")
          
          # Checking that the user provied a valid class for 'trgt'   
          valid.seasons <- valid.seasons <- union(seasons.default, seasons.FrenchPolynesia)
             
          if (length(which(!is.na(match(trgt, valid.seasons )))) <= 0)  
            stop( paste("Invalid argument: 'trgt' must be in 'c(", paste(valid.seasons, collapse=", "), ")'",sep="") ) 
            
          # Finding out if 'tr' belongs to 'seasons.default' or to ' seasons.FrenchPolynesia'.
          if ( trgt %in% seasons.default ) {
            season.type <- "default"
          } else if ( trgt %in% seasons.FrenchPolynesia ) {
              season.type <- "FrenchPolynesia"
            } # ELSE end

	  # Gets the season each element of 'x' belongs to 'seasons.default' or to 'seasons.FrenchPolynesia'
          seasons <- time2season(time(x), out.fmt="months", type=season.type)

          # Selects only those elements of 'x' belonging to the desired season
          index <- which(seasons == trgt)

      } # ELSE end

  return(x[index])

} # 'extract.zoo' END
