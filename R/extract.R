# File extract.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2009-2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

#########################################################################
# extract: Extracts from a zoo object, all the values belonging to a    #
#             given month, year or weather season                       #
#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  #
# Started: 16-Apr-2009                                                  #
# Updates: 15-May-2009; 30-Ago-2009 ; 10-Aug-2011; 09-Sep-2026           #
#########################################################################

extract <-function(x, ...) UseMethod("extract")

# This function was called 'extractzoo' up to hydroTSM 0.2-2
extractzoo <-function(x, ...) UseMethod("extract")

# 'x'    : variable of type 'zoo'
# 'trgt' : numeric or character indicating elements to extract from 'x'
#          Valid values are:
#          1) integer(s) from 1 to 12: 'trgt' is considered as month(s),
#             and all the values in 'x' belonging to the month(s) specified by
#             'trgt' will be extracted  (1=JAN, 2=FEB,...., 12=DEC)
#          2) integer(s) > 12: 'trgt' is considered as year(s), and all the
#             values in 'x' belonging to the year(s) specified by 'trgt'
#             will be extracted
#          3) character: 'trgt' is considered as a weather season, and
#             all the values in 'x' belonging to the season specified by
#             'trgt' will be extracted. Valid values are:
#              -) "DJF": December, January, February
#              -) "MAM": March, April, May
#              -) "JJA": June, July, August
#              -) "SON": September, October, November
#              -) "SONDEFM": September, October, November, December,
#                            January, February, March


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
# Updates: 13-Feb-2014 ; 14-Feb-2014                                           #
#          22-Aug-2022; 09-Sep-2026                                            #
################################################################################
extract.zoo <- function(x, trgt, ...) {

  # Checking that the user provied a valid argument for 'trgt'
  if ( !(is.numeric(trgt) | is.character(trgt)) )
      stop("Invalid argument: class('trgt') must be in c('integer', 'numeric','character')")

  # If 'trgt' is a month or a year
  if ( is.numeric(trgt) ) {

    # Checking that 'trgt' is integer
    if ( !isTRUE(all.equal(trgt - trunc(trgt), rep(0,length(trgt)) ) ) )
        stop("Invalid argument: 'trgt' must be integer")

    trgt <- as.integer(trgt)

    if ( all(trgt %in% 1:12) ) {
       index <- which(as.integer(format(time(x), "%m")) %in% trgt)
    } else if ( all(trgt > 12) ) {
        index <- which( as.integer(format(time(x), "%Y")) %in% trgt )
      } else {
          stop("Invalid argument: numeric 'trgt' must contain only months (1:12) or only years (>12)")
        } # ELSE end

  } # if END

    # if 'trgt' is a weather season
    else if ( inherits(trgt, "character") ) {
    
          seasons.default         <- c("DJF",  "MAM", "JJA",  "SON")
          seasons.FrenchPolynesia <- c("DJFM", "AM",  "JJAS", "ON")
          seasons.special         <- "SONDEFM"
          
          # Checking that the user provied a valid class for 'trgt'   
          valid.seasons <- union(union(seasons.default, seasons.FrenchPolynesia), seasons.special)
             
          if (length(which(!is.na(match(trgt, valid.seasons )))) <= 0)  
            stop( "Invalid argument: 'trgt' must be in 'c(", paste(valid.seasons, collapse=", "), ")'" ) 
            
          # Finding out if 'tr' belongs to 'seasons.default' or to ' seasons.FrenchPolynesia'.
          if ( trgt %in% seasons.default ) {
            season.type <- "default"
          } else if ( trgt %in% seasons.FrenchPolynesia ) {
              season.type <- "FrenchPolynesia"
            } else if ( trgt %in% seasons.special ) {
                season.type <- "SONDEFM"
            } # ELSE end

	  # Gets the season each element of 'x' belongs to 'seasons.default' or to 'seasons.FrenchPolynesia'
          seasons <- time2season(time(x), out.fmt="months", type=season.type)

          # Selects only those elements of 'x' belonging to the desired season
          index <- which(seasons == trgt)

      } # ELSE end

  if (length(index)==0)
    warning("There were no elements of 'x' within 'trgt'")

  return(x[index])

} # 'extract.zoo' END
