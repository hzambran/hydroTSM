#########################################################################
# extractzoo: Extracts from a zoo object, all the values belonging to a #
#             given month, year or weather season                       #
#########################################################################
#           April 16th, 2009;  May 15th, 2009; Ago 30th, 2009           #
#########################################################################

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

extractzoo <- function(x, trgt, ...) {

  # Checking that the user provied a valid argument for 'trgt'
  if ( is.na( match( class(trgt), c("integer", "numeric", "character") ) ) )
      stop("Invalid argument: class('trgt') must be in c('integer', 'numeric','character')")

  # If 'trgt' is a month or a year
  if ( (class(trgt)=="numeric") | (class(trgt)=="integer")) {

    # Checking that 'trgt' is integer
    if ( trgt - trunc(trgt) != 0 )
        stop("Invalid argument: 'trgt' must be integer")

	if (trgt %in% 1:12) {
	   index <- which( as.numeric(format(time(x), "%m")) == trgt )
	} else index <- which( as.numeric(format(time(x), "%Y")) == trgt )


  } # if END

    # if 'trgt' is a weather season
    else if (class(trgt)=="character") {

	  # Checking a valid value for 'trgt'
      if (is.na(match(trgt, c("DJF", "MAM", "JJA", "SON") ) ) )
         stop("Invalid argument: 'trgt' must be in c('DJF', 'MAM', 'JJA', 'SON')")

	  # Gets the season each element of 'x' belongs to
      seasons <- time2season(time(x), out.fmt="months")

      # Selects only those elements of 'x' belonging to the desired season
      index <- which(seasons == trgt)
	} # ELSE end

  return(x[index])

} # 'extractzoo' END