########################################################################
#  'vector2zoo': Transforms a numerical vector 'x' with a corresponding#
#             'Date' vector (any format) into a 'zoo' object           #
#                       17-Dic-2008, 01-Oct-2009, 06-Oct-2010          #
########################################################################
#  Transform a numericl vectorial  and  its corresponding dates into
#  a 'zoo' variable, for being used by other procedures of this library

# 'x':      : numeric vector
# 'dates'   : vector with a complete series of dates, in the same order of 'ts'
# 'date.fmt': format in which the dates are stored in 'dates'

# example:
# > x <- read.csv2("Ebro-Daily_TS_by_station-PP-Thr70-349stations-HistoricalPeriod.csv")
# > d <- vector2zoo(x[,2], dates=as.Date(x[,1]) )
# > summary(d)
vector2zoo <- function(x, dates, date.fmt="%Y-%m-%d") {

  # Requiring the Zoo Library
  require(zoo)

  if (is.na(match(class(dates), c("Date", "character", "factor"))))
        stop("Invalid argument: 'class(dates)' must be in c('Date', 'character', 'factor')")

   if (is.na(match(class(dates), c("Date"))))
      dates <- as.Date(dates, format= date.fmt)

  # Transforming into a 'zoo' type the values in the time series
  b <- as.zoo(x)

  # Setting as the date of the 'zoo' object the dates provided by 'dates'
  time(b) <- as.Date(dates, format= date.fmt)

  return( b )

}  # 'vector2zoo' END