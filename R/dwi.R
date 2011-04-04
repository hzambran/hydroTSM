#####################################################
# Generic Days With Information function            #
#####################################################
dwi <-function(x, ...) UseMethod("dwi")


###################################################
#            Zoo Days with Information            #
###################################################
# This function generates a table indicating the number of days
# with information (<>NA's) within a zoo object,
# aggregated by: Year, Month or Month by Year

# x        : variable of type 'zoo'
# out.unit : aggregation time for the computation of the amount of days with info.
#	     Valid values are:
#            -) "month": monthly;
#            -) "year" : annual;
#            -) "mpy"  : month per year
# from     : Character indicating the starting date for the values stored in all the files that
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# to       : Character indicating the starting date for the values stored in all the files that
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# date.fmt : Character indicating the date format in which you provide 'from' and 'to', e.g. "%d-%m-%Y"

dwi.default <- function(x, out.unit="years", from=range(time(x))[1],
                        to=range(time(x))[2], date.fmt="%Y-%m-%d", tstep="days", ...) {

    # Checking that 'class(x)==zoo'
    if (is.na(match(class(x), c("zoo") ) ) )
     stop("Invalid argument: 'x' must be of class 'zoo'")

    # Checking the validity of the 'out.unit' argument
    if ( is.na( match(out.unit, c("years", "months", "mpy") ) ) ) {
         stop("Invalid argument value: 'out.unit' must be in c('years', 'months', 'mpy')" ) }

    # Sequence of dates within the time period between 'from' and 'to'
    DateSeq <- seq( from=as.Date(from, format=date.fmt),
                    to=as.Date(to, format=date.fmt), by=tstep )

    # Selecting only those data that are within the time period between 'from' and 'to'
    x.sel <- x[ as.Date( time(x), format=date.fmt) %in% DateSeq]
    # Also is possible to use: x.sel <- window(x, start=as.Date(from, format=date.fmt), end=as.Date(to, format=date.fmt) )

    # Computing the Starting and Ending Year of the analysis
    Starting.Year <- as.numeric(format(as.Date(from, format=date.fmt), "%Y"))
    Ending.Year   <- as.numeric(format(as.Date(to, format=date.fmt), "%Y"))

    # Amount of Years belonging to the desired period
    nyears <- Ending.Year - Starting.Year + 1

    if (out.unit == "months")   {

         a <- numeric(12)

         a[1:12] <- sapply(1:12, function(j,y) {
                              tmp         <- extractzoo(y, trgt= j)
                              nona.index  <- which(!is.na(tmp))
                              a[j] <- length( nona.index )
                              }, y = x.sel)
         names(a) <- month.abb
         return(a)

     } # IF end

    else if (out.unit == "years") {

         a <- numeric(nyears)

         a[1:nyears] <- sapply(Starting.Year:Ending.Year, function(j,y) {
                               tmp         <- extractzoo(y, trgt= j)
                               nona.index  <- which(!is.na(tmp))
                               a[j] <- length( nona.index )
                               }, y = x.sel)

         names(a) <- as.character(Starting.Year:Ending.Year)

         return(a)

     } # ELSE IF end

     else if (out.unit == "mpy") {

         a <- matrix(data=NA,nrow=nyears, ncol=12)

         a <- sapply(Starting.Year:Ending.Year, function(i,y) {

                               tmp         <- extractzoo(y, trgt= i)

                               a[i-Starting.Year+1,1:12] <- sapply(1:12, function(j,y) {
                                             tmp2        <- extractzoo(y, trgt= j)
                                             nona.index  <- which(!is.na(tmp2))

                                             a[i-Starting.Year+1,j] <- length( nona.index )
                                         }, y = tmp)
                        }, y = x.sel)

         a <- t(a)

         #Change the names of the columns of the matrix
         colnames(a) <- month.abb
         #Change the names of the rows of the matrix
         rownames(a) <- as.character(Starting.Year:Ending.Year)

         return(a)

    }  # ELSE IF end

 } # 'dwi.default' end


#########################################################################
# dwi.data.frame: days with info in each station stored in a data frame #
#########################################################################
#                             March 21th, 2009                          #
#########################################################################
# This function generates a table indicating the number of days
# with information (<>NA's) within a data.frame

# 'x'         : variable of type 'data.frame'
# out.unit    : aggregation time for the computation of the amount of days with info.
#	        Valid values are:
#               -) "month": monthly;
#               -) "year" : annual;
# from        : starting date for detection of days with inormation
# to          : date format that will be used in the output variable
# date.fmt    : date format of "from" and "to". For CHE files, the format must be "%d-%m-%Y"
# 'dates'     : "numeric", "factor", "Date" indicating how to obtain the
#               dates for correponding to the 'sname' station
#               If 'dates' is a number, it indicates the index of the column in
#                 'x' that stores the dates
#               If 'dates' is a factor, it have to be converted into 'Date' class,
#                 using the date format  specified by 'date.fmt'
#               If 'dates' is already of Date class, the following line verifies that
#                 the number of days in 'dates' be equal to the number of element in the
#                 time series corresponding to the 'st.name' station
# 'verbose'  : logical; if TRUE, progress messages are printed

dwi.data.frame <- function(x, out.unit="years", from, to,
                           date.fmt="%Y-%m-%d", tstep="days", dates=1, verbose=TRUE,...) {

  # Checking the validity of the 'out.unit' argument
  if ( is.na( match(out.unit, c("years", "months") ) ) ) {
       stop("Invalid argument value: 'out.unit' must be in c('years', 'months')" ) }

  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
    {
     # Checking that the user provied a valid argument for 'dates'
     if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

     # Verification that the number of days in 'dates' be equal to
     # the number of elements in 'x'
     if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
          stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
    } # ELSE end

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( class(dates) == "numeric" ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt)
    x     <- x[-tmp]
  }  else
      # If 'dates' is a factor, it have to be converted into 'Date' class,
      # using the date format  specified by 'date.fmt'
      if ( class(dates) == "factor" ) {
	    dates <- as.Date(dates, format= date.fmt)
	  } # IF end

  # Checking the validity of the 'from' argument
  if (missing(from)) { from <- dates[1]
  } else if ( is.na( match(class(from), c("Date", "character") ) ) ) {
            stop("Invalid argument value: 'class(from)' must be in c('Date', 'character')" ) }

  # Checking the validity of the 'to' argument
  if (missing(to)) { to <- dates[length(dates)]
  } else if ( is.na( match(class(to), c("Date", "character") ) ) ) {
            stop("Invalid argument value: 'class(to)' must be in c('Date', 'character')" ) }

  # Sequence of dates within the time period between 'from' and 'to'
  DateSeq <- seq( from=as.Date(from, format=date.fmt),
                  to=as.Date(to, format=date.fmt), by=tstep )

  # Selecting only those data that are within the time period between 'from' and 'to'
  x.sel <- x[dates %in% DateSeq, ]
  # Also is possible to use: x.sel <- window(x, start=as.Date(from, format=date.fmt), end=as.Date(to, format=date.fmt) )

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(as.Date(from, format=date.fmt), "%Y"))
  Ending.Year   <- as.numeric(format(as.Date(to, format=date.fmt), "%Y"))

  # Amount of Years belonging to the desired period
  nyears <- Ending.Year - Starting.Year + 1

  # Amount of stations in 'x.sel'
  nstations <- ncol(x.sel)

  # NAme of the stations in 'x'
  snames <- colnames(x)

  if (out.unit == "years") {
    z <- matrix(data=NA, nrow= nstations, ncol=nyears)
  } else if (out.unit == "months") {
    z <- matrix(data=NA, nrow=nstations, ncol=12)
    }  # ELSE end


  z <- sapply(1:ncol(x.sel), function(j, y) {
              #y[j] <- length( subset(y[,j], !is.na(y[,j]) ) )

              if (verbose) message( paste("Station: ", format(snames[j], width=6, justify="left"),
				          " : ", format(j, width=3, justify="left"), "/",
					  nstations, " => ",
					  format(round(100*j/nstations,2), width=6, justify="left"),
					  "%", sep="") )

              tmp  <- vector2zoo(x=y[,j], dates=dates, date.fmt=date.fmt)

              z[j,] <- dwi.default(x=tmp, out.unit=out.unit, from=from, to=to, date.fmt=date.fmt)

              }, y = x.sel)

  colnames(z) <- snames

  return(z)

} # 'dwi.data.frame' END
