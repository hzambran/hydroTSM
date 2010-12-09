#########################################################################
# dm2seasonal: Generic function for computing seasonal values for every #
#              year of a daily/monthly zoo object                       #
#########################################################################
#                  May 15th, 2009; Aug 31th 2009                        #
#########################################################################

# 'x   '    : variable of type 'zoo' or 'data.frame'
# 'season'  : character, indicating the weather season to be used for selecting the data
#             Valid values are:
#             -) "DJF": December, January, February
#             -) "MAM": March, April, May
#             -) "JJA": June, July, August
#             -) "SON": September, October, November
# 'FUN'      : Function that will be applied to ALL the values of 'x' belonging to the given weather season
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the seasonal values  are computed considering only those values different from NA
#              FALSE: if there is AT LEAST one NA within a season, the corresponding values are NA
dm2seasonal <-function(x, ...) UseMethod("dm2seasonal")


dm2seasonal.default <- function(x, season, FUN, na.rm=TRUE, ...) {

  # Checking that the user provied a valid argument for 'x'
  if ( is.na( match( class(x), c("zoo") ) ) )
      stop("Invalid argument: 'class(x)' must be 'zoo'")

  # Checking that the user provied a valid argument for 'season'
  if ( missing(season) ) {
      stop("Missing argument: 'season' must be provided")
  } else # If 'season' is provided
      # Checking a valid value for 'season'
      if (is.na(match(season, c("DJF", "MAM", "JJA", "SON") ) ) )
         stop("Invalid argument: 'season' must be in c('DJF', 'MAM', 'JJA', 'SON')")

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")

  # Checking that 'x' is a Daily or Monthly ts
  if (is.na(match(sfreq(x), c("daily", "monthly") ) ) )
      stop(paste("Invalid argument: 'x' must be a daily or monthly ts, but 'sfreq(x)' is", sfreq(x), sep=" ")  )

  dates <- time(x)

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
  Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

  # Amount of Years belonging to the desired season
  nyears <- Ending.Year - Starting.Year + 1

  # Requiring the Zoo Library
  require(zoo)

  # If 'x' is a daily ts, the Monthly values are computed
  #if ( sfreq(x) == "Daily")  {
  #   x <- daily2monthly(x, FUN, na.rm ) }

  # Getting the Monthly values beloonging ONLY to the desired weather season
  s <- extractzoo(x= x, trgt=season)

  # Moving forward all the December values, in order that
  # December of 1991 be used together with Jan/92 and Feb/92,
  # instead of with Jan/91 and Feb/91
  if (season == "DJF") {
			syears            <- as.numeric(format( time(s), "%Y" ))
			dec.index         <- which(format(time(s), "%m") == 12)
			dec.years         <- syears[dec.index]
			dec.years         <- dec.years + 1
			syears[dec.index] <- dec.years

			s.a <- aggregate( s, by= syears, FUN=FUN, na.rm= na.rm)

			# Removing the last value of december, because it is outside of the analysis period
			s.a <- s.a[1:(length(s.a)-1)]
  } else  {

			s.a <- aggregate( s, by= format( time(s), "%Y" ), FUN=FUN, na.rm= na.rm )
	}

  # Getting the position of all the years in which there were no values
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(s.a))

  # Changing all the NaN's by NA's
  if ( length(nan.index) > 0 ) { s.a[nan.index] <- NA }

  # Getting the position of all the years in which there were no values
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(s.a))

  # Changing all the Inf and -Inf by NA's
  if ( length(inf.index) > 0 ) { s.a[inf.index] <- NA }

  return(s.a)

} # 'dm2seasonal.default' END



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
# 'out.type': string that define the desired type of output. Possible values are
#             -) "data.frame": a data.frame, with as many columns as stations
#                              are included in 'x'
#             -) "db"        : a data.frame, with 4 colums will be produced.
#                              The first column will store the ID of the station,
#                              The second column will store the Year,
#                              The third column will store the season
#                              The fouth column will contain the seasonal value, corresponding to the year specified in the second column
dm2seasonal.data.frame <- function(x, season, FUN, na.rm=TRUE,
                                   dates, date.fmt="%Y-%m-%d",
								   out.type="data.frame",... ) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provied a valid argument for 'season'
  if ( missing(season) ) {
      stop("Missing argument: 'season' must be provided")
  } else # If 'season' is provided
      # Checking a valid value for 'season'
      if (is.na(match(season, c("DJF", "MAM", "JJA", "SON") ) ) )
         stop("Invalid argument: 'season' must be in c('DJF', 'MAM', 'JJA', 'SON')")

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")

  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
     # Checking that the user provied a valid argument for 'dates'
     if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
         stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  # The column with dates is then substracted form 'x' for easening the further computations
  if ( class(dates) == "numeric" ) {
    tmp   <- dates
    dates <- as.Date(x[, dates], format= date.fmt)
    x     <- x[-tmp]
  }  # IF end

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt)

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # Checking a valid value for 'season'
  if (is.na(match(season, c("DJF", "MAM", "JJA", "SON") ) ) )
     stop("Invalid argument: 'season' must be in c('DJF', 'MAM', 'JJA', 'SON')")

  # Amount of stations in 'x'
  nstations <- ncol(x)

  # ID of all the stations in 'x'
  snames <- colnames(x)

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
  Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

  # Amount of Years belonging to the desired season
  nyears <- Ending.Year - Starting.Year + 1

  # Requiring the Zoo Library (Z’s ordered observations)
  require(zoo)

  print("Starting the computations...", quote=FALSE )

  if (out.type == "data.frame") {

	# Vector with the names of the field that will be used for storing the results
	field.names <- c(snames )

	# Creating the data.frame that will store the computed averages for each station
	z <- as.data.frame(matrix(data = NA, nrow = nyears, ncol = nstations,
						byrow = TRUE, dimnames = NULL) )
	colnames(z) <- field.names

	rownames(z) <- Starting.Year:Ending.Year

	z[1:nstations] <- sapply(1:nstations, function(j,y) {

		print( paste("Station: ", format(snames[j], width=10, justify="left"),
					 " : ",format(j, width=3, justify="left"), "/",
					 nstations, " => ",
					 format(round(100*j/nstations,2), width=6, justify="left"),
					 "%", sep=""), quote=FALSE )

		# Transforming the column of 'x' into a zoo object,
		# using the dates provided by the user
		tmp <- vector2zoo(x=x[,j], dates=dates, date.fmt=date.fmt)

		z[,j] <- dm2seasonal.default(x= tmp, season=season, FUN=FUN, na.rm=na.rm)

	}, y = x) # sapply END

  } else if (out.type == "db") {

        # Creating a vector with the names of the field that will be used for storing the results
        field.names <- c("StationID", "Year", "Season", "Value" )

        # Creating the data.frame that will store the computed averages for each subcatchment
        z <- as.data.frame(matrix(data = NA, nrow = nyears*nstations, ncol = 4,
                            byrow = TRUE, dimnames = NULL) )
        colnames(z) <- field.names

        y = x

        for (j in 1:nstations) {

            print( paste("Station: ", format(snames[j], width=10, justify="left"),
                         " : ",format(j, width=3, justify="left"), "/",
                         nstations, " => ",
                         format(round(100*j/nstations,2), width=6, justify="left"),
                         "%", sep=""), quote=FALSE )

            # Transforming the column of 'x' into a zoo object,
		    # using the dates provided by the user
		    tmp <- vector2zoo(x=x[,j], dates=dates, date.fmt=date.fmt)

		    # Computing the seasonal values
		    s.a <- dm2seasonal.default(x= tmp, season=season, FUN=FUN, na.rm=na.rm)

            # Putting the annual seasonal values in the output data.frame
            # The first column of 'x' corresponds to the Year
            row.ini <- (j-1)*nyears + 1
            row.fin <- j*nyears

            z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 'nyears' times
            z[row.ini:row.fin, 2] <- Starting.Year:Ending.Year
            z[row.ini:row.fin, 3] <- season    # it is automatically repeted 'nyears' times
            z[row.ini:row.fin, 4] <- s.a

        } # FOR end

    } # IF end

  return( z )

 } #'dm2seasonal.data.frame' END