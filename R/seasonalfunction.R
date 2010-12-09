########################################################################
# seasonalfunction: Generic function for applying any R function to    #
#                   summarize the seasonal values of 'x'               #
########################################################################
#                        Sep 11th, 2009                                #
########################################################################
# 'x   '    : variable of type 'zoo' or 'data.frame'
# 'FUN'      : Function that will be applied to ALL the values in 'x' belonging to each one of the 4 weather seasons
#              (e.g., Fun can be some of c('mean', 'max', 'min', 'sd'))
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA
seasonalfunction <- function(x, ...) UseMethod("seasonalfunction")

seasonalfunction.default <- function(x, FUN, na.rm=TRUE,...) {

	 # Checking that 'x' is a zoo object
	 if (is.na(match(class(x), c("zoo"))))
			stop("Invalid argument: 'x' must be of class 'zoo'")

     # Checking that the user provied a valid argument for 'FUN'
     if (missing(FUN))
         stop("Missing argument: 'FUN' must be provided")

	 # Checking the user provide a valid value for 'x'
	 if (is.na(match(sfreq(x), c("daily", "monthly")))) {
		 stop(paste("Invalid argument: 'x' is not a daily or monthly ts, it is a ", sfreq(x), " ts", sep="") ) }

     # Requiring the Zoo Library (Z’s ordered observations)
	 require(zoo)

     seasons <- c("DJF", "MAM", "JJA", "SON")

     # Creating the output variable
     z <- NA*numeric(4)

     z <- sapply(1:4, function(j) {

		s <- dm2seasonal(x, season=seasons[j], FUN=FUN, na.rm=na.rm)

        # 'as.numeric' is necessary for being able to change the names to the output
	    z[j] <- as.numeric( aggregate( s, by= rep(seasons[j], length(s)), FUN=FUN, na.rm= na.rm ) )

	 }) # sapply END

     ## Replacing the NaNs by 'NA.
     ## NaN's are obtained when using theFUN=mean with complete NA values
     nan.index <- which(is.nan(z))

     if (length(nan.index) > 0  )
       z[nan.index] <- NA

     # Getting the position of all the years in which there were no values
     # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
     inf.index <- which(is.infinite(z))

     # Changing all the Inf and -Inf by NA's
     if ( length(inf.index) > 0 ) { z[inf.index] <- NA }

	 names(z) <- seasons

	 return(z)

} # 'seasonalfunction.default' end



# 'dates'   : "numeric", "factor", "Date" indicating how to obtain the
#             dates for correponding to the 'sname' station
#             If 'dates' is a number, it indicates the index of the column in
#                'x' that stores the dates
#             If 'dates' is a factor, it have to be converted into 'Date' class,
#                using the date format  specified by 'date.fmt'
#             If 'dates' is already of Date class, the following line verifies that
#                the number of days in 'dates' be equal to the number of element in the
#                time series corresponding to the 'st.name' station
# 'date.fmt': format in which the dates are stored in 'dates'.
#             ONLY required when class(dates)=="factor" or "numeric"
# 'out.type': string that define the desired type of output. Possible values are
#             -) "data.frame": a data.frame, with as many columns as stations
#                              are included in 'x', and an additional column indicating the Year
#             -) "db"        : a data.frame, with 3 colums will be produced.
#                              The first column will store the Year,
#                              The second column will store the ID of the station,
#                              The third column will contain the seasonal
#                                value corresponding to that year and that station.
# 'verbose'      : logical; if TRUE, progress messages are printed
seasonalfunction.data.frame <- function(x, FUN, na.rm=TRUE,
                                        dates, date.fmt="%Y-%m-%d",
								        out.type="data.frame",
									    verbose=TRUE,...) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provied a valid argument for 'FUN'
   if (missing(FUN))
         stop("Missing argument: 'FUN' must be provided")

  # Checking that the user provied a valid argument for 'dates'
  if (missing(dates)) {
      stop("Missing argument: 'dates' must be provided")
  } else
    {
       # Checking that the user provied a valid argument for 'dates'
       if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
           stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

        # Verification that the number of days in 'dates' be equal to the number
        # of elements in 'x'
        if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
        stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")
    }  # ELSE end

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

  # Amount of stations in 'x'
  nstations <- ncol(x)

  # ID of all the stations in 'x'
  snames <- colnames(x)

  # Computing the Starting and Ending Year of the analysis
  Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
  Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

  # Amount of Years belonging to the desired period
  nyears <- Ending.Year - Starting.Year + 1

  # Amount of months belonging to the desired period
  nmonths <- 12*nyears

  # Requiring the Zoo Library (Z’s ordered observations)
  require(zoo)

  if (verbose) print("Starting the computations...", quote=FALSE )

  seasons <- c("DJF", "MAM", "JJA", "SON")

  if (out.type == "data.frame") {

	# Vector with the names of the field that will be used for storing the results
	field.names <- seasons

	# Creating the data.frame that will store the computed averages for each station
	z <- as.data.frame(matrix(data = NA, nrow = nstations, ncol = 4,
						byrow = TRUE, dimnames = NULL) )

	z <- sapply(1:nstations, function(j,y) {

		if (verbose) print( paste("Station: ", format(snames[j], width=10, justify="left"),
					              " : ",format(j, width=3, justify="left"), "/",
					              nstations, " => ",
					              format(round(100*j/nstations,2), width=6, justify="left"),
					              "%", sep=""), quote=FALSE )

		# Transforming the column of 'x' into a zoo object,
		# using the dates provided by the user
		tmp <- vector2zoo(x=y[,j], dates=dates, date.fmt=date.fmt)

		# Computing the annual values
		z[j,] <- seasonalfunction.default(x= tmp, FUN=FUN, na.rm=na.rm)

	}, y = x) # sapply END

    z <- t(z) # I don't know WHY !!
    rownames(z) <- snames

  } else if (out.type == "db") {

        # Creating a vector with the names of the field that will be used for storing the results
        field.names <- c("StationID", "Season", "Value" )

        # Creating the data.frame that will store the computed averages for each subcatchment
        z <- as.data.frame(matrix(data = NA, nrow = 4*nstations, ncol = 3,
                           byrow = TRUE, dimnames = NULL) )

        for (j in 1:nstations) {

            if (verbose) print( paste("Station: ", format(snames[j], width=10, justify="left"),
                                      " : ", format(j, width=3, justify="left"), "/",
                                      nstations, " => ",
                                      format(round(100*j/nstations,2), width=6, justify="left"),
                                      "%", sep=""), quote=FALSE )

            # Transforming the column of 'x' into a zoo object,
		    # using the dates provided by the user
		    tmp <- vector2zoo(x=x[,j], dates=dates, date.fmt=date.fmt)

		    # Computing the annual values
		    tmp <- seasonalfunction.default(x= tmp, FUN=FUN, na.rm=na.rm)

			# Putting the annual/monthly values in the output data.frame
            # The first column of 'x' corresponds to the Year
            row.ini <- (j-1)*4 + 1
            row.fin <-  j*4

            z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 4 times
            z[row.ini:row.fin, 2] <- seasons
            z[row.ini:row.fin, 3] <- tmp

        } # FOR end

        colnames(z) <- field.names

    } # IF end

  return( z )

 } #'seasonalfunction.data.frame' END