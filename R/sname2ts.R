# File sname2ts.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# sname2ts: Station name -> time series                                        #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-Jan-2009                                                         #
# Updates: 18-Oct-2012                                                         #
#          29-May-2013 ; 29-May-2013                                           #
################################################################################
# This function takes a data.frame whose columns contains the time series
# (without missing dates) of several gauging stations, it takes the name
# of one gauging station and extracts a time
# series with daily, monthly or annual time step
# 'x'        : data.frame containing the complete (without missing dates)
#              times series of all the stations.
#              It can also contain 1 column with the dates of the measurements,
#              or they can be provided in a separated way
# 'sname'    : string representing the name of the station, which have to correspond
#              with one column name in 'x'
# 'tstep.out': character that defines the time step of the desired output time series
#              it must be one of { "daily", "monthly", "annual" }
# 'dates'    : "numeric", "factor", "Date" indicating how to obtain the
#              dates for correponding to the 'sname' station
#              If 'dates' is a number, it indicates the index of the column in
#                'x' that stores the dates
#              If 'dates' is a factor, it have to be converted into 'Date' class,
#                using the date format  specified by 'date.fmt'
#              If 'dates' is already of Date class, the following line verifies that
#                the number of days in 'dates' be equal to the number of element in the
#                time series corresponding to the 'st.name' station
# 'date.fmt' : format in which the dates are stored in 'dates'.
#              ONLY required when class(dates)=="factor" or "numeric"
# 'var.type' : character representing the type of variable being plotted
#              Used for determining the function used for computing the
#              Monthly and Annual values when 'FUN' is missing
#              Valid values are:
#              -) "Precipitation" => FUN = sum
#              -) "Temperature"   => FUN = mean
#              -) "Flow"          => FUN = mean
# 'FUN'      : ONLY required when 'var.type' is missing
#              Function that have to be applied for transforming from daily to monthly or annual time step
#              For precipitation FUN MUST be "sum"
#              For temperature and flow time series, FUN MUST be "mean"#
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly and annual values  are computed considering only those values different from NA
#              FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA
sname2ts <- function(x, sname, dates=1, date.fmt="%Y-%m-%d", var.type,
                     tstep.out="daily", FUN, na.rm=TRUE, from, to) {

  # Checking that the user provied a valid argument for 'x'
  if ( is.na( match( class(x), c("data.frame") ) ) )
      stop("Invalid argument: 'x' must be of class 'data.frame'")

  # Checking the user provides 'sname'
  if (missing(sname)) { stop("Missing argument: 'sname' must be provided")
  } else
    # Checking the the station provided for the user exist within 'x'
    if ( !(sname %in% colnames(x) ) )
      stop(paste("Invalid argument: ' The station '", sname, "' is not a column name in 'x'", sep="") )

  # If monthly or annual values are required, 'FUN' or 'var.type' must be provided
  if (tstep.out != "daily") {
    # Checking that the user provied a valid argument for 'var.type'
    if (missing(FUN)) {
       # If the user did not provide a title for the plots, this is created automatically
       if (missing(var.type)) {
         stop("Missing argument: 'var.type' OR 'FUN' must be provided")
       } else # If 'var.type' is provided
        # Checking that the user provied a valid argument for 'var.type'
        if (is.na(match(var.type, c("Precipitation", "Temperature", "Flow") ) ) ) {
              stop("Invalid argument: 'var.type' must be in c('Precipitation', 'Temperature', 'Flow')")
          } else {
                 if (var.type=="Precipitation") { FUN= sum  }
                 else if (var.type=="Temperature") { FUN= mean  }
                 else if (var.type=="Flow") { FUN= mean  }
                 } #ELSE end
    }
  } # IF end

  # Checking that the user provied a valid argument for 'tstep.out'
  if (is.na(match( tstep.out, c("daily", "monthly", "annual") ) ) )
      stop("Invalid argument: 'tstep.out' must be in c('daily', 'monthly', 'annual'")

  # Checking that the user provided a valid argument for 'dates'
  if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
    stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  if ( class(dates) == "numeric" ) dates <- as.Date(as.character(x[, dates]), format= date.fmt) # zoo::as.Date

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt) # zoo::as.Date

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'st.name' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")

  # column index of the station identified by 'sname' within 'x'
  col.index <- which( colnames(x) == sname )

  # If the station name exists within 'x'
  if ( length(col.index) > 0 ) {

  # Selecting the time series within 'x' corresponding to the 'sname' station
  x <- x[ ,col.index]

  # Transform the vector of time series ('x') and the vector with dates ('dates')
  # into a zoo variable, using the format psecified by 'date.fmt'
  x.daily   <- vector2zoo(x, dates, date.fmt="%Y-%m-%d")


  ## In case 'fom' and 'to' are provided  ##

  # Checking the validity of the 'from' argument
  if (missing(from)) { 
     from     <- dates[1]
     from.pos <- 1
  } else {
      from <- as.Date(from, format=date.fmt) # zoo::as.Date
      if ( length( which(dates == from) ) > 0 ) {
        from.pos <- which( dates == from )
       } else stop("Invalid argument: 'from' is not in 'dates' ")
    } # ELSE end

  # Checking the validity of the 'to' argument
   if (missing(to)) { 
     to.pos <- length(dates)
     to     <- dates[to.pos]     
  } else {
      to <- as.Date(to, format=date.fmt) # zoo::as.Date
      if ( length( which(dates == to) ) > 0 ) {
        to.pos <- which( dates == to )
      } else stop("Invalid argument: 'to' is not in 'dates' ")
    } # ELSE end

  # Checking that 'to' is larger than 'from'
  if (to.pos < from.pos) stop("Invalid argument: 'to' have to be located in a row below the row corresponding to 'from'")

  # Extracting a subset of the values
  x.daily <- window(x.daily, start=from, end=to)

  # Output
  if (tstep.out == "daily") { return (x.daily)

    } else if (tstep.out =="monthly") {

      # Transformation from daily to monthly
      x.monthly <- daily2monthly(x.daily, FUN, na.rm )

      return (x.monthly)

      } else if (tstep.out =="annual") {

        # Transformation from daily to annual
        x.annual  <- daily2annual(x.daily, FUN, na.rm )

        return (x.annual)

        } # IF/ELSE/ELSE END

  } # IF end

} # 'sname2ts' END
