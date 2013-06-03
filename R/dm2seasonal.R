# File dm2seasonal.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# dm2seasonal: Generic function for computing seasonal values for every        #
#              year of a daily/monthly zoo object                              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-May-2009 ;                                                       #
# Updates: 31-Aug-2009 ; 19-Jun-2011 ; 08-Aug-2011                             #
################################################################################

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
# 'out.fmt'  : character indicating the format for the output time series. Possible values are:
#              -) "%Y"      : only the year will be used for the time. Default option. (e.g., "1961" "1962"...)
#              -) "%Y-%m-%d": a complete date format will be used for the time. Default option. (e.g., "1961" "1962"...)

dm2seasonal <-function(x, ...) UseMethod("dm2seasonal")


dm2seasonal.default <- function(x, season, FUN, na.rm=TRUE, out.fmt="%Y", ...) {

  # Checking that 'x' is a zoo object
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

  dm2seasonal.zoo(x=x, season=season, FUN=FUN, na.rm=na.rm, out.fmt=out.fmt, ...)

} # 'dm2seasonal.default' END




################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 08-Aug-2011                                                         #
# Updates: 08-Aug-2011                                                         #
################################################################################
dm2seasonal.zoo <- function(x, season, FUN, na.rm=TRUE, out.fmt="%Y", ...) {

  # Checking that the user provied a valid argument for 'season'
  if ( missing(season) ) {
      stop("Missing argument: 'season' must be provided")
  } else { # If 'season' is provided
    
      seasons.default         <- c("DJF",  "MAM", "JJA",  "SON")
      seasons.FrenchPolynesia <- c("DJFM", "AM",  "JJAS", "ON")
          
      # Checking that the user provied a valid class for 'season'   
      valid.seasons <- valid.seasons <- union(seasons.default, seasons.FrenchPolynesia)
             
      if (length(which(!is.na(match(season, valid.seasons )))) <= 0)  
         stop("Invalid argument: 'season' must be in 'c(", paste(valid.seasons, collapse=", "), ")'") 
            
      # Finding out if 'season' belongs to 'seasons.default' or to ' seasons.FrenchPolynesia'.
      if ( season %in% seasons.default ) {
        season.type <- "default"
      } else if ( season %in% seasons.FrenchPolynesia ) {
          season.type <- "FrenchPolynesia"
        } # ELSE end
            
    } # ELSE end

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")

  # Checking the user provide a valid value for 'x'
  if (sfreq(x) %in% c("quarterly", "annual"))
    stop("Invalid argument: 'x' is not a sub-daily, daily, weekly or monthly ts. 'x' is a ", sfreq(x), " ts" )
      
  # Checking 'out.fmt'
  if ( is.na(match(out.fmt, c("%Y", "%Y-%m-%d") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('%Y', '%Y-%m-%d')" )

  # Getting the Monthly values beloonging ONLY to the desired weather season
  s <- extract(x= x, trgt=season)

  # Moving forward all the December values, in order that
  # December of 1991 be used together with Jan/92 and Feb/92,
  # instead of with Jan/91 and Feb/91
  if ( (season == "DJF") | (season == "DJFM") ) {
  
	syears            <- as.numeric(format( time(s), "%Y" ))
	dec.index         <- which(format(time(s), "%m") == 12)
	dec.years         <- syears[dec.index]
	dec.years         <- dec.years + 1
	syears[dec.index] <- dec.years

	s.a <- aggregate( s, by= syears, FUN=FUN, na.rm= na.rm)

	# Removing the last value of december, because it is outside of the analysis period
	if ( (is.matrix(x)) | (is.data.frame(x)) ) {
	  s.a <- s.a[1:(nrow(s.a)-1), ]
	} else s.a <- s.a[1:(length(s.a)-1)]
			
  } else  s.a <- aggregate( s, by= format( time(s), "%Y" ), FUN=FUN, na.rm= na.rm )	

  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(s.a))
  if ( length(nan.index) > 0 ) s.a[nan.index] <- NA 

  # Replacing all the Inf and -Inf by NA's
  # min(NA:NA, na.rm=TRUE) == Inf  ; max(NA:NA, na.rm=TRUE) == -Inf
  inf.index <- which(is.infinite(s.a))
  if ( length(inf.index) > 0 ) s.a[inf.index] <- NA 
  
  # If the user wants a complete data format for the output ts:
  if (out.fmt == "%Y-%m-%d")
    time(s.a) <- as.Date(paste( time(s.a), "-01-01", sep=""))

  return(s.a)

} # 'dm2seasonal.zoo' END



################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-May-2009                                                         #
# Updates: 08-Aug-2011                                                         #
#          29-May-2013 ; 03-Jun-2013                                           #
################################################################################
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
                                   dates=1, date.fmt="%Y-%m-%d",
				   out.type="data.frame", 
				   out.fmt="%Y", ... ) {

  # Checking that the user provied a valid argument for 'out.type'
  if (is.na(match( out.type, c("data.frame", "db") ) ) )
      stop("Invalid argument: 'out.type' must be in c('data.frame', 'db'")

  # Checking that the user provied a valid argument for 'season'
  if ( missing(season) ) {
      stop("Missing argument: 'season' must be provided")
  } else { # If 'season' is provided
    
      seasons.default         <- c("DJF",  "MAM", "JJA",  "SON")
      seasons.FrenchPolynesia <- c("DJFM", "AM",  "JJAS", "ON")
          
      # Checking that the user provied a valid class for 'season'   
      valid.seasons <- valid.seasons <- union(seasons.default, seasons.FrenchPolynesia)
             
      if (length(which(!is.na(match(season, valid.seasons )))) <= 0)  
         stop("Invalid argument: 'season' must be in 'c(", paste(valid.seasons, collapse=", "), ")'" ) 
            
      # Finding out if 'season' belongs to 'seasons.default' or to ' seasons.FrenchPolynesia'.
      if ( season %in% seasons.default ) {
        season.type <- "default"
      } else if ( season %in% seasons.FrenchPolynesia ) {
          season.type <- "FrenchPolynesia"
        } # ELSE end
            
    } # ELSE end

  # Checking that the user provied a valid argument for 'FUN'
  if (missing(FUN)) stop("Missing argument: 'FUN' must be provided")
  
  # Checking 'out.fmt'
  if ( is.na(match(out.fmt, c("%Y", "%Y-%m-%d") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('%Y', '%Y-%m-%d')" )

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

  # Transforming 'x' into zoo
  x <- zoo(x, dates)  
     
  ##############################################################################
  if (out.type == "data.frame") {
  
    dm2seasonal.zoo(x=x, season=season, FUN=FUN, na.rm=na.rm, out.fmt=out.fmt, ...)
    
  } else if (out.type == "db") {

        # Amount of stations in 'x'
        nstations <- ncol(x)

        # ID of all the stations in 'x'
        snames <- colnames(x)

        # Computing the Starting and Ending Year of the analysis
        Starting.Year <- as.numeric(format(range(dates)[1], "%Y"))
        Ending.Year   <- as.numeric(format(range(dates)[2], "%Y"))

        # Amount of Years belonging to the desired season
        nyears <- Ending.Year - Starting.Year + 1

        message("[Starting computations...]")

        # Creating a vector with the names of the field that will be used for storing the results
        field.names <- c("StationID", "Year", "Season", "Value" )

        # Creating the data.frame that will store the computed averages for each subcatchment
        z <- as.data.frame(matrix(data = NA, nrow = nyears*nstations, ncol = 4,
                            byrow = TRUE, dimnames = NULL) )
        colnames(z) <- field.names

        y = x

        for (j in 1:nstations) {

            message("[ Station: ", format(snames[j], width=10, justify="left"),
                    " : ",format(j, width=3, justify="left"), "/",
                    nstations, " => ",
                    format(round(100*j/nstations,2), width=6, justify="left"),
                    "% ]" )

            # Transforming the column of 'x' into a zoo object,
	    # using the dates provided by the user
            tmp <- vector2zoo(x=x[,j], dates=dates, date.fmt=date.fmt)

	    # Computing the seasonal values
            s.a <- dm2seasonal.default(x= tmp, season=season, FUN=FUN, na.rm=na.rm, out.fmt=out.fmt)

            # Putting the annual seasonal values in the output data.frame
            # The first column of 'x' corresponds to the Year
            row.ini <- (j-1)*nyears + 1
            row.fin <- j*nyears

            z[row.ini:row.fin, 1] <- snames[j] # it is automatically repeted 'nyears' times
            z[row.ini:row.fin, 2] <- Starting.Year:Ending.Year
            z[row.ini:row.fin, 3] <- season    # it is automatically repeted 'nyears' times
            z[row.ini:row.fin, 4] <- s.a

        } # FOR end
        
        return( z )

    } # IF end

 } #'dm2seasonal.data.frame' END
 

################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2009                                                                #
# Updates: 2011                                                                #
#          29-May-2013                                                         #
################################################################################
dm2seasonal.matrix  <- function(x, season, FUN, na.rm=TRUE,
                                dates=1, date.fmt="%Y-%m-%d",
				out.type="data.frame", 
				out.fmt="%Y", ... ) {

   x <- as.data.frame(x)
   #NextMethod("daily2annual")
   dm2seasonal.data.frame(x=x, season=season, FUN=FUN, na.rm=na.rm,
                          dates=dates, date.fmt=date.fmt,
			  out.type=out.type, 
			  out.fmt=out.fmt, ... )

} # 'dm2seasonal.matrix  ' END
