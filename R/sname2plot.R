########################################################################
#  'sname2plot': Given a data.frame whose columns contains the ts      #
#             (without missing dates) of several gauging stations, it  #
#             takes the name of one gauging station and plots 9 graphs #
#             (see 'hydroplot' description)                            #
#                             17-Dic-2008                              #
########################################################################

# 'x':               : data.frame whose columns contains the time series
#                      (without missing values) for several gauging stations.
# 'sname'            : character with the name of the station whose values will be ploted.
#                      This name MUST eixst as column name in 'x'
#                      Additinonal columns are allowed, e.g. one column with Dates an other with IDs
# 'dates'            : "numeric", "factor", "Date" indicating how to obtain the
#                      dates for correponding to the 'st.name' station
#                      If 'dates' is a number, it indicates the index of the column in 'x' that stores the dates
#                      If 'dates' is a factor, it have to be converted into 'Date' class,
#                          using the date format  specified by 'date.fmt'
#                      If 'dates' is already of Date class, the following line verifies that
#                          the number of days in 'dates' be equal to the number of element in the
#                          time series corresponding to the 'st.name' station
# 'date.fmt'         : format in which the dates are stored in 'dates'.
#                      ONLY required when class(dates)== "character", "factor" or "numeric"
# 'var.type'         : character representing the type of variable being plotted
#                      Used for determining the function used for computing the
#                      Monthly and Annual values when 'FUN' is missing
#                      Valid values are:
#                      -) "Precipitation" => FUN = sum
#                      -) "Temperature"   => FUN = mean
#                      -) "Flow"          => FUN = mean
# 'FUN'             : ONLY required when 'var.type' is missing
#                     Function that have to be applied for transforming from daily to monthly or annual time step
#                     For precipitation FUN MUST be "sum"
#                     For temperature and flow time series, FUN MUST be "mean"#
# 'na.rm'           : Logical. Should missing values be removed?
#                     TRUE : the monthly and annual values  are computed considering only those values different from NA
#                     FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA
# 'var.unit'		 : string repreenting the measurement unit of the variable being plotted ("mm" for precipitation, "C" for temperature, and "m3/s" for flow)
# 'main'             : string repreenting the main title of the plot. If the user did not provide a title, this is
#                      created automatically as: main= paste(var.type, "at", st.name, sep=" "),
# 'win.len1'		 : number of days for being used in the computation of the first moving average
# 'win.len2'		 : number of days for being used in the computation of the second moving average
# 'ptype'            : type of plot that will be plotted
#                    : ptype= "ts" => only time series
#                    : ptype= "ts+boxplot" => only time series + boxplot
#                    : ptype= "ts+histogram" => only time series + histogram
#                    : ptype= "ts+boxplot+histogram" => time series + boxplot + histogram
# 'tick.tstep'       : string indicating the time step that have to be used for
#                      putting the ticks ont he time axis.
#                      Possible values are: 'days', 'months', 'years'
# 'lab.tstep'        : string indicating the time step that have to be used for
#                      putting the labels ont he time axis.
# 'pfreq'            : Passed to the 'hydroplot' function.
#                      Character indicating how many plots are desired by the user.
#                      Valid values are:
#                      -) 'dma': Daily, Monthly and Annual values are plotted
#                      -) 'ma' : Monthly and Annual values are plotted
#                      -) 'dm' : Daily and Monthly values are plotted
sname2plot <- function(x, sname, FUN, na.rm=TRUE,
                       ptype="ts+boxplot+hist",
		       pfreq="dma",                      
                       var.type,                      
                       var.unit="units",
                       main=NULL, xlab="Time", ylab=NULL,
                       win.len1=365*1,
                       win.len2=365*3,                      
                       tick.tstep="months",
                       lab.tstep="years",
                       lab.fmt,
                       cex=0.7,
                       cex.main=1.3,
                       cex.lab=1.3,
                       cex.axis=1.3,
                       col=c("blue", "lightblue", "lightblue"),
                       dates, date.fmt = "%Y-%m-%d") {

  # Checking the user provides 'sname'
  if (missing(sname)) { stop("Missing argument: 'sname' must be provided")
  } else
    # Checking the the station provided for the user exist within 'x'
    if ( !(sname %in% colnames(x) ) )
      stop(paste("Invalid argument: ' The station '", sname, "' is not a column name in 'x'", sep="") )

  # If monthly or annual values are required, 'FUN' or 'var.type' must be provided
  if (pfreq %in% c("dma", "ma", "dm")) {
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
                     if (var.type=="Precipitation") {
                         FUN <- sum
                         if (missing(var.unit)) { var.unit <- "mm"   }
                     } else if (var.type=="Temperature") {
                              FUN <- mean
                              if (missing(var.unit)) { var.unit <- "dC" }
                            } else if (var.type=="Flow") {
                                   FUN <- mean
                                   if (missing(var.unit)) { var.unit= "m3/s" }
                              }
                     } #ELSE end
    } # IF end
  } # IF end

  # Checking the user provides the dates
  if (missing(dates)) { stop("Missing argument: 'dates' must be provided")
  } else
      if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
          stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  if (is.na(match(ptype, c("ts", "ts+boxplot", "ts+hist", "ts+boxplot+hist"))))
        stop("'ptype' valid values are: 'ts', 'ts+boxplot', 'ts+hist', 'ts+boxplot+hist'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  if ( class(dates) == "numeric" ) dates <- as.Date(x[, dates], format= date.fmt)

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt)

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'sname' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")


  # column index of the station identified by 'sname' within 'x'
  col.index <- which( colnames(x) == sname )

  # If the station name exists within 'x'
  if ( length(col.index) > 0 ) {

    # Slecting the time series within 'x' corresponding to the 'sname' station
    x <- x[ ,col.index]

    # Transform the vector of time series ('x') and the vector with dates ('dates')
    # into a zoo variable, using the format psecified by 'date.fmt'
    x <- vector2zoo(x, dates, date.fmt)

    # 9 plots:
    # 1: Line plot with Daily time series, with 2 moving averages, specified by 'win.len1' and 'win.len2'
    # 2: Line plot with Monthly time series, with 2 moving averages, specified by 'win.len1' and 'win.len2'
    # 3: Line plot with Annual time series
    # 4: Boxplot with daily time series
    # 5: Boxplot with monthly time series
    # 6: Boxplot with annual time series
    # 7: Histogram of the daily time series
    # 8: Histogram of the monthly time series
    # 9: Histogram of the annual time series
    hydroplot(x, FUN=FUN, na.rm=na.rm, ptype=ptype, pfreq=pfreq,                      
          var.type=var.type, var.unit=var.unit, main=main, xlab=xlab, ylab=ylab,
          win.len1=win.len1, win.len2=win.len2, tick.tstep=tick.tstep, 
          lab.tstep=lab.tstep, lab.fmt=lab.fmt, cex=cex, cex.main=cex.main, cex.lab=cex.lab,
          cex.axis=cex.axis, col=col)

  } else stop( paste("The station name", sname, "does not exist in 'x'", sep=" ") )

}  # 'sname2plot' END
