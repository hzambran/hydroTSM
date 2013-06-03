# File sname2plot.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#  'sname2plot': Given a data.frame whose columns contains the ts              #
#                (without missing dates) of several gauging stations, it       #
#                takes the name of one gauging station and plots 9 graphs      #
#                (see 'hydroplot' description)                                 #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         # 
################################################################################
# Started: 17-Dic-2008                                                         #
# Updates: 19-Apr-2011 ; 10-Aug-2011                                           #
#          18-Oct-2012                                                         #
#          04-Abr-2013 : 29-May-2013 ; 03-Jun-2013                             #
################################################################################
sname2plot <- function(x, sname, FUN, na.rm=TRUE,
                       ptype="ts+boxplot+hist",
		       pfreq="dma",                      
                       var.type,                      
                       var.unit="units",
                       main=NULL, xlab="Time", ylab=NULL,
                       win.len1=0,
                       win.len2=0,                      
                       tick.tstep="auto",
                       lab.tstep="auto",
                       lab.fmt=NULL,
                       cex=0.3,
                       cex.main=1.3,
                       cex.lab=1.3,
                       cex.axis=1.3,
                       col=c("blue", "lightblue", "lightblue"),
                       dates=1, date.fmt = "%Y-%m-%d",
                       from, to, 
                       stype="default", 
                       season.names=c("Winter", "Spring", "Summer", "Autumn"),
                       h=NULL
                       ) {

  # Checking the user provides 'sname'
  if (missing(sname)) { 
    stop("Missing argument: 'sname' must be provided")
  } else
    # Checking the the station provided for the user exist within 'x'
    if ( !(sname %in% colnames(x) ) )
      stop("Invalid argument: ' The station '", sname, "' is not a column name in 'x'")

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
  if (is.na(match(class(dates), c("numeric", "factor", "Date"))))
    stop("Invalid argument: 'dates' must be of class 'numeric', 'factor', 'Date'")

  if (is.na(match(ptype, c("ts", "ts+boxplot", "ts+hist", "ts+boxplot+hist"))))
        stop("'ptype' valid values are: 'ts', 'ts+boxplot', 'ts+hist', 'ts+boxplot+hist'")

  # If 'dates' is a number, it indicates the index of the column of 'x' that stores the dates
  if ( class(dates) == "numeric" ) dates <- as.Date(as.character(x[, dates]), format= date.fmt) # zoo::as.Date

  # If 'dates' is a factor, it have to be converted into 'Date' class,
  # using the date format  specified by 'date.fmt'
  if ( class(dates) == "factor" ) dates <- as.Date(dates, format= date.fmt) # zoo::as.Date

  # If 'dates' is already of Date class, the following line verifies that
  # the number of days in 'dates' be equal to the number of element in the
  # time series corresponding to the 'sname' station
  if ( ( class(dates) == "Date") & (length(dates) != nrow(x) ) )
     stop("Invalid argument: 'length(dates)' must be equal to 'nrow(x)'")  
     
  # Checking that the user provied a valid value for 'stype'   
  valid.types <- c("default", "FrenchPolynesia")    
  if (length(which(!is.na(match(stype, valid.types )))) <= 0)  
     stop("Invalid argument: 'stype' must be in c('default', 'FrenchPolynesia')")  

  # Checking that the user provied a valid argument for 'season.names'
  if ( length(season.names) != 4 )
    stop("Invalid argument: 'season.names' must have 4 elements !")    

  # 'ylab' value
  if ( missing(ylab) ) { 
     ylab <- sname
  } else if ( is.null(ylab) ) ylab <- sname
     
  ##########################################   
  ## In case 'from' and 'to' are provided  ##
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
  if (to.pos < from.pos) stop("Invalid argument: 'to' have to be grater than 'from'")

  ################
  # column index of the station identified by 'sname' within 'x'
  col.index <- which( colnames(x) == sname )

  # If the station name exists within 'x'
  if ( length(col.index) > 0 ) {

    # Slecting the time series within 'x' corresponding to the 'sname' station
    x <- x[ ,col.index]

    # Transform the vector of time series ('x') and the vector with dates ('dates')
    # into a zoo variable, using the format psecified by 'date.fmt'
    x <- vector2zoo(x, dates, date.fmt)
    
    # Extracting a subset of the values
    x <- window(x, start=from, end=to)

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
              cex.axis=cex.axis, col=col, stype=stype, season.names=season.names, h=h)

  } else stop("The station name", sname, "does not exist in 'x'")

}  # 'sname2plot' END

