# File hydroplot.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# .hydroplotts Daily, Monthly and Annual Time Series                           #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         # 
# Started: 2008                                                                #
# Updates: 17-Apr-2011 ; 10-Aug-2011                                           #
#          15-Jan-2014                                                         #
################################################################################
# It requires the function'drawTimeAxis' that is stored in the 'lib_Plot.R' library
# 'x'		 : daily time series of type 'zoo'
# 'x.monthly : monthly time series of type 'zoo'
# 'x.annual' : annual time series of type 'zoo'
# 'win.len1' : number of days for being used in the computation of the first moving average
# 'win.len2' : number of days for being used in the computation of the second moving average
# 'var.type' : string representing the type of variable being plotted (e.g., "Precipitation", "Temperature" or "Flow").
#              ONLY used for labelling the y axis and the title of the plot (in case it is missing)
# 'var.unit' : string representing the measurement unit of the variable being plotted ("mm" for precipitation, "C" for temperature, and "m3/s" for flow).
#              ONLY used for labelling the y axis
# 'main'     : string representing the main title of the plot
# 'pfreq'    : string indicating how many plots are desired by the user.
#              Valid values are:
#              -) 'dma': Daily, Monthly and Annual values are plotted
#              -) 'ma' : Monthly and Annual values are plotted
#              -) 'dm' : Daily and Monthly values are plotted
# 'tick.tstep': string indicating the time step that have to be used for
#               putting the ticks ont he time axis.
#               Possible values are: 'days', 'months', 'years'
# 'lab.tstep' : string indicating the time step that have to be used for
#               putting the labels ont he time axis.

.hydroplotts <- function(x, x.monthly, x.annual, win.len1=0, win.len2=0,
			 pfreq="dma", tick.tstep= "auto", lab.tstep= "auto", lab.fmt=NULL,
                         var.type, var.unit="units", main=NULL, xlab="Time", ylab=NULL, 
                         cex.main=1.3, cex.lab=1.3, cex.axis=1.3, col="blue", 
                         lwd=1, lty=1, ...) {

      # Checking that the user provied a valid class for 'x'   
      valid.class <- c("xts", "zoo")    
      if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
         stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")

      # Checking that 'x.monthly' is a zoo or xts object
      if ( length(x.monthly) > 1 ) {
        if (is.na(match(class(x.monthly), c("zoo", "xts"))))
            stop("Invalid argument: 'x.monthly' must be in c('zoo', 'xts')")
      } # IF end

      # Checking that 'x.annual' is a zoo or xts object
      if ( length(x.annual) > 1 ) {
        if (is.na(match(class(x.annual), c("zoo", "xts"))))
            stop("Invalid argument: 'x.annual' must be be in c('zoo', 'xts')")
      } # IF end

      # Checking that the user provied a valid argument for 'pfreq'
      if (is.na(match(pfreq, c("o", "dma", "ma", "dm"))))
          stop("Invalid argument: 'pfreq' must be in c('o', 'dma', 'ma', 'dm')")
          
      # Valid tseps for ''tick.tstep' and 'lab.tstep' 
      valid.tstep <- c("auto", "years", "quarters", "months", "weeks", "days", 
                       "hours", "minutes", "seconds")

      # Checking that the user provied a valid argument for 'tick.tstep'
      if (is.na(match(tick.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'tick.tstep' must be in c('auto', 'years', 'quarters',
              'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")

      # Checking that the user provied a valid argument for 'lab.tstep'
      if (is.na(match(lab.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'lab.tstep' must be in c('auto', 'years', 'quarters',
              'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")

      # Requiring the Zoo Library (Zoo's ordered observations)
      #require(xts)

      # Booleans indicating if the moving averages for the dayly and monthly
      # time series can be computed and ploted. By default they  are FALSE,
      # and only if the lenght(x) is large enough they are changed into TRUE
      d.ma1 <- FALSE
      d.ma2 <- FALSE
      m.ma1 <- FALSE
      m.ma2 <- FALSE

      # Generating a Moving Average of the Daily time series, with a window width 'win.len1'
      if (win.len1 > 0 ) { # If 'win.len1==0', the moving average is not computed
          win.len <- win.len1
          if (length(x) >= win.len) {
            d.ma1 <- TRUE
            daily.ma1 <- ma.zoo(x, win.len) 
            if (!is.xts(daily.ma1)) daily.ma1 <- as.xts(daily.ma1)
          }            
      } # IF end

      # Generating a Moving Average of the Daily time series, with a window width 'win.len2'
      if (win.len2 > 0 ) {  # If 'win.len2==0', the moving average is not computed
          win.len <- win.len2
          if (length(x) >= win.len) {
            d.ma2 <- TRUE
            daily.ma2 <- ma.zoo(x, win.len)
            if (!is.xts(daily.ma2)) daily.ma2 <- as.xts(daily.ma2)
          }            
      } # IF end

      # Generating a Moving Average of the Monthly time series, with a window width 'win.len1'
      if (win.len1 > 0 ) { # If 'win.len1==0', the moving average is not computed
        win.len <- round(win.len1/365,1)*12
        if (length(x.monthly) >= win.len) {
          m.ma1 <- TRUE
          monthly.ma1 <- ma.zoo( x.monthly, win.len )  }
      } # IF end

      # Generating a Moving Average of the Monthly time series, with a window width 'win.len2'
      if (win.len2 > 0 ) {  # If 'win.len2==0', the moving average is not computed
        win.len <- round(win.len2/365,1)*12
        if (length(x.monthly) >= win.len) {
          m.ma2 <- TRUE
          monthly.ma2 <- ma.zoo( x.monthly, win.len ) }        
      } # IF end
      
      # If 'x' is not 'xts' it is transformed into one
      if ( !(is.xts(x)) ) x <- as.xts(x)

      # Plotting only the original zoo or xts object, without moving averages and legends
      if ( pfreq == "o") {
          # Plotting the Daily Time Series
          plot.xts(x, axes=FALSE, type="o", 
                   main=main, xlab=xlab, ylab=ylab, 
                   cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                   lty=lty, lwd=lwd, ...)
          axis(2, cex.lab=1.3, cex.axis=1.3)
          # Draws monthly ticks in the X axis, but labels only in years
          drawTimeAxis(x, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                    cex.lab=cex.lab, cex.axis=cex.axis, ...)
      } # IF end


      # Plotting the Daily, if needed
      if ( pfreq %in% c("dma", "dm") ) {
          # Generating the title of the Daily Time Series plot
          title <- paste("Daily time series", main, sep= " ")
          # Plotting the Daily Time Series
          # xaxt = "n": is for avoiding drawing the x axis
          plot.xts(x, axes=FALSE, type="o", 
                   main=title, xlab=xlab, ylab=paste(ylab," [", var.unit,"/day]", sep=""), 
                   cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                   lty=lty, lwd=lwd, ...)
          axis(2, cex.lab=cex.lab, cex.axis=cex.axis)
          # Draws monthly ticks in the X axis, but labels only in years
          drawTimeAxis(x, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                    cex.lab=cex.lab, cex.axis=cex.axis, ...)

          if (d.ma1) {
            # Plotting the 1st Moving Average of the Daily time series. If win.len1=365*1 => "Annual Moving Average"
            lines(daily.ma1, type="o", lty=2, lwd=1, col="green", cex = .5) }
          if (d.ma2) {
            # Plotting the 2nd Moving Average of the Daily time series. If win.len2=365*3 => "Moving Average of 3 Years"
            lines(daily.ma2, type="o", lty=3, lwd=1, col="red", cex = .5) }
          # Drawing a legend. y.intersp=0.5, is for the vertical spacin in the legend
          leg.text <- "Daily series"
          leg.lwd  <- lwd
          leg.lty  <- lty
          leg.col  <- col
          if (d.ma1) {
            leg.text <- c(leg.text, paste("MA(", round(win.len1/365,2), " years)", sep="") )
            leg.lwd  <- c(leg.lwd, 1)
            leg.lty  <- c(leg.lty, 2)
            leg.col  <- c(leg.col, "green")
          } # IF end
          if (d.ma2) {
            leg.text <- c(leg.text, paste("MA(", round(win.len2/365,2), " years)", sep="") )
            leg.lwd  <- c(leg.lwd, 1)
            leg.lty  <- c(leg.lty, 3)
            leg.col  <- c(leg.col, "red")
          } # IF end
          legend("topleft", leg.text, bty="n", cex =0.9, col= leg.col, lwd= leg.lwd, lty=leg.lty ) #bty="n" => no box around the legend
      } # IF end


      # Plotting the Monthly, if needed
      if ( pfreq %in% c("dma", "dm", "ma") ) {
        # Generating the title of the Monthly Time Series plot
        title <- paste("Monthly time series", main, sep= " ")
        # Plotting the Monthly time series
        plot.xts(x.monthly, axes=FALSE, type="o",
                 main=title, xlab=xlab, ylab=paste(ylab," [", var.unit,"/month]", sep=""),
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                 lty=lty, lwd=lwd, ... )
        axis(2, cex.lab=cex.lab, cex.axis=cex.axis)


        # Draws monthly ticks in the X axis, but labels only in years
        drawTimeAxis(x.monthly, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                  cex.lab=cex.lab, cex.axis=cex.axis, ...)
        if (m.ma1) {
        # Plotting the 1st Moving Average of the Daily time series. If win.len1=365*1 => "Annual Moving Average"
        lines(monthly.ma1, type="o", lty=2, lwd=1, col="green", cex = .5) }
        if (m.ma2) {
        # Plotting the 2nd Moving Average of the Daily time series. If win.len2=365*3 => "Moving Average of 3 Years"
        lines(monthly.ma2, type="o", lty=3, lwd=1, col="red", cex = .5) }
        # Drawing a legend        
        leg.text <- "Monthly series"
        leg.lwd  <- lwd
        leg.lty  <- lty
        leg.col  <- col
        if (m.ma1) {
          leg.text <- c(leg.text, paste("MA(", round(win.len1/365,1), " years)", sep="") )
          leg.lwd  <- c(leg.lwd, 1)
          leg.lty  <- c(leg.lty, 2)
          leg.col  <- c(leg.col, "green")
        } # IF end
        if (m.ma2) {
          leg.text <- c(leg.text, paste("MA(", round(win.len2/365,1), " years)", sep="") )
          leg.lwd  <- c(leg.lwd, 1)
          leg.lty  <- c(leg.lty, 3)
          leg.col  <- c(leg.col, "red")
        } # IF end
        legend("topleft", leg.text, bty="n", cex =0.9, col= leg.col, lwd= leg.lwd, lty=leg.lty ) #bty="n" => no box around the legend
      } # IF end

      # Plotting the Annual, if needed
      if ( pfreq %in% c("dma", "ma") ) {
          # Generating the title of the Annual Time Series plot
           title <- paste("Annual time series", main, sep= " ")
          # Plotting the Annual time series
          plot.xts(x.annual, axes=FALSE, type="o", 
                   main=title, xlab="Time", ylab=paste(ylab," [", var.unit,"/year]", sep=""),
                   cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                   lty=lty, lwd=lwd, ...)
          axis(2, cex.lab=cex.lab, cex.axis=cex.axis)
          # Draws monthly ticks in the X axis, but labels only in years
          drawTimeAxis(x.annual, tick.tstep="years", lab.tstep="years", lab.fmt="%Y",
                    cex.lab=cex.lab, cex.axis=cex.axis, ...)
      } # IF end

} # '.hydroplotts' end



################################################################################
# BoxPlot of Daily, Monthly and Annual Time Serires                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         # 
################################################################################
# Started: 2008                                                                #
# Updates: 17-Apr-2011                                                         #
#          29-May-2013                                                         #
################################################################################
# 'x'		 : daily time series of type 'zoo'
# 'x.monthly : monthly time series of type 'zoo'
# 'x.annual' : annual time series of type 'zoo'
# 'var.type' : string representing the type of variable being plotted
#              (e.g., "Precipitation", "Temperature" or "Flow").
#              ONLY used for labelling the y axis and the title of the plot (in case it is missing)
# 'var.unit' : string representing the measurement unit of the variable
#              being plotted ("mm" for precipitation, "C" for temperature, and "m3/s" for flow).
#              ONLY used for labelling the y axis
# 'main'     : string representing the main title of the plot
# 'pfreq'    : string indicating how many plots are desired by the user.
#              Valid values are:
#              -) 'dma': Daily, Monthly and Annual values are plotted
#              -) 'dm' : Daily and Monthly values are plotted
#              -) 'ma' : Monthly and Annual values are plotted
#
.hydroplotboxplot <- function(x, x.monthly, x.annual, pfreq="dma",
                              var.type, var.unit="units", 
                              main=NULL, xlab=NULL, ylab=NULL,
                              cex.main=1.3, cex.lab=1.3, cex.axis=1.3, 
                              col="lightblue", 
                              ...
			      ) {

  # Checking that 'x' is a zoo or xts object
  if (is.na(match(class(x), c("zoo", "xts"))))
     stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

  # Checking that 'x.monthly' is a zoo object
  if (is.na(match(class(x.monthly), c("zoo", "xts"))))
     stop("Invalid argument: 'class(x.monthly)' must be in c('zoo', 'xts')")

  # Checking that 'x.annual' is a zoo object
  if (is.na(match(class(x.annual), c("zoo", "xts"))))
     stop("Invalid argument: 'class(x.annual)' must be in c('zoo', 'xts')")

  # Checking that the user provied a valid argument for 'pfreq'
  if (is.na(match(pfreq, c("dma", "ma", "dm"))))
      stop("Invalid argument: 'pfreq' must be in c('dma', 'ma', 'dm')")

 # Checking if the Daily Boxplot have to be plotted
 if ( pfreq %in% c("dma", "dm") ) {
   # Generating a factor based on the year in which each daily date falls
   cyear <- format(time(x), "%Y")
   years <- factor(cyear, levels=unique(cyear), ordered=TRUE)
   # Generating the title of the Daily plot
   title <- paste("Daily Boxplot", main, sep= " ")
   # Drawing boxplot of Daily values against Year
   boxplot( coredata(x)~years, main=title, ylab=paste(ylab," [", var.unit, "/day]", sep=""), 
            cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
 } # IF end

 # Checking if the Monthly Boxplot have to be plotted
 if ( pfreq %in% c("dma", "dm", "ma") ) {
   # Generating a factor based on the month in which each monthly date falls
   cmonth <- format(time(x.monthly), "%b")
   months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)
   # Generating the title of the Monthly plot
   title <- paste("Monthly Boxplot", main, sep= " ")
   # Drawing boxplot of Monthly values against Year
   boxplot( coredata(x.monthly)~months, main=title, ylab=paste(ylab," [", var.unit,"/month]", sep=""), 
            cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
 } # IF end

 # Checking if the Annual Boxplot have to be plotted
 if ( pfreq %in% c("dma", "ma") ) {
   # Generating the title of the Annual plot
   title <- paste("Annual Boxplot", main, sep= " ")
   # Drawing boxplot of Annual values against Year
   boxplot( coredata(x.annual), main=title, ylab=paste(ylab," [", var.unit, "/year]", sep=""), 
            cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
 } # IF end

} #'.hydroplotboxplot' end


################################################################################
# Histogram of Daily, Monthly and Annual Time Serires                          #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         # 
################################################################################
# Started: 2008                                                                #
# Updates: 17-Apr-2011                                                         #
#          29-May-2013                                                         #
################################################################################
# 'x'		 : daily time series of type 'zoo'
# 'x.monthly : monthly time series of type 'zoo'
# 'x.annual' : annual time series of type 'zoo'
# 'var.type' : string representing the type of variable being plotted (e.g., "Precipitation", "Temperature" or "Flow").
#              ONLY used for labelling the y axis and the title of the plot (in case it is missing)
# 'var.unit' : string representing the measurement unit of the variable being plotted ("mm" for precipitation, "C" for temperature, and "m3/s" for flow).
#              ONLY used for labelling the x axis
# 'main'     : string representing the main title of the plot
# 'pfreq'    : string indicating how many plots are desired by the user.
#              Valid values are:
#              -) 'dma': Daily, Monthly and Annual values are plotted
#              -) 'ma' : Monthly and Annual values are plotted
#              -) 'dm' : Daily and Monthly values are plotted
.hydroplothist <- function(x, x.monthly, x.annual, pfreq="dma",
                           var.type, var.unit="units", 
                           main=NULL, xlab=NULL, ylab=NULL, 
                           cex.main=1.3, cex.lab=1.3, cex.axis=1.3, col="lightblue", 
                           ...
			   ) {

      # Checking that 'x' is a zoo or xts object
      if (is.na(match(class(x), c("zoo", "xts"))))
        stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")

      # Checking that 'x.monthly' is a zoo object
      if (is.na(match(class(x.monthly), c("zoo", "xts"))))
        stop("Invalid argument: 'class(x.monthly)' must be in c('zoo', 'xts')")

      # Checking that 'x.annual' is a zoo object
      if (is.na(match(class(x.annual), c("zoo", "xts"))))
        stop("Invalid argument: 'class(x.annual)' must be in c('zoo', 'xts')")

      # Checking that the user provied a valid argument for 'pfreq'
      if (is.na(match(pfreq, c("dma", "ma", "dm"))))
          stop("Invalid argument: 'pfreq' must be in c('dma', 'ma', 'dm')")

     # Checking if the Daily ts have to be plotted
     if ( pfreq %in% c("dma", "dm") ) {
       # Generating the title of the Daily plot
       title <- paste("Daily Histogram", main, sep= " ")
       # Drawing an histogram of Daily Precipitation
       hist(x, br=100, freq=FALSE, main=title, xlab=paste(ylab," [", var.unit, "/day]", sep=""), 
            ylab="Pbb", cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
     } # IF end

     # Checking if the Monthly ts have to be plotted
     if ( pfreq %in% c("dma", "dm", "ma") ) {
       # Generating the title of the Monthly plot
       title <- paste("Monthly Histogram", main, sep= " ")
       # Drawing an histogram of Monthly Precipitation
       hist(x.monthly, br=10, freq=FALSE, main=title, xlab=paste(ylab," [", var.unit, "/month]", sep=""), 
            ylab="Pbb", cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
     } # IF end

     # Checking if the Annual ts have to be plotted
     if ( pfreq %in% c("dma", "ma") ) {
       # Generating the title of the Annual plot
       title <- paste("Annual Histogram", main, sep= " ")
       # Drawing an histogram of Annual Precipitation
       hist(x.annual, br=5, freq=FALSE, main=title, xlab=paste(ylab," [", var.unit, "/year]", sep=""), 
            ylab="Pbb", cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, ...)
     } # IF end

} # '.hydroplothist' end


#########################################################################
# hydroplotseasonal: Seasonal plots of hydrological time series         #
#########################################################################
# Author : Mauricio Zambrano-Bigiarini                                  # 
# Started: 19-Jun-2011                                                  #
# Updates: 10-Aug-2011                                                  #
#########################################################################
.hydroplotseasonal <- function(x, FUN, na.rm=TRUE,
		               tick.tstep= "auto", lab.tstep= "auto", lab.fmt=NULL,
                               var.unit="units", main=NULL, xlab="Time", ylab=NULL, 
                               cex.main=1.3, cex.lab=1.3, cex.axis=1.3, col="blue", 
                               lwd=1, lty=1, stype="default", 
                               season.names=c("Winter", "Spring", "Summer", "Autumn"),
                               h=NULL, ...) {
      
      # checking the class of 'x'      
      if (!is.zoo(x))
         stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")
          
      # Valid tseps for ''tick.tstep' and 'lab.tstep' 
      valid.tstep <- c("auto", "years", "quarters", "months", "weeks", "days", 
                       "hours", "minutes", "seconds")

      # Checking that the user provied a valid argument for 'tick.tstep'
      if (is.na(match(tick.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'tick.tstep' must be in c('auto', 'years', 'quarters',
              'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")

      # Checking that the user provied a valid argument for 'lab.tstep'
      if (is.na(match(lab.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'lab.tstep' must be in c('auto', 'years', 'quarters',
              'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")
              
      # Checking that the user provied a valid value for 'stype'   
      valid.types <- c("default", "FrenchPolynesia")    
      if (length(which(!is.na(match(stype, valid.types )))) <= 0)  
        stop("Invalid argument: 'stype' must be in c('default', 'FrenchPolynesia')")

      # Checking that the user provied a valid argument for 'season.names'
      if ( length(season.names) != 4 )
         stop("Invalid argument: 'season.names' must have 4 elements !")

      # Checking the length of x
      if ( sfreq(x) == "daily" ) {
        if (length(x) < 365 )
          stop("Invalid argument: daily time series need -at least- 365 values !")
      } else if ( sfreq(x) == "monthly" ) {
          if (length(x) < 12 )
            stop("Invalid argument: monthly time series need -at least- 12 values !")
        } else if ( sfreq(x) == "annual" ) 
                 stop("Invalid argument: seasonal plots can not be drawn for annual time series !")    
      
      # Labels for the seasons
      if (stype=="default") { 
        seasons.lab <- c("DJF",  "MAM", "JJA", "SON")
      } else if (stype=="FrenchPolynesia") { 
          seasons.lab <- c("DJFM", "AM",  "JJAS", "ON")
        } # ELSE end 
       
      # Computing the seasonal values
      winter <- dm2seasonal(x, season=seasons.lab[1], FUN=FUN, out.fmt="%Y-%m-%d")
      spring <- dm2seasonal(x, season=seasons.lab[2], FUN=FUN, out.fmt="%Y-%m-%d")
      summer <- dm2seasonal(x, season=seasons.lab[3], FUN=FUN, out.fmt="%Y-%m-%d")
      autumm <- dm2seasonal(x, season=seasons.lab[4], FUN=FUN, out.fmt="%Y-%m-%d")

      # Transforming the seasonal values into xts objects
      winter <- as.xts(winter)
      spring <- as.xts(spring)
      summer <- as.xts(summer)
      autumm <- as.xts(autumm)


      #################################
      # Plotting seasonal time series #
      #################################
      def.par <- par(no.readonly = TRUE) # save default, for resetting... 
      on.exit(par(def.par))
      
      layout( matrix( c(1,1,1,1,1,1,1,1,1,5,5,2,2,2,2,2,2,2,2,2,6,6,3,3,3,3,3,3,3,3,3,7,7,4,4,4,4,4,4,4,4,4,8,8), ncol=11, byrow=TRUE) ) 
      
        if (length(h)==1) h <- rep(h,4)
        
        # winter
        plot.xts(winter, axes=FALSE, type="o", main=paste(season.names[1], " (", seasons.lab[1], ")", sep=""), xlab=xlab, ylab=ylab, 
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                 lty=lty, lwd=lwd, ...)
        axis(2, cex.lab=1.3, cex.axis=1.3)
        drawTimeAxis(winter, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                     cex.lab=cex.lab, cex.axis=cex.axis, ...)
        abline(h=h[1], col="red", lty=2)
                
        # spring
        plot.xts(spring, axes=FALSE, type="o", main=paste(season.names[2], " (", seasons.lab[2], ")", sep=""), xlab=xlab, ylab=ylab, 
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                 lty=lty, lwd=lwd, ...)
        axis(2, cex.lab=1.3, cex.axis=1.3)
        drawTimeAxis(spring, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                     cex.lab=cex.lab, cex.axis=cex.axis, ...)
        abline(h=h[2], col="red", lty=2)
                
        # summer
        plot.xts(summer, axes=FALSE, type="o", main=paste(season.names[3], " (", seasons.lab[3], ")", sep=""), xlab=xlab, ylab=ylab, 
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                 lty=lty, lwd=lwd, ...)
        axis(2, cex.lab=1.3, cex.axis=1.3)
        drawTimeAxis(summer, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                     cex.lab=cex.lab, cex.axis=cex.axis, ...)
        abline(h=h[3], col="red", lty=2)
      
        # autumm
        plot.xts(autumm, axes=FALSE, type="o", main=paste(season.names[4], " (", seasons.lab[4], ")", sep=""), xlab=xlab, ylab=ylab, 
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                 lty=lty, lwd=lwd, ...)
        axis(2, cex.lab=1.3, cex.axis=1.3)
        drawTimeAxis(autumm, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt,
                     cex.lab=cex.lab, cex.axis=cex.axis, ...)
        abline(h=h[4], col="red", lty=2)
      
      #################################
      # Plotting seasonal boxplots    #
      #################################
        boxplot(coredata(winter), col= "lightblue", ylab = ylab, 
                main = paste(season.names[1], " (", seasons.lab[1], ")", sep=""),
                pars=list(cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis))
        abline(h=h[1], col="red", lty=2) 
        boxplot(coredata(spring), col= "lightblue", ylab = ylab, 
                main = paste(season.names[2], " (", seasons.lab[2], ")", sep=""),
                pars=list(cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis))
        abline(h=h[2], col="red", lty=2) 
        boxplot(coredata(summer), col= "lightblue", ylab = ylab, 
                main = paste(season.names[3], " (", seasons.lab[3], ")", sep=""),
                pars=list(cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis))
        abline(h=h[3], col="red", lty=2) 
        boxplot(coredata(autumm), col= "lightblue", ylab = ylab, 
                main = paste(season.names[4], " (", seasons.lab[4], ")", sep=""),
                pars=list(cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis))   
        abline(h=h[4], col="red", lty=2)  
                                
} # .hydroplotseasonal END


################################################################################
# hydroplot: Daily, Monthly and Annual plots of hydrological time series       #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         # 
################################################################################
# Started: 2008                                                                #
# Updates: 19-Apr-2011 ; 19-Jun-2011  ; 10-Aug-2011                            #
#          04-Jun-2012                                                         #
#          04-Apr-2013 ; 29-May-2013                                           #
################################################################################
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
hydroplot <- function(x, FUN, na.rm=TRUE,
                      ptype="ts+boxplot+hist",
		      pfreq="dma",                      
                      var.type,                      
                      var.unit="units",
                      main=NULL, xlab="Time", ylab,
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
                      from, 
                      to,
                      date.fmt= "%Y-%m-%d",
                      stype="default",
                      season.names=c("Winter", "Spring", "Summer", "Autumn"),
                      h=NULL,                      
                      ...) {

     # Checking that the user provied a valid class for 'x'   
     if (!is.zoo(x)) 
        stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo')")
            
     # 'xname' value
     xname <- deparse(substitute(x))

     # 'ylab' value
     if ( missing(ylab) ) { 
       ylab <- xname
     } else if ( is.null(ylab) ) ylab <- xname

     # Checking that the user provied a valid argument for 'ptype'
     if (is.na(match(ptype, c("ts", "ts+boxplot", "ts+hist", "ts+boxplot+hist"))))
            stop("Invalid argument: 'ptype' must be in c('ts', 'ts+boxplot', 'ts+hist', 'ts+boxplot+hist')")

     # Checking that the user provied a valid argument for 'pfreq'
     if ( sfreq(x) == "daily" ) {
       if (is.na(match(pfreq, c("o", "dma", "dm", "ma", "seasonal"))))
          stop("Invalid argument: 'pfreq' must be in c('o', 'dma', 'ma', 'dm', 'seasonal')")
     } else if ( sfreq(x) == "monthly" ) {
         if (is.na(match(pfreq, c("ma", "seasonal")))) {
            message("[Warning: 'x' is a monthly object, so 'pfreq' has been changed to 'ma']")
            pfreq <- "ma"
         }
       } # ELSE end

     if ( (pfreq == "o") & (ptype != "ts") ) {
          message(paste("[Note: pfreq='o' => ptype has been changed to 'ts']" , sep="") )
          ptype <- "ts"
     } # IF end

     # Checking that the user provied a valid argument for 'var.type'
     if (missing(FUN) & (pfreq != "o") ) {
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
     
     ##########################################   
     ## In case 'from' and 'to' are provided  ##
     dates <- time(x)
     
     # Checking the validity of the 'from' argument
     if (missing(from)) { 
        from     <- dates[1]
        from.pos <- 1
     } else {
         from <- as.Date(from, format=date.fmt)
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
     x <- window(x, start=from, end=to)
     #################

     # Assigning a dummy value to FUN, which is not used when pfreq="o"
     if (pfreq == "o") FUN <- mean

     def.par <- par(no.readonly = TRUE) # save default, for resetting...
     on.exit(par(def.par))

     # IF the user wants SEASONAL plots
     if (pfreq == "seasonal") {
       
       # Checking that the user provied a valid value for 'stype'   
       valid.types <- c("default", "FrenchPolynesia")    
       if (length(which(!is.na(match(stype, valid.types )))) <= 0)  
         stop("Invalid argument: 'stype' must be in c('default', 'FrenchPolynesia')")

       # Checking that the user provied a valid argument for 'season.names'
       if ( length(season.names) != 4 )
         stop("Invalid argument: 'season.names' must have 4 elements !")
       
       if (!missing(ptype)) {
         if ( ptype != "ts+boxplot") {
           message("[Note: 'pfreq=seasonal' => 'ptype' has been changed to 'ts+boxplot']")
           ptype <- "ts+boxplot"
         } # IF end
       } # IF end
       
        if ( lab.tstep != "auto" ) {
          if ( lab.tstep != "years" ) {
             message("[Note: 'pfreq=seasonal' => 'lab.tstep' has been changed to 'years']")
             lab.tstep <- "years"
          } # IF end 
        } else lab.tstep <- "years"
        
        if ( !is.null(lab.fmt) ) {       
          if ( lab.fmt != "%Y" ) {
             message("[Note: 'pfreq=seasonal' => 'lab.fmt' has been changed to '%Y']")
             lab.fmt <- "%Y"
          } # IF end
        } else lab.fmt <- "%Y"
       
       .hydroplotseasonal(x=x, FUN=FUN, na.rm=na.rm, tick.tstep= tick.tstep, 
                          lab.tstep= lab.tstep, lab.fmt=lab.fmt, var.unit=var.unit, 
                          main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, 
                          cex.lab=cex.lab, cex.axis=cex.axis, col=col, 
                          stype=stype, season.names=season.names, h=h, ...)
                               
     } else {
     
     if (pfreq != "o") {

       # If 'x' is too short for plotting annual values, 'pfreq' is automatically changed
       if ( ( (sfreq(x) == "daily") & ( length(x) <= 366 ) ) |
            ( (sfreq(x) == "monthly") & ( length(x) <= 12 ) ) ) {
           if ( pfreq %in% c("dma", "ma") ) {
             if (pfreq == "dma") pfreq <- "dm"
             if (pfreq == "ma") pfreq <- "m"
             message(paste("[Warning: your ts is too short for plotting annual time series => 'pfreq'= ", pfreq, "]", sep="") )
           }
       } # IF end

       # Computing the monthly time series
       if ( sfreq(x) == "daily" ) {
         x.monthly <- daily2monthly(x, FUN=FUN, na.rm=na.rm)
       } else if ( sfreq(x) == "monthly" ) {
          x.monthly <- x
          } else x.monthly <- NA

       # Computing the annual time series
       if ( !is.na( match( sfreq(x), c("daily", "monthly") ) ) ) {
         x.annual <- daily2annual(x, FUN=FUN, na.rm=na.rm, out.fmt="%Y-%m-%d")
       } else if ( sfreq(x) == "annual" ) {
          x.annual <- x
          } else x.annual <- NA

     } else {
       x.monthly <- NA
       x.annual  <- NA
       } # ELSE end   
     

     if (ptype=="ts") {

       # Setting up the screen with 3 rows and 3 columns
       if (pfreq == "o") { 
          par(mfcol=c(1,1))
       } else if (pfreq == "dma") { 
            par(mfcol=c(3,1))
         } else if (pfreq %in% c("dm", "ma")) { 
            par(mfcol=c(2,1))
           } # ELSE end
       # Drawing the daily, monthly and annual time series of the variable against time
       .hydroplotts(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                    win.len1=win.len1, win.len2=win.len2, var.type=var.type, 
                    var.unit=var.unit, main=main, xlab=xlab, ylab=ylab,  
                    tick.tstep= tick.tstep, lab.tstep= lab.tstep, lab.fmt, 
                    cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[1], ...)
     } # IF end

     else if ( (ptype=="ts+boxplot") & (pfreq != "seasonal") ) {
       # Setting up the screen with 3 rows and 3 columns
       if (pfreq == "dma") { par(mfcol=c(3,2))
       } else if (pfreq %in% c("dm", "ma")) { par(mfcol=c(2,2))
         } # ELSE end
       # Drawing the daily, monthly and annual time series of the variable against time
       .hydroplotts(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                    win.len1=win.len1, win.len2=win.len2, var.type=var.type, 
                    var.unit=var.unit, main=main, xlab=xlab, ylab=ylab,  
                    tick.tstep= tick.tstep, lab.tstep= lab.tstep, lab.fmt, 
                    cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[1], ...)
       # Drawing a boxplot of the daily, monthly and annual time series of the variable
       .hydroplotboxplot(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                         var.type=var.type, var.unit=var.unit, main=main, xlab=xlab, ylab=ylab, 
                         cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[2], ...)
     } # ELSE end

     else if (ptype=="ts+hist") {
       # Setting up the screen with 3 rows and 3 columns
       if (pfreq == "dma") { par(mfcol=c(3,2))
       } else if (pfreq %in% c("dm", "ma")) { par(mfcol=c(2,2))
         } # ELSE end
       # Drawing the daily, monthly and annual time series of the variable against time
       .hydroplotts(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                    win.len1=win.len1, win.len2=win.len2, var.type=var.type, 
                    var.unit=var.unit, main=main, xlab=xlab, ylab=ylab,  
                    tick.tstep= tick.tstep, lab.tstep= lab.tstep, lab.fmt, 
                    cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[1], ...)
       # Drawing an histogram of the daily, monthly and annual time series of the variable
       .hydroplothist(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                      var.type=var.type, var.unit=var.unit, main=main, xlab=xlab, ylab=ylab, 
                      cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[3], ...)
     } # ELSE end

     else if (ptype=="ts+boxplot+hist") {
       # Setting up the screen with 3 rows and 3 columns
       if (pfreq == "dma") { par(mfcol=c(3,3))
       } else if (pfreq %in% c("dm", "ma")) { par(mfcol=c(2,3))
         } # ELSE end
       # Drawing the daily, monthly and annual time series of the variable against time
       .hydroplotts(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                    win.len1=win.len1, win.len2=win.len2, var.type=var.type, 
                    var.unit=var.unit, main=main, xlab=xlab, ylab=ylab,  
                    tick.tstep= tick.tstep, lab.tstep= lab.tstep, lab.fmt, 
                    cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[1], ...)
       # Drawing a boxplot of the daily, monthly and annual time series of the variable
       .hydroplotboxplot(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                         var.type=var.type, var.unit=var.unit, main=main, xlab=xlab, ylab=ylab, 
                         cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[2], ...)
       # Drawing an histogram of the daily, monthly and annual time series of the variable
       .hydroplothist(x=x, x.monthly=x.monthly, x.annual=x.annual, pfreq=pfreq,
                      var.type=var.type, var.unit=var.unit, main=main, xlab=xlab, ylab=ylab, 
                      cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, col=col[3], ...)
     } # ELSE end
     
   } # ELSE end (if (pfreq == "seasonal")

 } # 'hydroplot end
