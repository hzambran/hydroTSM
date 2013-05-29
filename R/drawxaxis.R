################################################################################
# 'drawTimeAxis': It draws an X axies with daily, monthly, or annual time marks#
################################################################################
# From version 0.3-0 it changed its name from 'drawxaxis' to 'drawTimeAxis', in#
# order to have a more intuitive name. The old 'drawxaxis' function is         #
# deprecated, but still be kept for compatibility reasons.                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates: March 2009, Nov 2010, April 2011                                    #
#          29-May-2013                                                         #
################################################################################
drawTimeAxis <- function(x, tick.tstep="auto", lab.tstep="auto", lab.fmt=NULL, cex.axis=1, ... ) {

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

 # 'lab.fmt': If the user didn't provide a value, default values are used
 #if ( missing(lab.fmt) | is.null(lab.fmt) ) {
 if ( is.null(lab.fmt) ) {
   if (lab.tstep == "auto") {
     lab.fmt <- TRUE
   } else {
       if (lab.tstep %in% c("years", "quarters", "months") ) {
         lab.fmt <- "%b-%Y"
       } else if (lab.tstep %in% c("weeks", "days") ) {
          lab.fmt <- "%Y-%m-%d"
         } else if (lab.tstep %in% c("hours", "minutes") ) {
            lab.fmt <- "%b %d %H:%M"
           } else lab.fmt <- "%d %H:%M:%S"
     } # ELSE end
 } # IF end

 # Margin line values (in mex units) for the axis title, axis labels and axis line
 if (lab.tstep == "auto") {
   mgp = c(3, 2, 0)
 } else mgp = c(3, 1, 0)

 # Ticks: Draws the ticks for the time series in the x axis, without labels
 if (tick.tstep == "auto") {
   ticks <- x
 } else ticks <- suppressWarnings(to.period(x, period=tick.tstep, indexAt='startof')) #xts::to.period
 ticks.coords <- xy.coords(.index(ticks), ticks[, 1])
 axis(side =1, at = ticks.coords$x, labels = FALSE, col = "#BBBBBB", ...)

 # Labels: Computes the string for the labels of the X axis
 if (lab.tstep == "auto") {
   labs.dates  <- suppressWarnings(axTicksByTime(x, ticks.on=lab.tstep, gt=1)) # xts::axTicksByTime ; xts::
   labs.coords <- xy.coords(.index(x), x[, 1]) 
   labs.lab    <- names(labs.dates)
 } else {
   new.dates   <- suppressWarnings(to.period(x, period=lab.tstep, indexAt='startof'))  #xts::to.period
   labs.dates  <- match(index(new.dates), time(x))
   labs.coords <- xy.coords(.index(x), x[, 1])
   labs.lab    <- format(time(new.dates), lab.fmt)
   } # ELSE end
 
 # Drawing the X axis
 axis(side =1, at = labs.coords$x[labs.dates], labels = labs.lab, cex.axis = cex.axis, tcl = -0.7, mgp= mgp, ...)
 if (lab.tstep != "auto") box()

} # 'drawTimeAxis' END


################################################################################
# 'drawxaxis': It draws an X axies with daily, monthly, or annual time marks   #
################################################################################
# Started on 2008                                                              #
# Updates: March 2009, Nov 2010, April 2011                                    #
################################################################################
drawxaxis <- function(x, tick.tstep="auto", lab.tstep="auto", lab.fmt=NULL, cex.axis=1, ... ) {

  drawTimeAxis(x, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt, cex.axis=cex.axis, ... ) 
}
