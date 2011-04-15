################################################################################
# 'drawxaxis': It draws an X axies with daily, monthly, or annual time marks   #
################################################################################
# Started on 2008                                                              #
# Updates: March 2009, Nov 2010, April 2011                                    #
################################################################################
drawxaxis <- function(x, tick.tstep="auto", lab.tstep="auto", lab.fmt=NULL, cex.axis=1, ... ) {

 # Valid tseps for ''tick.tstep' and 'lab.tstep' 
 valid.tstep <- c("auto", "years", "months", "weeks", "days", "hours", "minutes", "seconds")

 # Checking that the user provied a valid argument for 'tick.tstep'
 if (is.na(match(tick.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'tick.tstep' must be in c('auto', 'years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")

 # Checking that the user provied a valid argument for 'lab.tstep'
 if (is.na(match(lab.tstep, valid.tstep ) ) )
         stop("Invalid argument: 'lab.tstep' must be in c('auto', 'years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds')")

 require(xts)
   
 # 'lab.fmt': If the user didn't provide a value, default values are used
 if ( missing(lab.fmt) | is.null(lab.fmt) ) {
   if (lab.tstep == "days") {
     lab.fmt <- "%Y-%m-%d"
   } else if (lab.tstep == "months") {
       lab.fmt <- "%b"
     } else if (lab.tstep == "years") {
       lab.fmt <- "%b-%Y"
       } else lab.fmt <- TRUE
 } # IF end

 # Margin line values (in mex units) for the axis title, axis labels and axis line
 if (lab.tstep == "auto") {
   mgp = c(3, 2, 0)
 } else mgp = c(3, 1, 0)

 # Ticks: Draws the ticks for the time series in the x axis, without labels
 if (tick.tstep == "auto") {
   ticks <- x
 } else ticks <- suppressWarnings(to.period(x, period=tick.tstep, indexAt='startof')) 
 ticks.coords <- xy.coords(.index(ticks), ticks[, 1])
 axis(side =1, at = ticks.coords$x, labels = FALSE, col = "#BBBBBB", ...)

 # Labels: Computes the string for the labels of the X axis
 labs.date   <- suppressWarnings(axTicksByTime(x, ticks.on=lab.tstep, format = lab.fmt))
 labs.coords <- xy.coords(.index(x), x[, 1])
 labs.lab    <- names(labs.date)
 axis(side =1, at = labs.coords$x[labs.date], labels = labs.lab, cex.axis = cex.axis, tcl = -0.7, mgp= mgp, ...)

} # 'drawxaxis' END
