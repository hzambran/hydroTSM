################################################################################
# 'drawxaxis': It draws an X axies with daily, monthly, or annual time marks   #
################################################################################
# Started on 2008                 #
# Modified: March 2009, Nov 2010  #
###################################
# 'x'         : time series that will be plotted using the X axis that will be draw
#               class(x) must be 'ts' or 'zoo'
# 'tick.tstep': Character indicating the time step that have to be used for
#               putting the ticks ont he time axis.
#               Possible values are: 'days', 'months', 'years'
# 'lab.tstep' : Character indicating the time step that have to be used for
#               putting the labels ont he time axis.
#               Possible values are: 'days', 'months', 'years'
# 'cex.axis'  : magnification of axis annotation relative to cex
# 'lab.fmt'   : character, with the format to be used for the label of the axis
drawxaxis <- function(x, tick.tstep="months", lab.tstep="years", lab.fmt, cex.axis=1, ... ) {

 # Checking that the user provied a valid argument for 'tick.tstep'
 if (is.na(match(tick.tstep, c("days", "months", "years") ) ) )
         stop("Invalid argument: 'tick.tstep' must be in c('days', 'months', 'years')")

 # Checking that the user provied a valid argument for 'lab.tstep'
 if (is.na(match(lab.tstep, c("days", "months", "years") ) ) )
         stop("Invalid argument: 'lab.tstep' must be in c('days', 'months', 'years')")

 # If the user didn't provide a value for 'lab.fmt', default values are used
 if (missing(lab.fmt)) {
   if (lab.tstep == "days") {
     lab.fmt <- "%Y-%m-%d"
   } else if (lab.tstep == "months") {
       lab.fmt <- "%b"
     } else if (lab.tstep == "years") {
       lab.fmt <- "%Y"
       }
 } # IF end

 # Computing the position of the tick on the Time axis
 if ( (tick.tstep == "days") | (tick.tstep == "months") ) {

   start <- range( time(x) )[1]
   end   <- range( time(x) )[2]

   # Computes daily or monthly ticks on the X axis, without labels
   tt <- seq(from=start, to=end, by = tick.tstep)

 } else if (tick.tstep == "years") {

      if ( class(time(x)) == "Date" ) {
          start <- as.Date( range( time(x) )[1])
          end   <- as.Date( range( time(x) )[2])
          # Computes the ticks for Annual series in the x axis, without labels
          tt <- seq(from=start, to=end, by = tick.tstep)
      } else if ( class(time(x)) == "character" ) {
           start <- as.numeric( range( time(x) )[1])
           end   <- as.numeric( range( time(x) )[2])
           # Computes the ticks for Annual series in the x axis, without labels
           tt   <- start:end
        } # ELSE END

   } # ELSE END

 # Draws the ticks for the time series in the x axis, without labels
 Axis(side = 1, at = tt, labels = FALSE, ...)

 # Computes the string of the labels of the X axis
 if (lab.tstep == "days" | lab.tstep == "months" ) {

   # Computes the position of the labels of the X axis
   tt <- seq(from=start, to=end, by = lab.tstep)

   labs <- format(tt, lab.fmt)

 } else if (lab.tstep == "years") {

   if ( class(time(x)) == "Date" ) {
          # Computes the position of the labels of the X axis
          tt <- seq(from=start, to=end, by = "months")

          tt <-  subset(tt, format(tt, "%m")=="01")

          labs <- format(tt, lab.fmt)
      } else if ( class(time(x)) == "character" ) {
           start <- as.numeric( range( time(x) )[1])
           end   <- as.numeric( range( time(x) )[2])
           # Computes the position of the labels of the X axis
           tt   <- start:end
           labs <- as.character(tt)
        } # ELSE END

 }  # IF END

 # Draws the labels corresponding to the selected ticks in the X axis
 Axis(side = 1, at = tt, labels = labs, tcl = -0.7, cex.axis = cex.axis, ...)

} # 'drawxaxis' END