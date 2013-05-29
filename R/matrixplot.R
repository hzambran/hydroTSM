# File matrixplot.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

####################################################################
# matrixplot: Plots a color matrix representing the amount of days #
#          with information in a set of gauging stations           #
####################################################################
# Author : Mauricio Zambrano-Bigiarini                             #
# Started: 21-May-2009;                                            #
# Updates: 22-Sep-2009, 24-Sep-2010 ; 23-Aug-2011                  #
####################################################################
# Adapted (and thank you very much) from:
# http://www2.warwick.ac.uk/fac/sci/moac/currentstudents/peter_cock/r/matrix_contour/

# 'x'         : variable of type 'matrix', with the amount of days with information in each station
#               -) The rows represent the gauging stations
#               -) The columns represetn the years, and they stores the amount of
#                  days with information in each station
# 'ColorRamp' :  Character or function defining a personalized color ramp for ploting the maps. \cr
#                Valid character values are in c("Days", "Precipitation", "Temperature",
#                "PCPAnomaly", "PCPAnomaly2" "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3").
# 'ncolors'   :  Number of color intervals that will be used for differentiating
#                from 0 to 366 days with information
# 'main'      :  Main title for the plot

matrixplot <- function(x, ColorRamp="Days", ncolors=70, main="", ...) {
     
  # If 'x' is a zoo, it trys to coherce into a matrix
  if (class(x) == "zoo") x <- coredata(x) # zoo::coredata
  
  # Checking that the user provied a valid class for 'x'   
  valid.class <- c("matrix", "data.frame")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
     stop("Invalid argument: 'class(x)' must be in c('matrix', 'data.frame')")

  # If 'x' is a data.frame, it trys to coherce into a matrix
  if (class(x) == "data.frame") x <- as.matrix(x)

  # Generate the traspose of the matrix, in order to get the years in the 'x' axis
  # and the stations in the 'y' axis
  #x <- t(x)

  Days.cols          <- colorRampPalette(c("red3", "orange", "darkgoldenrod1", "yellow", "darkolivegreen2", "green"))

  #Precipitation.cols <- colorRampPalette(c("aquamarine", "blue", "darkblue"))
  Precipitation.cols <- colorRampPalette(c("red3", "orange", "yellow", "darkolivegreen3", "lightskyblue", "royalblue3"))
  Temperature.cols   <- colorRampPalette(c("yellow4", "yellow", "orange", "red3", "darkred"))

  PCPAnomaly.cols    <- colorRampPalette(c("sienna4", "peachpuff", "royalblue", "blue"))
  PCPAnomaly2.cols   <- colorRampPalette(c("darkred", "red3", "orange", "yellow", "lightskyblue", "royalblue3", "darkblue"))

  TEMPAnomaly.cols   <- colorRampPalette(c("lightyellow", "yellow", "red", "darkred"))
  TEMPAnomaly2.cols  <- colorRampPalette(c("yellow4", "yellow", "orange", "red3", "darkred"))
  TEMPAnomaly3.cols  <- colorRampPalette(c("darkblue", "royalblue3", "lightskyblue", "yellow", "orange", "red3", "darkred"))


  # Another way for temperature colors, using the reverse order (from white to red):
  # Temperature.cols <- rev(heat.colors(100))

  # # Generating palettes of colors
  if (class(ColorRamp) != "function"  ) {
     # Checking that the user provided a valid argument for 'ColorRamp'
    if (is.na(match(ColorRamp, c("Days", "Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2", "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3") ) ) ) {
      stop("Invalid argument: 'ColorRamp' must be in c('Days', 'Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')")
    } else {
      # Assgning the color ramp, when 'ColorRamp' was given as a character
      if (ColorRamp == "Days") {
      ColorRamp <- Days.cols
      } else if (ColorRamp == "Precipitation") {
        ColorRamp <- Precipitation.cols
        } else if (ColorRamp == "Temperature") {
            ColorRamp <- Temperature.cols
          } else if (ColorRamp == "PCPAnomaly") {
               ColorRamp <- PCPAnomaly.cols
            } else if (ColorRamp == "PCPAnomaly2") {
               ColorRamp <- PCPAnomaly2.cols
              }  else if (ColorRamp == "TEMPAnomaly") {
                  ColorRamp <- TEMPAnomaly.cols
                } else if (ColorRamp == "TEMPAnomaly2") {
                    ColorRamp <- TEMPAnomaly2.cols
                  } else if (ColorRamp == "TEMPAnomaly3") {
                      ColorRamp <- TEMPAnomaly3.cols
                    }# ELSE end
      } # ELSE end
  } # IF end

  #par(fig=c(0,0.8,0,0.8), new=TRUE)
  require(lattice) # for levelplot()
  y <- levelplot(x, scales=list(tck=0, x=list(rot=90)),
                 col.regions=ColorRamp(ncolors),
                 #at= seq(0, 366, length.out= ncolors),
                 main= main,
                 xlab=NULL, ylab=NULL,...)

  #par(fig=c(0,0.8,0.55,1), new=TRUE)
  #up <- colMeans(x)
  #up.g <- barplot(up)

  #par(fig=c(0.65,1,0,0.8),new=TRUE)
  #right <- rowMeans(x)
  #r.g <- barplot(right)

  #print(y, split = c(1, 1, 10, 10))
  #print(up.g, split = c(1, 1, 10, 10), newpage = FALSE)

  return(y)
} # 'matrixplot' END
