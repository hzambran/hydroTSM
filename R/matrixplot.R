# File matrixplot.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2009-2020 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

####################################################################
# matrixplot: Plots a color matrix representing the amount of days #
#          with information in a set of gauging stations           #
####################################################################
# Author : Mauricio Zambrano-Bigiarini                             #
# Started: 21-May-2009;                                            #
# Updates: 22-Sep-2009, 24-Sep-2010 ; 23-Aug-2011                  #
#          02-Feb-2015                                             #
#          10-Mar-2020                                             #
#          03-Sep-2024                                             #
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

matrixplot <- function(x, 
                       ColorRamp="Days", 
                       ncolors=70, 
                       main="", 
                       cuts,                                # Numeric, indicating the values used to divide the range of 'x' in the legend of colours. If not provided, it is automatically selected as a function of 'lenght(col)'
                       cuts.dec=2,                          # Number of decimal places used to present the numbers that divide the range of 'x' in the legend of colours
                       cuts.labels,                         # Character indicating the label to be used in the ccolour legend for each one of the values defined by 'cuts'. If not provided, as.character(cuts)' is used
                       cuts.style=c("equal", "pretty", "fixed", "sd", "quantile", "kmeans", "bclust", "fisher"), # discarded becsue takes too much time or not alway provide the required number of classes: "dpih", "headtails", "hclust",  "jenks",
                       legend.cex=1.1,                      # character expansion factor *relative* to current \code{par("cex")} used for the legend text.
                       legend.title="",                     # text to be displayed above the legned of colours (e.g., showing the measurement units of the raster being displayed)
                       legend.title.cex=1.5,                # expansion factor(s) for the legend title
                       legend.fontsize=15,                  # The size of text (in points) used in the legend title                    
                       ...) {
     
  # If 'x' is a zoo, it trys to coherce into a matrix
  if (is.zoo(x)) x <- coredata(x) # zoo::coredata
  
  # Checking that the user provied a valid class for 'x'   
  valid.class <- c("matrix", "data.frame")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0)  
     stop("Invalid argument: 'class(x)' must be in c('matrix', 'data.frame')")

  # If 'x' is a data.frame, it trys to coherce into a matrix
  if (is(x, "data.frame")) x <- as.matrix(x)

  # checking 'cuts.style'
  cuts.style <- match.arg(cuts.style)




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
  if (!(is(ColorRamp, "function") ) ) {
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

  # Creating the colors to be used
  col <- ColorRamp(ncolors)

 
  # Defining the cuts to be used inhe plot
  if (missing(cuts)) {
    ncuts <- length(col) + 1
    temp <-  x[!is.nan(x)]

    lcuts <- classInt::classIntervals(temp, n=length(col), dataPrecision=cuts.dec, style = cuts.style )[["brks"]]
    lcuts <- round(lcuts, cuts.dec)
    ncuts <- length(lcuts)
    col   <- col[1:(ncuts-1)]
  } else {
      if ( (length(col)+1) != length(cuts) ) {
        stop("Invalid argument: 'length(col)+1 != length(cuts)' ") 
      } else lcuts <- cuts
    } # ELSE end


  if ( missing(cuts.labels) ) {
    cuts.labels <- as.character(lcuts)
  } else {
      if (length(lcuts) != length(cuts.labels) )
        stop("Invalid argument: 'length(cuts) != length(cuts.labels)' ") 
    } # ELSE end


  #par(fig=c(0,0.8,0,0.8), new=TRUE)
  #require(lattice) # for levelplot()
  #y <- levelplot(x, scales=list(tck=0, x=list(rot=90)),
  #               col.regions=ColorRamp(ncolors),
  #               #at= seq(0, 366, length.out= ncolors),
  #               main= main,
  #               xlab=NULL, ylab=NULL,...)

  y <- levelplot(x, 
                 scales=list(tck=0, x=list(rot=90)),
                 colorkey = list(title = paste0(legend.title, " \n"),
                                 title.gpar=list(fontsize=legend.fontsize, font=1), #font=2 -> bold font
                                 space = "right",
                                 at=as.numeric(factor(lcuts)),                                                  # equally spaced color bins in the legend
                                 labels=list(labels=cuts.labels, at=as.numeric(factor(lcuts)), cex=legend.cex), # equally spaced color bins in the legend
                                 col=col                                     
                                 ), 
                 main=main,
                 xlab=NULL, ylab=NULL,...,
                 at=lcuts, col.regions=col
                 ) 

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
