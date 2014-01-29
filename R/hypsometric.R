# File hypsometric.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2014 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Computes: Hypometric curve corresponding to a DEM                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 12-Sep-2009                                                         #
# Updates: Jul 2010                                                            #
#          21-May-2012                                                         #
#          29-Jan-2014                                                         #
################################################################################
# Requires: 'sp', 'rgdal'                                                      #
################################################################################
# Taken from: http://osgeo-org.1803224.n2.nabble.com/hypsometric-integral-from-ecdf-curve-td2231345.html

# 'x'   : 'SpatialGridDataFrame' object with the elevations of the catchment
# Result: Plot in the 'y' axis the elevations of the catchment, and in
#         the 'x' axis, the percentage of the catchment area that is BELOW a given elevation

# Example:
#require(rgdal)
#dem <- readGDAL(x)
#hypsometric(dem)
hypsometric <- function(x, 
                        band=1,
                        main="Hypsometric Curve",
                        xlab="Relative Area above Elevation, (a/A)",
                        ylab="Relative Elevation, (h/H)", 
                        col="blue",...) {

  if (class(x) != "SpatialGridDataFrame")
    stop("Invalid argument: 'class(x)' must be 'SpatialGridDataFrame'")
    
  # Checking band argument
  band.error <- FALSE
  if (is.numeric(band) | is.integer(band) ) {
    if ( (band < 1) | (band > length(colnames(x@data))) )
      band.error <- TRUE      
  } else if (is.character(band) )
    if ( !(band %in% colnames(x@data) ) )
      band.error <- TRUE    
  if (band.error) stop("Invalid argument: 'band' does not exist in 'x' !")
    
  # Getting the elevatin data from the DEM
  mydem <- x@data[band][,1]

  # Minimum, Mean and Maximum elevations in 'dem'
  z.min  <- min(mydem, na.rm=TRUE)
  z.mean <- mean(mydem, na.rm=TRUE)
  z.max  <- max(mydem, na.rm=TRUE)

  # Horizontal dimension of the cells of 'x'
  x.dim <- x@grid@cellsize[1]
  # Vertical dimension of the cells of 'x'
  y.dim <- x@grid@cellsize[2]

  # Maximum area of the 'dem', in [m2]
  max.area <- length(which(!is.na(mydem))) * x.dim * y.dim

  # res$t: elevation values, plus a first value that I don't know what it is
  # res$y: accumulated area BELOW a given elevation value.
  res <- plot.stepfun(ecdf(as.matrix(mydem)), lwd=0, cex.points=0)

  # Median elevation in 'dem', based on the ECDF of the elevation data, with precision=3
  z.median.index <- which(round(res$y,3)==0.5)[1]
  z.median       <- res$t[z.median.index]

  if (is.na(z.median)) {
    # Median elevation in 'dem', based on the ECDF of the elevation data, with precision=2
    z.median.index <- which(round(res$y,2)==0.5)[1]
    z.median       <- res$t[z.median.index]
  } else if (is.na(z.median)) {
      z.median <- median(mydem, na.rm=TRUE)
    } # ELSE end
  

  #plot(1 - res$y[-1], res$t[-c(1, length(res$t))],
  #     main=main, xlim=c(0, 1),
  #     type="l", ylab=ylab, xlab=xlab, col=col,...)

  # res$t[1] represent the minimum elevation within the basin
  # res$y[1] represent the accumulated area below the minimum elevation

  # Relative area ABOVE a given elevation
  relative.area <- ( 1 - res$y[-1] )

  # Relative elevation
  relative.elev <- ( res$t[-c(1, length(res$t))] -z.min ) / ( z.max - z.min )

  plot(relative.area, relative.elev,
       xaxt="n", yaxt="n",
       main=main, xlim=c(0, 1), ylim=c(0, 1),
       type="l", ylab=ylab, xlab=xlab, col=col,...)

  # Draws the tickets and labels corresponding to the X axis
  Axis(side = 1, at = seq(0.0, 1, by=0.05), labels = TRUE)

  # Draws the tickets and labels corresponding to the Y axis
  Axis(side = 2, at = seq(0.0, 1, by=0.05), labels = TRUE)

  # Obtaining a functional form of the spline approximation to the hyposometric integral.
  # Possible methods are in c("fmm", "periodic", "natural", "monoH.FC")
  f <- splinefun(relative.area, relative.elev, method="monoH.FC")

  # Computing the hypsometric integral
  hi <- integrate(f=f, lower=0, upper=1, stop.on.error = FALSE)

  # Drawing a legend with the values of the min, mean, and max elevations
  legend("topright", c(
         paste("Min Elev. :",   round(z.min, 2), "[m.a.s.l.]", sep=" "),
         paste("Median Elev.:", round(z.median, 1), "[m.a.s.l.]", sep=" "),
         paste("Mean Elev.:", round(z.mean, 1), "[m.a.s.l.]", sep=" "),
         paste("Max Elev. :",   round(z.max, 1), "[m.a.s.l.]", sep=" "),
         paste("Max Area  :",   round(max.area/1000000, 1), "[km2]", sep=" "),
         "",
         paste("Integral value :", round(hi$value, 3), sep=" "),
         paste("Integral error :", round(hi$abs.error, 3), sep=" ")
         ),
		 bty="n", cex =0.9, col = c("black","black","black"),
		 lty=c(NULL,NULL,NULL,NULL) ) #bty="n" => no box around the legend

} # 'hypsometric' END
