#####################################################
# Computes: Hypometric curve corresponding to a DEM #
#####################################################
#   Requires: 'rgdal'                               #
#####################################################
#	Date: 12-Sep-2009, Jul 2010                     #
#####################################################
# Taken from: http://osgeo-org.1803224.n2.nabble.com/hypsometric-integral-from-ecdf-curve-td2231345.html

# 'x'   : 'SpatialGridDataFrame' object with the elevations of the catchment
# Result: Plot in the 'y' axis the elevations of the catchment, and in
#         the 'x' axis, the percentage of the catchment area that is BELOW a given elevation

# Example:
#require(rgdal)
#dem <- readGDAL(x)
#hypsometric(dem)
hypsometric <- function(x, main="Hypsometric Curve",
                        xlab="Relative Area above Elevation, (a/A)",
						ylab="Relative Elevation, (h/H)", col="blue",...) {

  if (class(x) != "SpatialGridDataFrame")
    stop("Invalid argument: 'class(x)' must be 'SpatialGridDataFrame'")

  # Minimum and Maximum elevations in 'dem'
  z.min <- min(x@data, na.rm=TRUE)
  z.max <- max(x@data, na.rm=TRUE)

  # Horizontal dimension of the cells of 'x'
  x.dim <- x@grid@cellsize[1]
  # Vertical dimension of the cells of 'x'
  y.dim <- x@grid@cellsize[2]

  # Maximum area of the 'dem', in [m2]
  max.area <- length(which(!is.na(x@data$band1))) * x.dim * y.dim

  # res$t: elevation values, plus a first value that I don't know what it is
  # res$y: accumulated area BELOW a given elevation value.
  res <- plot.stepfun(ecdf(as.matrix(x)), lwd=0, cex.points=0)

  # Mean elevation in 'dem'
  z.mean.index <- which(round(res$y,3)==0.5)[1]
  z.mean       <- res$t[z.mean.index]

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
  hi <- integrate(f=f, lower=0, upper=1)

  # Drawing a legend with the values of the min, mean, and max elevations
  legend("topright", c(
         paste("Min Elev. :", round(z.min, 2), "[m.a.s.l.]", sep=" "),
         paste("Mean Elev.:", round(z.mean, 1), "[m.a.s.l.]", sep=" "),
		 paste("Max Elev. :", round(z.max, 1), "[m.a.s.l.]", sep=" "),
         paste("Max Area  :", round(max.area/1000000, 1), "[km2]", sep=" "),
         "",
         paste("Integral value :", round(hi$value, 3), sep=" "),
         paste("Integral error :", round(hi$abs.error, 3), sep=" ")
         ),
		 bty="n", cex =0.9, col = c("black","black","black"),
		 lty=c(NULL,NULL,NULL,NULL) ) #bty="n" => no box around the legend

} # 'hypsometric' END
