# File mspplot.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# mspplot: Plots 2 or more interpolated maps on the same graph                 #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 22-Apr-2009                                                         #
# Updates: 25-Apr-2009, Ago 2009, Sep 2009                                     #
#          15-Jan-2014                                                         #
################################################################################
# This function makes an IDW interpolation over a catchment defined by a
# polygonal shapefile, and plots its map. It works only for 1 single time

# 'x.ts'     : vector with the measured value at each station.
#              each value of 'x.ts' has to have as name (names(x) the ID of the station.
#              1) CAN contain as many stations as you want, e.g., all the stations in the your database
#              2) AT LEAST, HAVE to contain the stations that will be used for the interpolations

# 'ColorRamp': Character or function defining a personalized color ramp for ploting the maps.
#              Valid character values are "Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2" "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3"

# 'plot'  : Boolean, indicating if the interpolated values have to be ploted or not
# 'col.nintv': integer, number of colors that have to be used for plotting the interpolated values
# 'col.at'   : to specify at which interpolated values colours change. Valid values are:
#              -) "R"   : uses the default setting of spplot
#              -) "auto": default option. => at = seq(min, max,length.out=col.nintv)
#                         min  <- floor( min(idw["var1.pred"]@data, na.rm=TRUE ) )
#                         max  <- ceiling( max(idw["var1.pred"]@data, na.rm=TRUE) )
#              -) numeric: vector of reals giving the exact values in which the colors have to change
#                          Useful when the user desires the same color for the same value when
#                          comparing to maps with different range of values
# 'main'         : string with the title to be used for the plot


# 'stations.plot': Boolean, indicating if the gauging stations, defined by 'gis.fname' have to be plotted
# 'stations.gis' : 'data.frame' with the stations that will be added to the plot. ONLY required when 'stations.plot' == TRUE
# 'X'            : character, field name in 'x.gis' that stores the EAST coordinate of the stations. ONLY required when 'stations.plot' == TRUE
# 'Y'            : character, field name in 'x.gis' that stores the NORTH coordinate of the stations. ONLY required when 'stations.plot' == TRUE
# 'p4s'          : Data about the projection of 'stations.gis', usually created by the CRS function of the 'sp' package. ONLY required when 'stations.plot' == TRUE

# 'arrow.plot'   : Boolean, indicating if a North Arrow have to be plotted
# 'arrow.offset' : 2D list with the numeric coordinates in which
#                  the North Arrow have to be plotted. e.g., arrow.offset = c(690000,4760000)
# 'arrow.scale'  : Scale (in the map units) to be used for plotting the North Arrow, e.g., scale = 20000

# 'scalebar.plot': Boolean, indicating if a Scale Bar have to be plotted
# 'sb.offset'    : 2D list with the numeric coordinates in which
#                  the North Arrow have to be plotted. e.g., sb.offset = c(400000,4490000)
# 'sb.scale'     : Scale (in the map units) to be used for plotting the
#                  Scale Bar, e.g., scale = 100000, means that the scale bar will have a length of 100km

# 'verbose'      : logical; if TRUE, progress messages are printed


mspplot <- function(x,
                    subcatchments,
                    IDvar=NULL,
                    p4s=CRS(as.character(NA)),
                    plot=TRUE,
                    ColorRamp="PCPAnomaly",
                    col.nintv=10, col.at="auto",
                    main,
                    stations.plot=FALSE, stations.gis, X, Y,
                    arrow.plot=FALSE, arrow.offset, arrow.scale,
                    scalebar.plot=FALSE, sb.offset, sb.scale,
                    verbose=TRUE) {

 ##################################
 #  1) Checking the arguments     #
 ##################################

  # Checking the class of 'x'
  #if (is.na(match(class(x), c("data.frame", "numeric", "integer") ) ) ) {
  #   stop( "Invalid argument: class(x) must be in c('data.frame', 'numeric', 'integer')" ) }

  # Checking that the file 'subcatchments' really exists
  if (class(subcatchments) == "character") {
    if (!file.exists(subcatchments) )
       stop(paste("Invalid argument: the file '", basename(subcatchments), "' doesn't exist", sep="") )
  } # IF end

  # 1) Definition of the Color Ramps
  Precipitation.cols <- colorRampPalette(c("aquamarine", "blue", "darkblue"))
  Temperature.cols   <- colorRampPalette(c("lightyellow", "yellow", "red", "darkred"))

  PCPAnomaly.cols    <- colorRampPalette(c("sienna4", "peachpuff", "royalblue", "blue"))
  PCPAnomaly2.cols   <- colorRampPalette(c("darkred", "red3", "orange", "yellow", "lightskyblue", "royalblue3", "darkblue"))

  TEMPAnomaly.cols   <- colorRampPalette(c("lightyellow", "yellow", "red", "darkred"))
  TEMPAnomaly2.cols  <- colorRampPalette(c("yellow4", "yellow", "orange", "red3", "darkred"))
  TEMPAnomaly3.cols  <- colorRampPalette(c("darkblue", "royalblue3", "lightskyblue", "yellow", "orange", "red3", "darkred"))

  # Another way for temperature colors, using the reverse order (from white to red):
  # Temperature.cols <- rev(heat.colors(100))

  ## Generating palettes of colors
  if (class(ColorRamp) != "function"  ) {
     # Checking that the user provided a valid argument for 'ColorRamp'
    if (is.na(match(ColorRamp, c("Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2", "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3") ) ) ) {
      stop("Invalid argument: 'ColorRamp' must be in c('Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')")
    } else {
      # Assgning the color ramp, when 'ColorRamp' was given as a character
      if (ColorRamp == "Precipitation") {
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

  # Checking that 'col.nintv' is integer
  if ( (trunc(col.nintv) - col.nintv) != 0 )
     stop("Invalid argument: 'col.nintv' must be integer")

  # Checking that the user provied a valid argument for 'col.at'
  if (is.na(match(class(col.at), c("numeric", "integer") ) ) ) {
        if ( is.na(match(col.at, c("R", "auto") ) ) ) {
            stop("Invalid argument: 'col.at' must be in c('R', 'auto') or be a numeric vector")
        } # IF end
  } # IF end

 # If the user wants a plot of the interpolated values and did not provide a title, this is created automatically
  if (plot & missing(main)) { main <- "msplot" }

  # If the user wants to plot the Norht Arrow, we have to check the necessary inputs:
  if (arrow.plot) {
    # Checking that the user provied an argument for 'arrow.offset'
    if (missing(arrow.offset)) stop("Missing argument: 'arrow.offset' must be provided when 'arrow.plot'=TRUE")
    if (missing(arrow.scale)) stop("Missing argument: 'arrow.scale' must be provided when 'arrow.plot'=TRUE")
  } #IF end

  # If the user wants to plot the Scale Bar, we have to check the necessary inputs:
  if (scalebar.plot) {
    # Checking that the user provied an argument for 'sb.offset'
    if (missing(sb.offset)) stop("Missing argument: 'sb.offset' must be provided when 'scalebar.plot'=TRUE")
    if (missing(sb.scale)) stop("Missing argument: 'sb.scale' must be provided when 'scalebar.plot'=TRUE")
  } #IF end

  # If the user wants to plot the gauging stations, we have to check the necessary inputs:
  if (stations.plot) {

     if (missing(stations.gis)) {
       stop( "Missing argument: 'stations.gis' has to be supplied when 'stations.plot=TRUE'" )
     } else {# Checking the class of 'stations.gis'
          if (class(stations.gis) != "data.frame" ) {

	     stop( "Invalid argument: class(stations.gis) have to be 'data.frame' " )

	  } else if (missing(X)) {
	       stop( "Missing argument: 'X' has to be supplied when 'stations.plot=TRUE'" )
	    } else {# Checking that the field 'x' exists in 'stations.gis'
		if ( !(X %in% colnames(stations.gis)) ) {
		    stop(paste("Invalid argument: The field '", X, "' doesn't exist in 'stations.gis'", sep="") )
		} else stations.gis["x"] <- stations.gis[X]
	      } # ELSE end

	    if (missing(Y)) {
	       stop( "Missing argument: 'Y' has to be supplied when 'stations.plot=TRUE'" )
	    } else {# Checking that the field 'y' exists in 'stations.gis'
		if ( !(Y %in% colnames(stations.gis)) ) {
		    stop(paste("Invalid argument: The field '", Y, "' doesn't exist in 'stations.gis'", sep="") )
		} else stations.gis["y"] <- stations.gis[Y]
	      } # ELSE end

	    # Getting the number of stations in 'stations.gis'
	    nstations <- nrow(stations.gis)

       } # ELSE end

  } # IF end;


  # 6.4)Polygon with the catchment, for being put over the interpolations
	# Reading the SubCATCHMENTS of the CATCHMENT
	#require(maptools) #it is necessary for usign the function "readShapePoly"
	if (class(subcatchments) == "character") {

           if (require(maptools)) {
	     if (verbose) message(paste("[reading GIS Subcatchments in: '", basename(subcatchments), "'...]", sep="") )
	     SubCatchments.shp <- maptools::readShapePoly(subcatchments, proj4string=p4s, IDvar= IDvar)
	     # Number of Subcatchmnets
	     nSub <- nrow(SubCatchments.shp@data)
	     if (verbose) message(paste("[Subcatchments found:", nSub, sep=" ") )
	   } else stop( paste("Missing package: You need 'maptools' for reading the '", basename(subcatchments), "' shapefile", sep="") )

	} else {	#  If the user already provided 'subcatchments' as an 'SpatialPolygonsDataFrame' object

	     # Checking that 'IDvar' exists within 'subcatchments'
	    if ( ! is.null(IDvar) ) {
	      if ( is.na( match( IDvar, colnames(subcatchments@data) ) ) )
		 stop("Invalid argument: 'IDvar' does not exists in 'subcatchments'")
	    } # IF end

            SubCatchments.shp <-  subcatchments

	  } #ELSE end

	catchment.l1 = list("sp.polygons", SubCatchments.shp, first=FALSE);

  # 6.5) Points with the stations used for computing the interpolations, for being put over the interpolations
         if (stations.plot) {
	     x.gis <- stations.gis
	     #require(sp)
	     #sp::coordinates(x.gis) <- ~ x + y # This assignement is not supported in R < 2.13.X
	     coordinates(x.gis) <- ~ x + y 
	     
	     # Projecting the coordinates of the meteorological stations into the right system
	     #sp::proj4string(x.gis) = p4s # This assignement is not supported in R < 2.13.X
	     proj4string(x.gis) <- p4s 
	     stations.l2 = list("sp.points", SpatialPoints(x.gis, proj4string = p4s), col="black", first=FALSE);

	     # 6.6) Legend with the Minimum, Maximum and Mean interpolated values
	     #nstations.l3 <- list("sp.text", c(450000, 4600000), paste("Stations used:", nrow(x.gis), sep=" "), cex=0.3 )
	     nstations.l3 <- list("sp.text", c(450000, 4600000), paste("Stations used:", nstations, sep=" "), cex=0.7 )
	} # IF end

  # 6.7) North Arrow
	arrow  <- list("SpatialPolygonsRescale", layout.north.arrow(), offset =arrow.offset, scale = arrow.scale)
  # 6.8) Scale Bar
	# 2D list with the numeric coordinates in which
	# the 'scalebar.min.text' have to be plotted. e.g., scalebar.min.offset = c(690000,4760000)
	sb.min.offset <- c( sb.offset[1], sb.offset[2] + sb.scale/10 )
	# Character, indicating the minimum value that will be ploted in the left eddge of the Scale Bar,
	# in the position specified by 'scalebar.min.offset', e.g. scalebar.min.text = "0"
	sb.min.text   <- "0"
	# 2D list with the numeric coordinates in which the 'scalebar.max.text'
	# have to be plotted. e.g., scalebar.min.offset = c(690000,4760000)
	sb.max.offset <- c( sb.offset[1] + sb.scale, sb.offset[2] + sb.scale/10 )
	# Character, indicating the maximum value that will be ploted in the
	# left eddge of the Scale Bar, in the position specified by 'scalebar.max.offset'
	# e.g. scalebar.max.text = "100km"
	sb.max.text   <- paste(as.character(sb.scale/1000),"km", sep="")

	scale     <- list("SpatialPolygonsRescale", layout.scale.bar(), offset = sb.offset, scale = sb.scale, fill=c("transparent","black"))
	scale.min <- list("sp.text", sb.min.offset, sb.min.text, cex = .8)
	scale.max <- list("sp.text", sb.max.offset, sb.max.text, cex = .8)
	 # 6.9) Final Layout (legend creation)
		 # Creating the layout that will be used for plotting the interpolations
		 # Creating the layout that will be used for plotting the interpolations
	    if (stations.plot) {
		   if ( arrow.plot & scalebar.plot ) { map.layout <- list(arrow, scale, scale.min, scale.max, catchment.l1, stations.l2, nstations.l3)
		   } else if ( arrow.plot & !scalebar.plot ) { map.layout <- list(arrow, catchment.l1, stations.l2, nstations.l3)
			 } else if (!arrow.plot & scalebar.plot) { map.layout <- list(scale, scale.min, scale.max, catchment.l1, stations.l2, nstations.l3)
			   } else {map.layout <- list(catchment.l1, stations.l2, nstations.l3) }
		} else	 # if stations.plot==FALSE
			{
			 if ( arrow.plot & scalebar.plot ) { map.layout <- list(arrow, scale, scale.min, scale.max, catchment.l1)
			 } else if ( arrow.plot & !scalebar.plot ) { map.layout <- list(arrow, catchment.l1)
			   } else if (!arrow.plot & scalebar.plot) { map.layout <- list(scale, scale.min, scale.max, catchment.l1)
				 } else {map.layout <- list(catchment.l1) }
		  } # END if (stations.plot)

	 if (class(col.at)=="numeric") {
	   a <- sp::spplot(x, sp.layout = map.layout, scales=list(draw=TRUE, y=list(rot=90), abbreviate=FALSE),
                       main=main, col.regions= ColorRamp(col.nintv), at=col.at, as.table=TRUE )
	} else {
	  a <- sp::spplot(x, sp.layout = map.layout, scales=list(draw=TRUE, y=list(rot=90), abbreviate=FALSE),
                      main=main, col.regions= ColorRamp(col.nintv), as.table=TRUE )

	  } # ELSE end

	print(a)

} # 'mspplot' END
