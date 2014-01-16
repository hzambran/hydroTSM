# File hydrokrige.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# hydrokrige: (Block) IDW, OK, KED Interpolation over catchments,              #
#             with optional plot                                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# April 22-25th, 2009; September 2009, December 2009, April 2010               #
#          04-Jul-2012 ; 05-Jul-2012 ; 09-Jul-2012                             #   
#          15-Jan-2014                                                         #
################################################################################
# This function makes an IDW interpolation over a catchment defined by a
# polygonal shapefile, and plots its map. It works only for 1 single time

# 'x.ts'     : vector with the measured value at each station.
#              each value of 'x.ts' has to have as name (names(x) the ID of the station.
#              1) CAN contain as many stations as you want, e.g., all the stations in the your database
#              2) AT LEAST, HAVE to contain the stations that will be used for the interpolations

# 'x.gis'    : data.frame with the spatial information (GIS) for the gauging stations.
#              The names of each station, stored in the field 'NAME', have to be equal
#              to the corresponding ID used in 'x.ts'
#               1) CAN contain as many stations as you want, e.g., all the stations in your database
#               2) AT LEAST, HAVE to contain the location of those stations that will be used for the interpolations
#                  The MINIMUM fields that Have to be present in this file, and their corresponding colum index are the following:
#               4: 'x'
#               5: 'y'
#               6: 'ELEVATION'
#               8: 'BASIN_NAME' : basin name, starting with a letter
#              10: 'NAME'       : station name, starting with a letter

# 'X'        : character, field name in 'x.gis' that stores the EAST coordinate of the stations
# 'Y'        : character, field name in 'x.gis' that stores the NORTH coordinate of the stations
# 'sname'    : character, field name in 'x.gis' that stores the name of the stations (have to start by a letter)
# 'bname'    : OPTIONAL. character, field name in 'x.gis' that stores the name of the subcatchment in 'x.gis' that will be analysed
#              ONLY necessary when 'catchment.name' is not "all"
# 'elevation': OPTIONAL. character, field name in the shapefile 'subcatchments' that stores the elevation of the stations (m.a.s.l.).

# 'predictors': OPTIONAL. SpatialGridDataFrame object, with prediction/simulation locations. Usually, a digital elevation model (DEM) read with the 'readGDAL' function of the 'rgdal' package. \cr
#               See the 'newdata' argument in 'gstat::?krige'. \cr
#               It should  contain attribute columns with the independent variables (if present) and  (if locations is a formula) the coordinates with names as defined in 'locations
#               If 'predictors' is missing, the grid to be used as prediction/simulation locations is generated sampling the polygon specified by the user in 'subcatchments', acoording to the arguments provided by 'cells.size' and 'grid.type'

# 'catchment.name': name of the catchment that will be analized. Posble values are:
#                   -)"all"       : ALL the stations in the 'x.gis' will be used
#                   -)other string: ONLY those stations in 'x.gis' with a 'BASIN_NAME' field
#                                   value == 'catchment.name' will be used

# 'type'      : Character, indicating the type of plot required by the user.
#                   Possible values are:
#                   -) "cells" : the interpolated values are shown by each cell individually
#                   -) "block" : the interpolated values are show by each catchment,
#                                where the value for each catchment is computed as the mean value
#                                over all the cells that belong to each subcatchment
#                   -) "both"  : "cells" and "block" are plotted in paralell

# 'subCatchments': filename (with path) of the shapefile with all the
#                        Subcatchments within the Catchment. It has to be of 'polygon' type


# 'IDvar'            : (from ?readShapePoly) a character string the name of a
#                      column in the 'SubCatchments.shp' shapefile DBF containing the ID values
#                      of the shapes - the values will be converted to a character vector

# 'p4s'             : Data about the projection of the GIS files, usually created
#                     by the CRS function of the 'sp' package

# 'cells.size'      : Size of the cells to be used in the regular interpolation grid, [m2]
# 'grid.type'       : Character, indicating the yype of grid to be computed
#                     over the area defined by 'SubCatchments.shp'
#                     Valid values are:
#                     -) 'regular'    : for regular (systematically aligned) sampling; Dedault option
#                     -) 'random'     : for completely spatial random;
#                     -) 'stratified' : for stratified random (one single random
#                                       location in each "cell"); '"nonaligned"' for
#                                       nonaligned systematic sampling (nx random
#                                       y coordinates, ny random x coordinates);
#                     -) 'hexagonal'  : for sampling on a hexagonal lattice;
#                     -) 'clustered'  : for clustered sampling

# 'nmin'    : For local kriging: if the number of nearest observations within distance maxdist
#              is less than 'nmin', a missing value will be generated; see 'maxdist'.
#              By default 'nmin' =1
# 'nmax'    : For local kriging: the number of nearest observations that should be used for a
#              kriging prediction, where nearest is defined in terms of the space
#              of the spatial locations. By default, all observations are used
#              OPTIONAL

# 'distmax' : For local kriging: only observations within a distance of Max.Dist from the prediction
#              location are used for prediction or simulation; if combined with 'nmax',
#              both criteria apply. By default, all observations are used
#              OPTIONAL

# 'ColorRamp' : Function defining the colour ramp to be used for plotting the maps OR
#              character representing the colours to be used in the plot. In the latter case, valid values are: \cr
#              c('Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')

# 'plot'  : Logical, indicating if the interpolated values have to
#              be ploted or not
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

# 'stations.plot': Logical, indicating if the gauging stations, defined by 'gis.fname' have to be plotted
# 'stations.offset' : 2D list with the numeric coordinates in which
#                  the amount of stations have to be plotted. e.g., stations.offset = c(450000, 4600000)
# 'arrow.plot'   : Logical, indicating if a North Arrow have to be plotted
# 'arrow.offset' : 2D list with the numeric coordinates in which
#                  the North Arrow have to be plotted. e.g., arrow.offset = c(690000,4760000)
# 'arrow.scale'  : Scale (in the map units) to be used for plotting the North Arrow, e.g., scale = 20000
# 'scalebar.plot': Logical, indicating if a Scale Bar have to be plotted
# 'sb.offset'    : 2D list with the numeric coordinates in which
#                  the North Arrow have to be plotted. e.g., sb.offset = c(400000,4490000)
# 'sb.scale'     : Scale (in the map units) to be used for plotting the
#                  Scale Bar, e.g., scale = 100000, means that the scale bar will have a length of 100km

# 'allNA.action' : Action to be executed when all the values in 'x.ts' are NA. Valid values are:
#                  -) "error": it will produce an error message. Default option
#                  -) numeric: it will replace all the NA values for the numeric value given here, givin place to a map with a constant value. At your own risk !
# 'verbose'      : logical; if TRUE, progress messages are printed

hydrokrige <- function(x.ts, x.gis, ...) UseMethod("hydrokrige")

hydrokrige.default <- function(x.ts, x.gis,
			       X="x", Y="y",
			       sname,
			       bname,
			       elevation,
			       predictors,
			       catchment.name="all",
			       type="cells",
			       formula,
			       subcatchments,
			       IDvar=NULL,
			       p4s=CRS(as.character(NA)),
			       cell.size=1000, grid.type="regular",
			       nmin=0, nmax = Inf, maxdist = Inf,
			       ColorRamp="PCPAnomaly",
			       plot=TRUE, col.nintv=10, col.at="auto",
			       main,
			       stations.plot=FALSE, stations.offset,
			       arrow.plot=FALSE, arrow.offset, arrow.scale,
			       scalebar.plot=FALSE, sb.offset, sb.scale,
			       verbose=TRUE,
			       allNA.action="error", ...) {

 ##################################
 #  1) Checking the arguments     #
 ##################################

  # If the user didn't provide a value for 'p4s' and used the defaul one
  if ( missing(p4s) ) {
    p4s.exists <- FALSE
  } else p4s.exists <- TRUE

  # Checking the class of 'allNA.action'
  if (is.na(match(class(allNA.action), c("character", "numeric", "integer") ) ) )
     stop( "Invalid argument: class(allNA.action) have to be in c('character', 'numeric, 'integer')" )

  # Many arguments are checked in 'gists2spt'

  # Checking that the user provided a valid value for 'x.gis'
  if ( missing(x.gis) ) {
    stop("Missing argument: 'x.gis' must be provided")
  } else if ( class(x.gis) != "data.frame")
     stop("Invalid argument: 'class(x.gis)' must be 'data.frame'")

  # Checking that there are -at least- 2 stations in 'x.ts' with a location in 'x.gis'
  if (is.na( length( which( names(x.ts) %in% x.gis[, sname] ) ) ) ) {
     stop(paste("Invalid argument: There are not 2 or more stations in 'x.ts' with a spatial location in 'x.gis'") ) }

  # Checking that the user provided a valid value for 'subcatchments' OR 'predictors'
  if ( missing(subcatchments) & missing(predictors) )
    stop("Missing argument: 'subcatchments' or 'predictors' must be provided")

  # When provided, checking that the file 'subcatchments' really exists
  if (!missing(subcatchments)) {
      if (class(subcatchments) == "character") {
	if (!file.exists(subcatchments) )
	   stop(paste("Invalid argument: The file '", basename(subcatchments), "' doesn't exist", sep="") )
      } # IF end
  } # IF end

  # If the user didn't provide 'predictors' the following values are checked
  if (missing(predictors)) {

      # Checking that 'grid.type'
      if (is.na(match(grid.type, c("regular", "random", "stratified", "hexagonal", "clustered") ) ) )
	      stop("Invalid argument: 'grid.type' must be of in c('regular', 'random', 'stratified', 'hexagonal', 'clustered'")

      # Printing the defaul 'cell.size' value when the user didn't provide it
      if (missing(cell.size)) { message(paste("[Note: Missing 'cell.size': using 'cell.size= ", cell.size, " [m]. ]", sep="")) }

  } # IF end

  # Checking that 'nmin' is integer
  if ( !missing(nmin) )
    if ( (trunc(nmin) - nmin) != 0 )
     stop("Invalid argument: 'nmin' must be integer")

  # Checking that 'nmax' is integer
  if ( !missing(nmax) )
    if ( nmax != Inf)
      if ( (trunc(nmax) - nmax) != 0 ) {
        stop("Invalid argument: 'nmax' must be integer") }

  # Checking that the user provied a valid argument for 'ColorRamp'
  if (is.na(match(ColorRamp, c("Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2", "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3") ) ) )
      stop("Invalid argument: 'ColorRamp' must be in c('Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')")

  # Checking that 'type'
  if (is.na(match(type, c("cells", "block", "both") ) ) )
     stop("Invalid argument: 'type' must be of in c('cells', 'block', 'both'")

  # If the user wants block interpolations, he must provide the shapefile with the subcatchments
  if (!is.na(match(type, c("block", "both") ) ) )
    if ( missing(subcatchments) )
       stop("Missing argument: 'subcatchments' must be provided for block interpolations")

  # Checking that 'col.nintv' is integer
  if ( !missing(col.nintv) )
    if ( (trunc(col.nintv) - col.nintv) != 0 )
       stop("Invalid argument: 'col.nintv' must be integer")

  # Checking that the user provied a valid argument for 'col.at'
  if (is.na(match(class(col.at), c("numeric", "integer") ) ) ) {
        if ( is.na(match(col.at, c("R", "auto") ) ) ) {
            stop("Invalid argument: 'col.at' must be in c('R', 'auto') or be a numeric vector")
        } # IF end
  } # IF end

 # If the user wants a plot of the interpolated values and did not provide a title, this is created automatically
  if (plot & missing(main)) {
       if (!hasArg(formula) ) {
         main <- paste("IDW", ColorRamp, ". Nmax=", nmax, sep=" ")
       } else if (hasArg(formula) ) {
         #formula is decomposed in its 3 elements
         f <- strsplit(as.character(formula), " ")
         #main <- paste("Krige formula :", f[2], f[1], f[3], sep=" ")
         main <- paste(as.character(f[2]), as.character(f[1]), as.character(f[3]), sep=" ")
       }
  } # IF  end

  # If the user wants to plot the Norht Arrow, we have to check the necessary inputs:
  if (arrow.plot) {
    # Checking that the user provided an argument for 'arrow.offset'
    if (missing(arrow.offset)) stop("Missing argument: 'arrow.offset' must be provided when 'arrow.plot'=TRUE")
    if (missing(arrow.scale)) stop("Missing argument: 'arrow.scale' must be provided when 'arrow.plot'=TRUE")
  } #IF end

  # If the user wants to plot the Scale Bar, we have to check the necessary inputs:
  if (scalebar.plot) {
    # Checking that the user provide an argument for 'sb.offset'
    if (missing(sb.offset)) stop("Missing argument: 'sb.offset' must be provided when 'scalebar.plot'=TRUE")
    if (missing(sb.scale)) stop("Missing argument: 'sb.scale' must be provided when 'scalebar.plot'=TRUE")
  } #IF end

  # If the user wants to plot the amount of stations, we have to check the necessary inputs:
  if (stations.plot)  {
    # Checking that the user provide an argument for 'stations.offset'
    if (missing(stations.offset))
      stop("Missing argument: 'stations.offset' must be provided when 'stations.plot'=TRUE")
  } # IF end


 # IF ALL the elements in 'x.ts' are NA:
 if ( length(x.ts) == length(which(is.na(x.ts))) ) {

   if ( allNA.action == "error" ) {
     stop("Invalid argument: All the values in x.ts are NA. You need measured values !")
   } else if (is.numeric(allNA.action) ) {
      #names                <- colnames(x.ts)
      x.ts[1:length(x.ts)] <- allNA.action
      #colnames(x.ts)       <- names
      if (verbose) message( "[Warning: All the values in x.ts are NA. They were changed to '", allNA.action, "'. ]" )
     }

 } # IF end


 #######################################################################
 # 1) Reading the shapefile with the Subcatchments                     #
 #######################################################################

 # 1) If the user provided a value for 'subcatchments'
 if (!missing(subcatchments)) {

     # If the user provided the name of the shapefile
     if (class(subcatchments) == "character") {

       # Reading the SubCATCHMENTS of the CATCHMENT
       #require(maptools) #it is necessary for usign the function "readShapePoly"

       if (verbose) message("[reading GIS Subcatchments in: '", basename(subcatchments), "'...]")

       # Reading the Shapefile with the subcatchments
       SubCatchments.shp <- maptools::readShapePoly(subcatchments, proj4string=p4s, IDvar= IDvar)

       # Number of Subcatchmnets
       nSub <- nrow(SubCatchments.shp@data)

       if (verbose) message("[Subcatchments found:", nSub, "]")


     } else  { #  If the user already provided 'subcatchments' as an 'SpatialPolygonsDataFrame' object

        # Checking that 'IDvar' exists within 'subcatchments'
	if ( ! is.null(IDvar) ) {
          if ( !is.na(match(type, c("block", "both") ) ) & is.na( match( IDvar, colnames(subcatchments@data) ) ) )
             stop("Invalid argument: 'IDvar' does not exists in 'subcatchments'")
	} # IF end

        SubCatchments.shp <-  subcatchments

       } # ELSE end


 } # IF end


 #######################################################################################################
 # 3) Verifying the compatibility amonth the projections of 'predictors, 'p4s' and 'SubCatchments.shp' #
 #######################################################################################################

 # 3.1) Verifying the compatibility between 'p4s' with 'predictors' and ''SubCatchments.shp'', when they are present

 #require(sp) # for 'proj4string'

 # If the user provided 'p4s'
 if ( p4s.exists ) {

    # 3.1.2) If the user provided 'subcatchments',
    if ( !missing(subcatchments) ) {

        if ( !identical( CRS(sp::proj4string(SubCatchments.shp)), p4s ) )  {

	    if (verbose) message("[Warning: 'p4s' and 'subcatchments' have different CRS. The projection of the shapefile was changed to the one given by 'p4s': '", p4s@projargs, "']" )
            proj4string(SubCatchments.shp) <- p4s
        } # IF end

    } # IF end

    # 3.2) When 'p4s' doesn't exist
 } else {

      # 3.2.1) If 'predictors' and 'subcatchments' are both present.
      if ( !missing(subcatchments) & !missing(predictors) ) {

	    # Verifying the compatibility between  'predictors' and 'subcatchments'
            if ( !identical( proj4string(SubCatchments.shp), proj4string(predictors) ) )  {

		if (verbose) message("[Warning: 'subcatchments' and 'predictors' has different CRS. The projection of the shapefile was changed to the one given by 'predicotrs': '", proj4string(predictors), "']")
                proj4string(SubCatchments.shp) <- CRS(proj4string(predictors))
		p4s                            <- CRS(proj4string(predictors))

            } # IF end

	  # IF ONLY 'predictors' OR 'subcatchments', was given as argument
        } else {

            # 3.2.2) Assingning the projection of 'SubCatchments.shp' to 'p4s', when 'SubCatchments.shp' was given
            if ( !missing(subcatchments) ) {
                 if ( !is.na(proj4string(SubCatchments.shp) ) ) { #is projected gives NA when the shp is not projected

                    if (verbose) message("[Warning: 'You didn't specified a  projection ('p4s') for 'x.gis'. It was set to the one of 'subcatchments': '", proj4string(SubCatchments.shp), "']")
                    p4s <- CRS(proj4string(SubCatchments.shp))

                } # IF end

            } # ELSE end

	    # 3.2.3) Assingning the projection of 'predictors' to 'p4s', when 'predictors' was given
            if ( !missing(predictors) ) {
                 if ( !is.na(proj4string(predictors) ) )  {

                    if (verbose) message("[Warning: 'You didn't specified a  projection ('p4s') for 'x.gis'. It was set to the one of 'predictors': '", proj4string(SubCatchments.shp), "']")
                    p4s <- CRS(proj4string(predictors))

                } # IF end

            } # ELSE end

          } #ELSE end

    } # ELSE end


 #######################################################################
 # 2) Creating an interpolation grid, if 'predictors, was not provided #
 #######################################################################

 # If the user didn't provide a grid with predictors,
 # the predictor grid is sampled from the Shapefile with the subcatchments
 if ( missing(predictors) ) {

    if (verbose) message("['predictors' was not provided. Computing the grid...]")
    # Defining a sampling GRID. If grid.type="regular', then the grid is made
    # of squared cells of 'cell.size'm x 'cell.size'm with regular spacing
    # For avoiding a random grid (sampled randomly from the first cell), and
    # getting a fixed, reproducable grid, it is neccessary to add the argument "offset = c(0.5, 0.5)"
    predictors <- spsample(SubCatchments.shp, type=grid.type, cellsize=cell.size, offset = c(0.5, 0.5))

    # Making possible that the grid can be used in the interpolations:
    # and transforming from 'SpatialPoints' to 'SpatialPixels' the class of 'predictors'
    gridded(predictors) <- TRUE

 }  # IF end

 ##############################################################################
 # 3) Checking the compatibility of projection between 'predictors' and 'p4s' #
 ##############################################################################

 # If predictors (already given by the user or just computed before) has a projection
 if ( (p4s.exists) & !is.na(proj4string(predictors) ) ) {

     if ( !identical( CRS(proj4string(predictors)), p4s ) )  {

	if (verbose) message("[Warning: 'p4s' and 'predictors' have different CRS. The projection of 'predictors' was changed to the one given by 'p4s': '", p4s@projargs, "']")
	proj4string(predictors) <- p4s

     } # ELSE end

 } #IF end

 #######################################################################
 # 4) Copying the measures of 'x.ts' into 'x.gis', creating 'x.work'
 #######################################################################
 x.work <- gists2spt(x.gis=x.gis, x.ts=x.ts, X=X, Y=Y, elevation=elevation,
                     sname=sname, bname=bname, catchment.name=catchment.name,
                     na.rm=TRUE, p4s=p4s)


 # If 'predictors' was provided, its values are copied to 'x.work'
 # if 'predictors' is a 'SpatialGridDataFrame', it means that it has been
 #    read from a raster file and it has attributes that can be used
 if ( !missing(predictors)  ) {

   if (class(predictors)=="SpatialGridDataFrame" ) {

       # Grid-points overlay.
       # Assigning to all the points in 'x.work', the corresponding fields in 'predictors'
       x.work.ov = overlay(predictors, x.work)

       # Getting the names of the predictor variables in 'predictors'
       pnames <- names(x.work.ov@data)

       # copy the predictors values to 'x.work'
       for (i in 1:length(pnames) )
          x.work[[ pnames[i]] ] <- x.work.ov@data[pnames[i]]

       # Removing the NA's in the predictors, because they give rise to erros in the 'krige' function
       # This should not be made in the previous loop becuase it can
       # raise some errors due different number of rows in the predictors being added
       na.row.index <- NULL
       for ( i in 1:length(pnames) )
          na.row.index <- c(na.row.index, which( is.na( x.work[[ pnames[i]] ] ) ) )

       na.row.index <- unique(na.row.index)
       if (length(na.row.index) > 0 )
          x.work   <- x.work[-na.row.index, ]

   } # IF end

 } # IF end

     #if (verbose) {
     #  print("Dataset used for the interpolations:", quote=FALSE)
     #  print(x.work)
     #} #IF end



 #########################################
 # 4)         Interpolations             #
 #########################################


 ############################
 # 4.1) GRID Interpolations #
 ############################
 #if ( type %in% c("cells", "both") ) {
 if ( type %in% c("cells", "both", "block") ) {   #until clarifying how the krige function aggregate the values into the subcatchments

    if (verbose) message("[Starting grid interpolations...]")

    # If ALL the measured values are EQUAL, the following error ir rised
    # 'chfactor.c", line 130: singular matrix in function LDLfactor()'
    # This line checks for avoiding the previously mentioned error.
    if ( length(unique(x.work$value))  == 1  )  {

        if (verbose)
	      message("[Warning: All the measurements are equal. Assigning the constant value to the predictions]")

        # Flag that indicates if all the measurements are equal
	constant_field <- TRUE

        constant_value <- unique(x.work$value)
        
        # This is just to make a different value, in order to allow the interpolations
        x.work$value[1] <- x.work$value[1] + 1

    } else {
        # Flag that indicates if all the measurements are equal
	constant_field <- FALSE
      } # ELSE end


    # If the user didn't provide a formula, Inverse Distance Weighted (IDW) is used
    if (!hasArg(formula) ) {

        #Library gstat is required for the 'krige/idw' function
        #require(gstat)

        # Interpolating with the INVERSE DISTANCE WEIGHTED, , using the
        # 'nmax' nearest neighbours, 'nmin' minimum number of stations and
        # 'maxdist' maximum searching radious
        x.idw <- gstat::idw(value~1, locations=x.work, newdata=predictors, nmin=nmin, nmax=nmax, maxdist=maxdist, ...)

        # min and max values for colors
        idw.min  <- min(x.idw["var1.pred"]@data, na.rm=TRUE)
        idw.max  <- max(x.idw["var1.pred"]@data, na.rm=TRUE)


    } else { # If the user provided a formula => OK or KED

           if (verbose) {
               #formula is decomposed in its 3 elements
               f <- strsplit(as.character(formula), " ")
               message("[Krige formula :", f[2], f[1], f[3], "]")
           } # IF end

           # REquiring automap library
           #require(automap)

           # If ALL the values in x.work are equal, only 1 variogam family is attempted
           if (constant_field) {
               x.idw   <- gstat::idw(value~1, locations=x.work, newdata=predictors, nmin=nmin, nmax=nmax, maxdist=maxdist)
               idw.min <- idw.max  <- constant_value
           } else {
               x.autokrige.cells <- automap::autoKrige(formula, input_data=x.work, new_data=predictors, nmin=nmin, nmax=nmax, maxdist=maxdist, verbose=verbose,...)
               
               #Assigning the outputs of autokrige
               x.idw <- x.autokrige.cells$krige_output

               # min and max values for colors
               idw.min  <-min(x.idw@data["var1.pred"], na.rm=TRUE)
               idw.max  <-max(x.idw@data["var1.pred"], na.rm=TRUE)
             } #LSE end

     } # ELSE end
     #}

    if (constant_field) {
    
      # Putting NA in 'var1.var'
      x.idw@data["var1.var"]  <- NA

      # When 'krige' is called, an additional field  '"var1.stdev"' is present , from R 2.10 (more or less)
      if (hasArg(formula) ) x.idw@data["var1.stdev"]  <- NA 

      # If the user didn't provide the raster map (class "SpatialGridDataFrame"),
      # all the cells in 'predictors' will receive the constant value
      if (class(predictors) != "SpatialGridDataFrame" ) {

        x.idw@data["var1.pred"] <- constant_value

      } else {
        # If the user provided the raster map, only those cells wihtout an NA in
        # the raster will receive the constant value

          index.without.na                            <- which(!is.na(predictors[[1]]))
          x.idw@data["var1.pred"][index.without.na,1] <- constant_value

        }   # ELSE end

    } # IF end

    #################################################
    # Assigning the result of the grid interpolations
    result <- x.idw

    #################################################
    # IF all the block interpolated values were EQUAL
    if (idw.min == idw.max) {
       idw.min <- idw.min - abs(idw.min)/10
       idw.max <- idw.max + abs(idw.max)/10
    } # ELSE if

 } # IF end


 #############################
 # 4.2) Block Interpolations #
 #############################
 if ( type %in% c("block", "both") ) {

     if (verbose) message("[Starting block interpolations...]")

     ## If the user provided 'subcatchments', here we verify that has the same projection thn 'x.work'
     #if ( !missing(p4s) & !missing(subcatchments) ) {
        #if ( proj4string(SubCatchments.shp) !=  proj4string(x.work) )  {
	      #if (verbose) print(paste("'SubCatchments.shp' and x.gis' have different CRS. The projection of 'SubCatchments.shp' was changed to the one of 'x.gis': '", p4s@projargs, "'", sep=""), quote=FALSE, max.levels=0 )
	      #proj4string(SubCatchments.shp) <- p4s
        #} # IF end
     #} # IF end


     # If the user didn't provide a formula, IDW is used
     if (!hasArg(formula) ) {

        # 10.1.2) Block IDW: Interpolating with IDW using the Subcatchments as 'newdata'
        #x.idw.block <- idw(value~1, locations=x.work, newdata=SubCatchments.shp, nmin=nmin, nmax=nmax, maxdist=maxdist,...) # So far is not clear how the krige function aggregate into subbasins

        # this assignment is only needed for counting the number of subcatchments in wich we need a value
        x.idw.block <- SubCatchments.shp

        #if (!is.null(IDvar)) x.idw.block@data[[IDvar]] <- NULL
        
        if (  class(predictors) == "SpatialGridDataFrame" ) {
           tmp.block <- overlay( as(x.idw["var1.pred"], "SpatialPixelsDataFrame"), SubCatchments.shp, fn = mean)
        } else tmp.block <-  overlay(x.idw["var1.pred"], SubCatchments.shp, fn = mean)

        #if ( length(x.idw.block@data[["var1.pred"]]) == length( tmp.block[,1]) ) {
        #  x.idw.block@data[["var1.pred"]] <- tmp.block[,1]
        #}  else stop("Error: The size of -at least- one Subcatchment is smaller than 'cellsize'")
        
        if ( nrow(x.idw.block@data) == length( tmp.block[,1]) ) {
          x.idw.block@data[["var1.pred"]] <- tmp.block[,1]
        }  else stop("Error: The size of -at least- one Subcatchment is smaller than 'cellsize'")

	      # min and max values for colors
        idw.blk.min  <- min(x.idw.block["var1.pred"]@data, na.rm=TRUE)
        idw.blk.max  <- max(x.idw.block["var1.pred"]@data, na.rm=TRUE)

     } else { # If the user provided a formula => OK or KED

        if (verbose) {
            #formula is decomposed in its 3 elements
            f <- strsplit(as.character(formula), " ")
            message(paste("[Krige formula :", f[2], f[1], f[3], "]", sep=" "))
        }  # IF end

	#require(automap)
        #x.autokrige.block <- automap::autoKrige(formula, input_data=x.work, new_data=SubCatchments.shp, nmin=nmin, nmax=nmax, maxdist=maxdist, verbose=verbose,...)
	#x.idw.block <- x.autokrige.block$krige_output

	x.idw.block <- SubCatchments.shp
        #if (!is.null(IDvar)) x.idw.block@data[[IDvar]] <- NULL

        #tmp.block <- overlay( as(x.idw["var1.pred"], "SpatialPixelsDataFrame"), SubCatchments.shp, fn = mean)

        if (  class(predictors) == "SpatialGridDataFrame" ) {
           tmp.block <- overlay( as(x.idw["var1.pred"], "SpatialPixelsDataFrame"), SubCatchments.shp, fn = mean)
        } else tmp.block <-  overlay(x.idw["var1.pred"], SubCatchments.shp, fn = mean)
        
        #if (length(x.idw.block@data[["var1.pred"]]) == length( tmp.block[,1]) ) {
        #  x.idw.block@data[["var1.pred"]] <- tmp.block[,1]
        #} else stop("Error: The size of -at least- one Subcatchment is smaller than 'cellsize'")

        if ( nrow(x.idw.block@data) == length( tmp.block[,1]) ) {
          x.idw.block@data[["var1.pred"]] <- tmp.block[,1]
        }  else stop("Error: The size of -at least- one Subcatchment is smaller than 'cellsize'")

	      # min and max values for colors
        idw.blk.min  <- min(x.idw.block@data["var1.pred"], na.rm=TRUE)
        idw.blk.max  <- max(x.idw.block@data["var1.pred"], na.rm=TRUE)

       } # ELSE end

      #################################################
      # IF all the block interpolated values were EQUAL
      if (idw.blk.min == idw.blk.max) {
         idw.blk.min <- idw.blk.min - abs(idw.blk.min)/10
         idw.blk.max <- idw.blk.max + abs(idw.blk.max)/10
      } # ELSE if

      #################################################
      # Adding the block interpolations to the results
      if (type == "block") { result <- x.idw.block
      } else result <- list(Cells=result, Block=x.idw.block)


    } # IF end

 # if the user wants to plot the interpolations:
 if (plot) {

     ########################
     # 5) Legend Definition #
     ########################

     # 5.1) Polygon with the catchment, for being put over the interpolations
     if (!missing(subcatchments))
       catchment.l1 = list("sp.polygons", SubCatchments.shp, first=FALSE)

     # 5.2) Points with the stations used for computing the interpolations, for being put over the interpolations
     if ( !missing(p4s) ) {
       stations.l2 = list("sp.points", SpatialPoints(x.work, proj4string = p4s), col="black", first=FALSE)
     } else stations.l2 = list("sp.points", SpatialPoints(x.work), col="black", first=FALSE)

     # 5.3) Legend with the Minimum, Maximum and Mean interpolated values
     # For Ebro: stations.offset = c(450000, 4600000)
     if (stations.plot) {
       nstations.l3 <- list("sp.text", stations.offset, paste("Stations used:", nrow(x.work), sep=" "), cex=0.8 )
     } # IF end

     # 5.4) North Arrow
     if (arrow.plot) {
	    arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset =arrow.offset, scale = arrow.scale)
     } #IF end

     # 5.5) Scale Bar
     if (scalebar.plot) {

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

     } #IF end

     # 5.6) Final Layout (legend creation)
     if (!missing(subcatchments)) {

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

     } else  if (missing(subcatchments)) {

	   # Creating the layout that will be used for plotting the interpolations
	     if (stations.plot) {
               if ( arrow.plot & scalebar.plot ) { map.layout <- list(arrow, scale, scale.min, scale.max, stations.l2, nstations.l3)
               } else if ( arrow.plot & !scalebar.plot ) { map.layout <- list(arrow, catchment.l1, stations.l2, nstations.l3)
             } else if (!arrow.plot & scalebar.plot) { map.layout <- list(scale, scale.min, scale.max, stations.l2, nstations.l3)
                   } else {map.layout <- list(stations.l2, nstations.l3) }
	     } else	 # if stations.plot==FALSE
            {
             if ( arrow.plot & scalebar.plot ) { map.layout <- list(arrow, scale, scale.min, scale.max)
             } else if ( arrow.plot & !scalebar.plot ) { map.layout <- list(arrow)
               } else if (!arrow.plot & scalebar.plot) { map.layout <- list(scale, scale.min, scale.max)
                 } else {map.layout <- NULL }
            } # END if (stations.plot)

      } # ELSE end

     #######################
     # 6) Color Definition #
     #######################

     Precipitation.cols <- colorRampPalette(c("aquamarine", "blue", "darkblue"))
     Temperature.cols   <- colorRampPalette(c("lightyellow", "yellow", "red", "darkred"))
     # Another way for temperature colors, using the reverse order (from white to red):
     # Temperature.cols <- rev(heat.colors(100))

     PCPAnomaly.cols    <- colorRampPalette(c("sienna4", "peachpuff", "royalblue", "blue"))
     PCPAnomaly2.cols   <- colorRampPalette(c("darkred", "red3", "orange", "yellow", "lightskyblue", "royalblue3", "darkblue"))

     TEMPAnomaly.cols   <- colorRampPalette(c("lightyellow", "yellow", "red", "darkred"))
     TEMPAnomaly2.cols  <- colorRampPalette(c("yellow4", "yellow", "orange", "red3", "darkred"))
     TEMPAnomaly3.cols  <- colorRampPalette(c("darkblue", "royalblue3", "lightskyblue", "yellow", "orange", "red3", "darkred"))


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

     # Specify at which interpolated values colours change
     if (class(col.at)=="numeric") {
	      at.idw       <- col.at
	      at.idw.block <- col.at
     } else
       { if (col.at == "auto") {

          if ( type %in% c("cells", "both") ) {

            if ( ( abs(idw.min) >= 0 ) & ( abs(idw.max) <= 1 ) ) {
              idw.min  <- sign(idw.min)*round( abs(idw.min), 3)
              idw.max  <- sign(idw.max)*round( abs(idw.max), 3)
            } else {                
                idw.min  <- floor(idw.min)
                idw.max  <- ceiling(idw.max)  
              } # ELSE end
              
            if (idw.max==idw.min) {              
              col.nintv <- 4
              if (idw.max !=0 ) {
                idw.min <- idw.min - abs(idw.min)/10
                idw.max <- idw.max + abs(idw.max)/10
              } else {
                  idw.min <- -0.1
                  idw.max <-  0.1
                }
            } # IF end
            
          } # IF end

          if ( type %in% c("block", "both") ) {
            if ( ( abs(idw.min) >= 0 ) & ( abs(idw.max) <= 1 ) ) {
              idw.blk.min  <- sign(idw.blk.min)*round( abs(idw.blk.min), 3)
              idw.blk.max  <- sign(idw.blk.max)*round( abs(idw.blk.max), 3)
            } else {
                idw.blk.min  <- floor(idw.blk.min)
                idw.blk.max  <- ceiling(idw.blk.max)
              } # ELSE end
            if (idw.blk.max==idw.blk.min) {
              col.nintv <- 4
              if (idw.blk.max!=0) {
                idw.blk.min <- idw.blk.min - abs(idw.blk.min)/10
                idw.blk.max <- idw.blk.max + abs(idw.blk.max)/10
              } else {
                  idw.blk.min <- -0.1
                  idw.blk.max <- 0.1                  
                }
            } # IF end
          } # IF end
          
	  if ( type %in% c("cells", "both") ) at.idw       <- seq(idw.min, idw.max, length.out=col.nintv)
	  if ( type %in% c("block", "both") ) at.idw.block <- seq(idw.blk.min, idw.blk.max, length.out=col.nintv)	   

        } else # col.at == "R"
    		    { message("[I'm sorry, but 'col.at' = 'R' is not implemented yet. Using 'col.at' = 'auto']")
    		      #at.idw      <- NULL
    		      #at.idw.block<- NULL
    		      if ( type %in% c("cells", "both") ) { at.idw       <- seq(idw.min, idw.max,length.out=col.nintv) }
    		      if ( type %in% c("block", "both") ) { at.idw.block <- seq(idw.blk.min, idw.blk.max, length.out=col.nintv) }
    		    }
       } # IF end

 } # IF end


 ###################################
 # 7) Plotting the Interpolations  #
 ###################################

 # if the user wants to plot the interpolations:
 if (plot) {

    if ( type %in% c("cells", "both") ) {
        # Plotting the interpolated Precipitation values with the scale bar, north arrow, stations, and catchment borders
        a <- sp::spplot(x.idw["var1.pred"], sp.layout = map.layout,
                    scales=list(draw=TRUE, y=list(rot=90), abbreviate=FALSE),
                    main=main, col.regions= ColorRamp(col.nintv),
                    at = at.idw )

	      dev.set(2)
        ifelse( type == "both", print(a, split=c(1,1,2,1), more=TRUE), print(a) )

    } # IF end

    if ( type %in% c("block", "both") ) {

        a <- sp::spplot(x.idw.block["var1.pred"], sp.layout = map.layout,
                    scales=list(draw=TRUE, y=list(rot=90), abbreviate=FALSE),
                    main=main, col.regions= ColorRamp(col.nintv),
                    at= at.idw.block)

        dev.set(2)
        ifelse( type == "both", print(a, split=c(2,1,2,1)), print(a) )

    } # IF end

    # If the library 'automap' was used, the parameters used for the
    # interpolation with the 'autoKrige' function are shown
    if ( hasArg(formula) ) {

      if (!constant_field)  {

  	    # Creation of a new screen if needed
  	    if( length(dev.list())==1) dev.new()

  	    # Plotting the parameters of the the 'autoKrige' function
  	    if ( type %in% c("cells", "both") ) {
  	      dev.set(3)
  	      plot(x.autokrige.cells)
  	    } #IF end

  	#    # Creation of a new screen if needed
#  	    if ( type == "both") {
#  	       if (length(dev.list()) == 2) dev.new()  }
#
#  	    # Plotting the parameters of the the 'autoKrige' function
#  	    if ( type %in% c("block", "both") ) {
#  	      ifelse( type == "block", dev.set(3), dev.set(4))
#  	      #plot(x.autokrige.block)  # when using 'autoKrige' function
#          plot(x.idw.block)
#        } # IF end


      } # IF end

    } # IF end
 } # IF end

 return(result)

} # 'hydrokrige.default' END



################################################################################
#                      hydrokrige.data.frame                                   #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: April 22-28th,                                                      #
# Updates: Oct 2009, Dec 2010, Apr 2010                                        #
#          29-May-2013 ; 03-Jun-2013                                           #
################################################################################
# BLOCK IDW interpolation (with optional plot) over a set of subcatchments,    #
# defined by a polygonal shapefile, and during a Time Window defined by the    #
# user                                                                         #
# The interpolated values for each subcatchment is computed as the mean value  #
# over all the cells that belong to each subcatchment                          #
################################################################################

# 'x.ts' : Character with the filename (with path) of the cvs file with the time series for all the stations.
#              1) CAN be a database with all the time series in the Basin.
#              2) AT LEAST, HAVE to contain the time series that will be used for the interpolations
#              The structure of this file is the following:
#              -)1st column: the first value HAS TO BE 'Date' (the header of the column) ,
#                            and all the other rows have to contain the dates in which the stations have values
#              -)2nd...Nth column: the first value has to be a string with
#                                  the name of the station -starting with a letter!!-
#                                  (the header of the column), and all the other
#                                  rows have to contain the measured values by the station

# 'x.gis': Character with the filename (with path)  of the cvs file with the GIS information for all the stations.
#               1) CAN to contain the spatial location of ALL the stations in the Basin.
#               2) AT LEAST, HAVE to contain the location of those stations that will be used for the interpolations
#                  The MINIMUM fields that Have to be present in this file, and their corresponding colum index are the following:
#               4: 'XPR'     == X
#               5: 'YPR'     == Y
#               6: 'ELEVATION'
#               8: 'BASIN_NAME' : basin name, starting with a letter
#              10: 'NAME'       : station name, starting with a letter
# 'X'        : character, field name in 'x.gis' that stores the EAST coordinate of the stations
# 'Y'        : character, field name in 'x.gis' that stores the NORTH coordinate of the stations
# 'sname'    : character, field name in 'x.gis' that stores the name of the stations (have to start by a letter)
# 'bname'    : character, field name in 'x.gis' that stores the name of the subcatchment in 'x.gis' that will be analysed
#              Only necessary when 'catchment.name' is not "all"

# 'catchment.name': name of the catchment that will be analized. Posble values are:
#                   -)"all"       : ALL the stations in the 'gis.fname' file will be used for
#                                   extracting the corresponding time series from the 'ts.fname' file
#                   -)other string: ONLY those stations in 'gis.fname' with a 'BASIN_NAME' field
#                                   value == 'catchment.name' will be used for
#                                   extracting the corresponding time series from 'ts.fname'

# 'subcatchments'      : filename (with path) of the shapefile with all the
#                        Subcatchments within the Catchment. It has to be of 'polygon' type
# 'elevation'          : OPTIONAL. character, field name in the shapefile 'subcatchments' that stores the elevation of the stations (m.a.s.l.)

# 'IDvar'              : (from ?readShapePoly) a character string with the name of a
#                        column in the 'subcatchments' shapefile DBF containing the ID values
#                        of the shapes - the values will be converted to a character vector

# 'p4s'                : Data about the projection of the GIS files, usually created
#                        by the CRS function of the 'sp' package

# 'cells.size'         : Size of the cells to be used in the regular interoplation grid, [m2]
# 'grid.type'          : Character, indicating the yype of grid to be computed
#                        over the area defined by 'subcatchments'
#                        Valid values are:
#                        -) 'regular'    : for regular (systematically aligned) sampling; Dedault option
#                        -) 'random'     : for completely spatial random;
#                        -) 'stratified' : for stratified random (one single random
#                                          location in each "cell"); '"nonaligned"' for
#                                          nonaligned systematic sampling (nx random
#                                          y coordinates, ny random x coordinates);
#                        -) 'hexagonal'  : for sampling on a hexagonal lattice;
#                        -) 'clustered'  : for clustered sampling

# 'nmax'    : For local kriging: the number of nearest observations that should be used for a
#              kriging prediction, where nearest is defined in terms of the space
#              of the spatial locations. By default, all observations are used
#              OPTIONAL
# 'nmin'    : For local kriging: if the number of nearest observations within distance maxdist
#              is less than 'nmin', a missing value will be generated; see 'maxdist'.
#              By default 'nmin' =1
# 'Max.Dist' : For local kriging: only observations within a distance of Max.Dist from the prediction
#              location are used for prediction or simulation; if combined with 'nmax',
#              both criteria apply. By default, all observations are used
#              OPTIONAL

# 'maxdist' : Maximum distance in which of nearer neighbours that have to be used for interpolating (with IDW, OK, KED)

# 'ColorRamp' : string representing the type of variable being analysed ("Precipitation", "Temperature" or "Flow")

# 'from'     : Starting Date that will define the time window in which all the data will be analysed
# 'to'       : Ending Date that will define the time window in which all the data will be analysed
# 'date.fmt' : Date format of "from" and "to". Thedefault is "%Y-%m-%d"

# 'plot'  : Boolean, indicating if the interpolated values have to
#              be ploted or not
# 'col.nintv': integer, number of colors that have to be used for plotting the interpolated values
# 'col.at'   : to specify at which interpolated values colours change. Valid values are:
#              -) "R"   : uses the default setting of spplot
#              -) "auto": default option. => at = seq(min, max,length.out=col.nintv)
#                         min  <- floor( min(idw["var1.pred"]@data, na.rm=TRUE ) )
#                         max  <- ceiling( max(idw["var1.pred"]@data, na.rm=TRUE) )
#              -) numeric: vector of reals giving the exact values in which the colors have to change
#                          Useful when the user desires the same color for the same value when
#                          comparing to maps with different range of values
# 'main': string with the title to be used for the plot

# 'stations.plot': Boolean, indicating if the gauging stations, defined by 'gis.fname' have to be plotted
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
# from        : starting date for detection of days with inormation
# to          : date format that will be used in the output variable
# date.fmt    : date format of "from" and "to". For CHE files, the format must be "%d-%m-%Y"
# 'dates'     : "numeric", indicating the column index in 'x.ts' that stores the dates
# write2disk  : Logical. Indicates if we want to write the output into a CSV file, default=TRUE
# fname       : OPTIONAL. Character with the filename of the output file. Only needed if 'write2disk'=TRUE

hydrokrige.data.frame <- function( x.ts, x.gis,
			           X="x", Y="y", sname, bname,
			           elevation,
			           predictors,
			           catchment.name="all",
			           type="block",
			           formula,
			           subcatchments,
				   IDvar=NULL,
				   p4s=CRS(as.character(NA)),
			           cell.size=1000, grid.type="regular",
			           nmin=0, nmax = Inf, maxdist = Inf,
			           ColorRamp="PCPAnomaly",
			           plot=FALSE, col.nintv=10, col.at="auto",
			           main,
			           stations.plot=FALSE, stations.offset,
			           arrow.plot=FALSE, arrow.offset, arrow.scale,
			           scalebar.plot=FALSE, sb.offset, sb.scale,
			           verbose=TRUE,
                                   allNA.action="error",
			           dates=1, from, to,
			           write2disk=TRUE,
				   out.fmt="csv2",
			           fname=paste(ColorRamp, "by_Subcatch.csv", sep=""),...) {

  # If the user didn't provide a value for 'p4s' and used the defaul one
  if ( missing(p4s) ) {
    p4s.exists <- FALSE
  } else p4s.exists <- TRUE

  # Checking the class of 'x.gis'
  if (class(x.gis) != "data.frame" )
	 stop(paste("Invalid argument: class(x.gis) must be 'data.fame'", sep=""))

  # Checking 'out.fmt'
  if (is.na(match(out.fmt, c("csv", "csv2") ) ) )
	 stop("Invalid argument: 'out.fmt' must be of in c('csv', 'csv2'")

  # Checking that the file 'subcatchments' really exists
  if (class(subcatchments) == "character") {
    if (!file.exists(subcatchments) )
       stop(paste("Invalid argument: the file '", basename(subcatchments), "' doesn't exist", sep="") )
  } # IF end

  # Checking that 'grid.type'
  if (is.na(match(grid.type, c("regular", "random", "stratified", "hexagonal", "clustered") ) ) )
	 stop("Invalid argument: 'grid.type' must be of in c('regular', 'random', 'stratified', 'hexagonal', 'clustered'")

  # Printing the defaul 'cell.size' value when the user didn't provide it
  if (missing(cell.size)) { message(paste("[Missing 'cell.size': using 'cell.size= ", cell.size, " [m]. ]", sep="") ) }

  # Checking that 'nmin' is integer
  if ( !missing(nmin) )
    if ( (trunc(nmin) - nmin) != 0 )
     stop("Invalid argument: 'nmin' must be integer")

  # Checking that 'nmax' is integer
  if ( !missing(nmax) )
    if ( nmax != Inf)
      if ( (trunc(nmax) - nmax) != 0 ) {
        stop("Invalid argument: 'nmax' must be integer") }

  # Checking that the user provied a valid argument for 'ColorRamp'
  if (is.na(match(ColorRamp, c("Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2", "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3") ) ) )
      stop("Invalid argument: 'ColorRamp' must be in c('Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')")

  # Checking that 'type'
  if (is.na(match(type, c("block") ) ) )
	message("[Warning: when 'x.ts' is a data.frame, 'type' MUST BE 'block'. It has been changed to 'block']" )

  # Checking that 'col.nintv' is integer
  if ( !missing(col.nintv) )
    if ( (trunc(col.nintv) - col.nintv) != 0 )
       stop("Invalid argument: 'col.nintv' must be integer")

  # Checking that the user provied a valid argument for 'col.at'
  if (class(col.at) != "numeric" ) {
	if ( is.na(match(col.at, c("R", "auto") ) ) ) {
		stop("Invalid argument: 'col.at' must be in c('R', 'auto') or be a numeric vector")
	} # IF end
  } # IF end

  # If the user wants a plot of the interpolated values and did not provide a title, this is created automatically
  if (plot & missing(main)) {
       if (!hasArg(formula) ) {
         main <- paste("Interpolated", ColorRamp, ". Nmax=", nmax, sep=" ")
       } else if (hasArg(formula) ) {
         main <- paste("Kriging", ColorRamp, ". ", formula, ". Nmax=", nmax, sep=" ")
       }
  } # IF  end

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

  # Checking that the user provied a valid argument for 'dates'
  # If 'dates' is a single number, it indicates the index of the column of 'x' that stores the dates
  if ( (class(dates) == "numeric") & (length(dates)==1 ) ) {
     dates.col  <- dates
     dates      <- x.ts[, dates.col]
  }  else { # When class('dates') is not numeric
       # Adding, as first column, the dates given by the user
       x.ts      <- cbind(Date=dates, x.ts)
       dates.col <- 1
     } # ELSE end

  # Checking the validity of the 'from' argument
  if (missing(from)) { 
     from     <- dates[1]
     from.pos <- 1
  } else if ( length( which(dates == from) ) > 0 ) {
      from.pos <- which( dates == from )
    } else stop("Invalid argument: 'from' is not in 'dates' ")

  # Checking the validity of the 'to' argument
   if (missing(to)) { 
      to.pos <- length(dates)  
      to     <- dates[to.pos]
  } else if ( length( which(dates == to) ) > 0 ) {
      to.pos <- which( dates == to )
    } else stop("Invalid argument: 'to' is not in 'dates' ")

  # Checking that 'to' is larger than 'from'
  if (to.pos < from.pos) stop("Invalid argument: 'to' have to be located in a row below the row corresponding to 'from'")


  #####################################################
  # 2)           Reading the ts Files                 #
  #####################################################

  # Subsetting x.ts for ingluding ONLY those data within the desired period
  x.ts  <- x.ts[from.pos:to.pos, ]

  # Re-assigning the value of 'dates'
  dates  <- x.ts[, dates.col]


  ######################################################################
  # 3) Reading the shapefile with the Subcatchments
  ######################################################################

  # 1) If the user provided a value for 'subcatchments'
  if (!missing(subcatchments)) {

     # If the user provided the name of the shapefile
     if (class(subcatchments) == "character") {

       # Reading the SubCATCHMENTS of the CATCHMENT
       if (require(maptools)) { #it is necessary for usign the function "readShapePoly"

         if (verbose) message(paste("[reading GIS Subcatchments in: '", basename(subcatchments), "'...]", sep="") )

         # Reading the Shapefile with the subcatchments
         SubCatchments.shp <- maptools::readShapePoly(subcatchments, proj4string=p4s, IDvar= IDvar)
       
       } else stop( paste("Missing package: You need 'maptools' for reading the '", basename(subcatchments), "' shapefile", sep="") )

     } else  { #  # If the user provided 'subcatchments' already as an 'SpatialPolygonsDataFrame' object

	# Checking that 'IDvar' exists within 'subcatchments'
	if ( ! is.null(IDvar) ) {
          if ( !is.na(match(type, c("block", "both") ) ) & is.na( match( IDvar, colnames(subcatchments@data) ) ) )
             stop("Invalid argument: 'IDvar' does not exists in 'subcatchments'")
	} # IF end

        SubCatchments.shp <-  subcatchments

       } # ELSE end

    # Getting the ID of each subcatchment
    subcatch.IDs <- c(rownames(SubCatchments.shp@data))
    
    if (verbose) message("[ Subb IDs: ]")
    if (verbose) message( paste(subcatch.IDs, collate=" ") )

    # Number of Subcatchmnets
    nSub <- nrow(SubCatchments.shp@data)

    if (verbose) message(paste("[Subcatchments found:", nSub, "]", sep=" ") )

  } # IF end


 #######################################################################################################
 # 4) Verifying the compatibility amonth the projections of 'predictors, 'p4s' and 'SubCatchments.shp' #
 #######################################################################################################

 # 4.1) Verifying the compatibility between 'p4s' with 'predictors' and ''SubCatchments.shp'', when they are present

 #require(sp) # for 'proj4string'

 # If the user provided 'p4s'
 if ( p4s.exists ) {

    # 4.1.2) If the user provided 'subcatchments',
    if ( !missing(subcatchments) ) {

        if ( !identical( CRS(sp::proj4string(SubCatchments.shp)), p4s ) )  {

	    if (verbose) message(paste("[Warning: 'p4s' and 'subcatchments' have different CRS. The projection of the shapefile was changed to the one given by 'p4s': '", p4s@projargs, "']", sep="") )
            proj4string(SubCatchments.shp) <- p4s
        } # IF end

    } # IF end

    # 4.2) When 'p4s' doesn't exist
 } else {

         # 4.2.1) If 'predictors' and 'subcatchments' are both present.
         if ( !missing(subcatchments) & !missing(predictors) ) {

	    # Verifying the compatibility between  'predictors' and 'subcatchments'
            if ( !identical( proj4string(SubCatchments.shp), proj4string(predictors) ) )  {

		if (verbose) message(paste("[Warning: 'subcatchments' and 'predictors' has different CRS. The projection of the shapefile was changed to the one given by 'predicotrs': '", proj4string(predictors), "']", sep="") )
                proj4string(SubCatchments.shp) <- CRS(proj4string(predictors))
	             	p4s                            <- CRS(proj4string(predictors))

            } # IF end

	  # IF ONLY 'predictors' OR 'subcatchments', was given as argument
        } else {

            # 4.2.2) Assingning the projection of 'SubCatchments.shp' to 'p4s', when 'SubCatchments.shp' was given
            if ( !missing(subcatchments) ) {
                 if ( !is.na(proj4string(SubCatchments.shp) ) )  {

                    if (verbose) message(paste("[Warning: 'You didn't specified a  projection ('p4s') for 'x.gis'. It was set to the one of 'subcatchments': '", proj4string(SubCatchments.shp), "']", sep="") )
                    p4s <- CRS(proj4string(SubCatchments.shp))

                } # IF end

            } # ELSE end

	    # 4.2.3) Assingning the projection of 'predictors' to 'p4s', when 'predictors' was given
            if ( !missing(predictors) ) {
                 if ( !is.na(proj4string(predictors) ) )  {

                    if (verbose) message(paste("[Warning: 'You didn't specified a  projection ('p4s') for 'x.gis'. It was set to the one of 'predictors': '", proj4string(SubCatchments.shp), "']", sep="")  )
                    p4s <- CRS(proj4string(predictors))

                } # IF end

            } # ELSE end

          } #ELSE end

    } # ELSE end


     #######################################################################
     # 5) Creating an interpolation grid, if 'predictors, was not provided #
     #######################################################################

     # If the user didn't provide a grid with predictors,
     # the predictor grid is sampled from the Shapefile with the subcatchments
     if ( missing(predictors) ) {

      	if (verbose) message("['predictors' was not provided. Computing the grid...]" )
      	# Defining a sampling GRID. If grid.type="regular', then the grid is made
      	# of squared cells of 'cell.size'm x 'cell.size'm with regular spacing
      	# For avoiding a random grid (sampled randomly from the first cell), and
      	# getting a fixed, reproducable grid, it is neccessary to add the argument "offset = c(0.5, 0.5)"
      	predictors <- spsample(SubCatchments.shp, type=grid.type, cellsize=cell.size, offset = c(0.5, 0.5))
      
      	# Making possible that the grid can be used in the interpolations:
      	# and transforming from 'SpatialPoints' to 'SpatialPixels' the class of 'predictors'
      	gridded(predictors) <- TRUE

     }  # IF end

     ##############################################################################
     # 6) Checking the compatibility of projection between 'predictors' and 'p4s' #
     ##############################################################################

     # If predictors (already given by the user or just computed before) has a projection
     if ( (p4s.exists) & !is.na(proj4string(predictors) ) ) {

	 if ( !identical( CRS(proj4string(predictors)), p4s ) )  {

	    if (verbose) message(paste("[Warning: 'p4s' and 'predictors' have different CRS. The projection of 'predictors' was changed to the one given by 'p4s': '", p4s@projargs, "']", sep="") )
	    proj4string(predictors) <- p4s

	 } # ELSE end

     } #IF end

   ################################

     # Extract the first letter of the variable to be interpolated ('T', 'P' or 'Q'), to add it to he field names
     var.stg <- toupper(substr(ColorRamp,1,1))

     # Creating a vector with the names of the field that will be used for storing the results
     #field.names <- c("Date", paste(var.stg, "Sub", subcatch.IDs, sep="") ) # needed when the field 'IDvar' doesn't have the word "Sub" on it
     field.names <- c("Date", paste(var.stg, subcatch.IDs, sep="") )


     ######################
     # 7) Date Selection  #
     ######################

     ndates <- nrow(x.ts)

     # Creating the data.frame that will store the computed averages for each subcatchment
     #a           <- as.data.frame(matrix(data = NA, nrow = ndates, ncol = 1 + nSub, byrow = TRUE, dimnames = NULL))
     a           <- matrix(data = NA, nrow = ndates, ncol = 1 + nSub, byrow = TRUE, dimnames = NULL)
     #colnames(a) <- field.names

     #a$Date <- dates

     ##########################
     # 8) Interpolation loop  #
     ##########################

     for (d in 1:ndates )   {
     
         message("                                 ")
         message("[ Date: ", dates[d], " ]")

	 # Selecting the row in 'x.ts.catch' corresponding to the desired date among all the dates
	 ts.row.index <- which( x.ts[, dates.col] == dates[d] )

	 # Selecting the time series corresponding to the current day
	 x.ts.day <- x.ts[ts.row.index, ]
	 names(x.ts.day) <- colnames(x.ts[ts.row.index, ])

	 x.idw <- hydrokrige.default(x.ts.day, x.gis, X=X, Y=Y,
				 sname=sname, bname=bname, elevation=elevation,
				 predictors,
				 catchment.name= catchment.name,
				 type= "block",
				 formula,
				 subcatchments= SubCatchments.shp,
				 IDvar= IDvar, p4s= p4s,
				 cell.size= cell.size,
				 grid.type= grid.type,
				 nmin= nmin, nmax= nmax, maxdist= maxdist,
				 ColorRamp= ColorRamp,
				 plot= plot,
				 col.nintv= col.nintv, col.at= col.at,
				 main= paste(dates[d], ":", main, sep=" "),
				 stations.plot=stations.plot, stations.offset=stations.offset,
				 arrow.plot= arrow.plot, arrow.offset= arrow.offset, arrow.scale= arrow.scale,
				 scalebar.plot= scalebar.plot, sb.offset= sb.offset, sb.scale= sb.scale,
				 verbose=verbose, allNA.action=allNA.action)

	 if (verbose) message( "[ Finished Date : ", dates[d], "  :  ", d, "/", ndates, "      =>      ", round(100*d/ndates,2), "% ]" )

	 a[ts.row.index, 2:(nSub+1)] <- round(x.idw@data$var1.pred, 3)


    } # FOR end

    # Transforming the resulting matrix into a data.frame.
    # Working with a matrix and after the loop transforming it into a data.frame i
    # it is much faster that working within the loop with a data frame.
    a           <- as.data.frame(a)
    colnames(a) <- field.names
    a$Date      <- dates

    ##########################
    # 7) Writting Results    #
    ##########################

    # If the user wants to write the outputs into a CSV file
    if (write2disk) {

       subcatch.row.index <- pmatch( rownames(x.idw@data), subcatch.IDs )

       # Writing the Coordinates of the baricenter of each subcatchment
       #b <- cbind(rownames(x.idw@data), x.idw@data[,1:2], paste(var.stg,"Sub",subcatch.IDs, sep="")  )
       b <- cbind(rownames(x.idw@data), coordinates(x.idw), paste(var.stg, subcatch.IDs, sep="")  )
       colnames(b) <- c("Subcatchment_ID", X, Y, sname)

       if (out.fmt == "csv") {

         # Writing the interpolated values for each subcatchment
         write.csv(a, file=fname, row.names=FALSE)

         # Writting the spatial location of each catchment along with its corresponding ID
         write.csv(b, file=paste("GIS-", fname, sep=""), row.names=FALSE)

       } else if (out.fmt == "csv2") {

         # Writing the interpolated values for each subcatchment
         write.csv2(a, file=fname, row.names=FALSE)

         # Writting the spatial location of each catchment along with its corresponding ID
         write.csv2(b, file=paste("GIS-", fname, sep=""), row.names=FALSE)
       } # ELSE end

    } # IF end

    return(a)

} # 'hydrokrige.data.frame' END
