# File gists2spt.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

##############################################################
# 'gists2spt': Time Series and GIS to Spatio-temporal Object #
#                   Sep 12th, 2009                           #
##############################################################
# Given a data.frame (x.gis) with the spatial coordinates of a set of
# gauging stations, and a set of measurements in that stations (x.ts)
# this function adds the measurements in 'x.ts' to the corresponding
# stations in 'x.gis', even if they are not in the same order.
# If 'X' and 'Y are given, the returning object will be a 'SpatialPointsDataFrame'
# with coordinates given by the 'X' and 'Y' fields
# If 'p4s' is given, the returning objectwill be projected according
# to the specification provided by 'p4s'

# 'x.ts' : vector or data.frame with the numeric values in each station of 'x.gis', for a particular given time
# 'x.gis': data,frame with the GIS information for all the stations.
#          1) CAN contain the spatial location of ALL the stations in the Basin.
#          2) AT LEAST, HAVE to contain the location of those stations in 'x.ts' that will be used for the further analysis
#             The MINIMUM fields that Have to be present in this file, and their corresponding colum index are the following:  
#             'x', 'y', 'sname', 'bname', 'elevation', which are described below.
# 'X'        : character, field name in 'x.gis' that stores the EAST coordinate of the stations in 'x.gis'. 
#              The expected name is 'x', but if the value provided by the user is different, a new 'x' field is created and is used as the esting coordinate of 'x.gis'
# 'Y'        : character, field name in 'x.gis' that stores the NORTH coordinate of the stations in 'x.gis'.
#              The expected name is 'y', but if the value provided by the user is different, a new 'y' field is created and is used as the northing coordinate of 'x.gis'
# 'elevation': character, field name in 'x.gis' that stores the elevations (m.a.s.l.) of the stations in 'x.ts'
# 'sname'    : character, field name in 'x.gis' that stores the UNIQUE name or ID of the stations in 'x.gis' (have to start by a letter)
# 'bname'    : character, field name in 'x.gis' that stores the name of the subcatchment in 'x.gis' that will be analysed
#              Only necessary when 'catchment.name' is not "all"

# 'catchment.name': name of the catchment that will be analized. Posble values are:
#                   -)"all"       : ALL the stations in the 'gis.fname' file will be used for 
#                                   extracting the corresponding time series from the 'ts.fname' file
#                   -)other string: ONLY those stations in 'x.gis' with a 'bname' field 
#                                   value == 'catchment.name' will be used 
# 'na.rm'  : a logical value indicating whether 'NA' values should be 
#            stripped before delivering the resulting object.
# 'p4s'    : Data about the projection of the GIS files, usually created 
#            by the CRS function of the 'sp' package
gists2spt <- function(x.gis, x.ts, sname, bname, X="x", Y="y", elevation,
                      catchment.name="all", na.rm=TRUE, p4s) {

   #####################################################################
   # 0) Checking the arguments
   #####################################################################

   # Checking that the 'sname' argument was provided
   if (missing(sname) ) {
     stop(paste("Missing argument: The argument 'sname' must be provided", sep="") )
   } else  # Checking that the field 'sname' exists in 'x.gis'
	   if ( !(sname %in% colnames(x.gis)) ) {
	     stop(paste("Invalid argument: The field '", sname, "' doesn't exist in 'x.gis'", sep="") )
	   } # IF end

   # Checking the class of 'x.ts'
    if (is.na(match(class(x.ts), c("data.frame", "numeric", "integer") ) ) )
     stop( "Invalid argument: class(x.ts) must be in c('data.frame', 'numeric', 'integer')" )

	# If 'x.ts' is a data.frame with only 1 row, it is converted into numeric,
        # for being sure that some functions refered to names of 'x.ts' will work
	if ( (class(x.ts) == "data.frame") )  {
	  snames <- colnames(x.ts)
	  x.ts   <- as.numeric(x.ts)
	  names(x.ts) <- snames
	} # IF end

   # Checking the class of 'x.gis'
   if (class(x.gis) != "data.frame" )
     stop( "Invalid argument: class(x.gis) have to be 'data.frame' " )

   # If the user provided 'X', it checks that the field 'x' exists in 'x.gis'
   if (!missing(X)) {
     if ( !(X %in% colnames(x.gis)) )
     stop(paste("Invalid argument: The field '", X, "' doesn't exist in 'x.gis'", sep="") )
   } # IF end

   # If the user provided 'Y', checks that the field 'y' exists in 'x.gis'
   if (!missing(Y)) {
     if ( !(Y %in% colnames(x.gis)) )
     stop(paste("Invalid argument: The field '", Y, "' doesn't exist in 'x.gis'", sep="") )
   } # IF end

   # Checking that the field 'elevation' exists in 'x.gis'
   if (!missing(elevation)) {
     if ( !(elevation %in% colnames(x.gis)) )
       stop(paste("Invalid argument: The field '", elevation, "' doesn't exist in 'x.gis'", sep="") )
   } # IF end

   # Checking that the field 'bname' exists in 'x.gis', when a subset of the catchment will be used
   if (catchment.name != "all") {
     # Checking that the field 'bname' exists in 'x.gis'
     if ( !(bname %in% colnames(x.gis)) )
       stop(paste("Invalid argument: The field '", bname, "' doesn't exist in 'x.gis'", sep="") )
   } # IF end

   #####################################################################
   # 1) Preparing the data
   # The following columns are transformed from factor into characters, for
   # avoiding that during the main loop, the ID's of the values will be used
   # instead of the corresponding values
   x.gis[, sname] <- as.character(x.gis[, sname])

   if (catchment.name != "all")
     x.gis[, bname] <- as.character(x.gis[, bname])

   #####################################################################
   # 2) Selection of the stations in 'x.gis' that belongs to 'catchment.name'
   if (catchment.name != "all") {

       # Selecting only those stations of 'x.gis' that are within the,
       # selected catchment ( bname == 'catchment.name')
       x.gis <- x.gis[ x.gis[, bname] == catchment.name, ]

   } # iF ELSE end

   #####################################################################
   # 3) Matching the station in 'x.ts' with those in 'x.gis'

   # ID's of all the stations in 'x.ts'
   #snames <- colnames(x.ts)
   snames <- names(x.ts)

   # Computing the row index of those stations of 'x.gis' that are present
   # in 'x.ts'
   gis.row.index <- which(x.gis[, sname] %in% snames)

   if ( length(gis.row.index)==0 ) stop("There are not stations in 'x.gis' with corresponding names in 'x.ts'")

   # Creating the spatio-temporal object, initially is a copy of all the
   # stations in 'x.gis' that will be used for the analysis
   x.spt <- x.gis[gis.row.index, ]

   # Getting the column index in 'x.ts' that stores the measured values
   # for each station in 'x.gis'. The row position in 'x.gis' can be different
   # to the column position in 'x.ts'
   ts.col.index <- pmatch( x.spt[, sname], names(x.ts) )

   # Assigning the numeric values specified in 'x.ts' to each station in 'x.spt'
   x.spt <- transform(x.spt, value= as.numeric( x.ts[ts.col.index]) )

   #####################################################################
   # 4) Removing NA values in 'x.spt'
   if (na.rm==TRUE) {
     na.row.index <- which(is.na(x.spt[, "value"]))
     # If 'x.spt' has some NA's they are removed, in oter case, nothing happens
     if (length(na.row.index) > 0) {
         x.spt <- x.spt[-na.row.index, ]
     }
   } # IF end

   # IF all the stations have a NA value:
   if (nrow(x.spt) == 0)
     stop(paste("Invalid values: All the stations in 'x.gis' have a 'NA' value", sep="") )

   #####################################################################

   # 5) Creating an 'SpatialPointsDataFrame' object
   if (!missing(X) & !missing(Y) ) {

     # If 'x.gis' didn't have a field called 'x', it is created from the field
     # 'x' especified by the user
     if ( !("x" %in% colnames(x.spt) ) ) {  x.spt[,"x"] <- x.spt[,X] }

     # If 'x.gis' didn't have a filed called 'y', it is created from the field
     # 'y' especified by the user
     if ( !("y" %in% colnames(x.spt) ) ) { x.spt[,"y"] <- x.spt[,Y] }

     # Settting the COORDINATES of 'x.spt'
     #sp::coordinates(x.spt) <- ~x + y. # This assignement is not supported in R < 2.13.X
     coordinates(x.spt) <- ~x + y

     # Projecting the coordinates of 'x.spt'
     #if (!missing(p4s)) sp::proj4string(x.spt) = p4s # This assignement is not supported in R < 2.13.X
     if (!missing(p4s)) proj4string(x.spt) <- p4s

   } # IF end

   #####################################################################
   return(x.spt)

} # 'gists2spt' END
