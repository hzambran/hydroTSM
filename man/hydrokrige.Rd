%% File hydrokrige.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2015 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hydrokrige}
\Rdversion{1.1}
\alias{hydrokrige}
\alias{hydrokrige.default}
\alias{hydrokrige.data.frame}
\title{
Krige for Hydrological Time Series
}
\description{
Automatic interpolation for hydrological ts, with optional plot (wrapper to some functions of the \pkg{gstat} and \pkg{automap} packages) \cr

Originally it was thought as a way to make easier the computation of average precipitation over subcatchments (given as input in a shapefile map), based on values measured at several gauging stations, but nowadays it can also be used for interpolating any variable over a grid given by a raster map.\cr

Available algorithms: inverse distance weighted (\acronym{IDW}), ordinary kriging (\acronym{OK}) and kriging with external drift (\acronym{KED})\cr 

The (Block) Inverse Distance Weighted (\acronym{IDW}) interpolation is a wrapper to the \env{idw} function of the \pkg{gstat} package (so, it requires the \pkg{gstat} package). \cr

The automatic kriging (\acronym{OK} or \acronym{KED}) is a wrapper to the \code{\link[automap]{autoKrige}} function of the \pkg{automap} package (so, it requires the \pkg{automap} and \pkg{gstat} packages), which automatically selects the best variogram model from four different ones: spherical, exponential, gaussian and Matern with M. Stein's parameterization (for more details, see \code{\link[automap]{autoKrige}})
}
\usage{
hydrokrige(x.ts, x.gis, ...)

\method{hydrokrige}{default}(x.ts, x.gis, X= "x", Y= "y", sname, bname, 
        elevation, predictors, catchment.name = "all", type="cells", 
        formula, subcatchments, IDvar = NULL, 
        p4s=CRS(as.character(NA)), cell.size = 1000, 
        grid.type = "regular", nmin = 0, nmax = Inf, maxdist = Inf, 
        ColorRamp = "PCPAnomaly", plot = TRUE, col.nintv = 10, 
        col.at = "auto", main, stations.plot = FALSE, stations.offset,
        arrow.plot = FALSE, arrow.offset, arrow.scale, 
        scalebar.plot = FALSE, sb.offset, sb.scale, verbose = TRUE,
        allNA.action="error", ...)

\method{hydrokrige}{data.frame}(x.ts, x.gis, X= "x", Y= "y", sname, bname, 
        elevation, predictors, catchment.name = "all", type = "block", 
        formula, subcatchments, IDvar= NULL, 
        p4s=CRS(as.character(NA)), cell.size = 1000, 
        grid.type = "regular", nmin = 0, nmax = Inf, maxdist = Inf, 
        ColorRamp = "PCPAnomaly", plot = FALSE, col.nintv = 10,
        col.at = "auto", main, stations.plot = FALSE, stations.offset,
        arrow.plot = FALSE, arrow.offset, arrow.scale, 
        scalebar.plot = FALSE, sb.offset, sb.scale, 
        verbose = TRUE, allNA.action="error",
        dates=1, from, to, write2disk = TRUE, 
        out.fmt= "csv2",
        fname = paste(ColorRamp, "by_Subcatch.csv", sep = ""), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.ts}{
numeric or data.frame object, with measured values at several locations. \cr

-) \code{x.ts} CAN contain as many points as you want, e.g., all the gauging stations in the your database. \cr
2) \code{x.ts} HAVE to contain -at least- some points (e.g., gauging stations) that are also present in \code{x.gis} \cr

The names of each value in \code{x.ts} are used as the ID of each measurement point. When \code{x.ts} is a vector, this can be checked with \code{names(x)}, whereas when \code{x.ts} is a data.frame, it can be checked with \code{colnames(x.ts)}. The IDs of each measurement point have to be equal to the ID stored in the field \code{sname} of \code{x.gis}. \cr 

When \code{x.ts} is a data.frame, its structure have to be as follow: \cr
-) 1st column      : OPTIONAL. It MAY contain the dates, date-time or IDs that identify the measured values of each row. See \code{dates} argument. \cr
-) 2nd...Nth column: Measured values at each point (e.g., gauging station). The name of the columns is used as the ID of each station \bold{-starting with a letter!!-}.
}
  \item{x.gis}{
data.frame with the spatial information for each measurement point (e.g., gauging stations). \cr

-) \code{x.gis} MAY contain as many points as you want, e.g., all the stations in your database \cr
-) \code{x.gis} HAVE to contain -at least- the location of those stations that will be used for the interpolations \cr

The MINIMUM fields that have to be present in this file, and their corresponding column index are those described by: \code{X}, \code{Y}, \code{sname}. \cr

The ID of each measurement point, given in the field \code{sname}, has to be equal to the corresponding ID used in \code{x.ts}
}
  \item{X}{
character, field name in \code{x.gis} that stores the easting coordinate of each measurement point.
}
  \item{Y}{
character, field name in \code{x.gis} that stores the northing coordinate of each measurement point.
}
  \item{sname}{
character, field name in \code{x.gis} that stores the ID of each measurement point. \bold{the name of each measurement point HAS to start by a letter!!-}
}
  \item{bname}{
OPTIONAL. character, field name in \code{x.gis} that stores the name of the subcatchment in \code{x.gis} that will be analysed. \cr
ONLY necessary when \code{catchment.name} is different from \kbd{all}.
}
  \item{elevation}{
OPTIONAL. character, field name in \code{x.gis} that stores the elevation of the gauging stations (m a.s.l.).
}
  \item{predictors}{
OPTIONAL. \link[sp]{SpatialGridDataFrame-class} object, with prediction/simulation locations (it is needed for \acronym{KED}). \cr
Usually, a digital elevation model (DEM) read with the \code{\link[rgdal]{readGDAL}} function of the \pkg{rgdal} package. See the \env{newdata} argument in \code{\link[gstat]{krige}}. \cr
It should contain attribute columns with the independent variables (if present) and the coordinates with names as defined in \code{x.gis} \cr
If \code{predictors} is missing, the grid to be used as prediction/simulation locations is generated from sampling the polygon specified by the user in \code{subcatchments}, according to the arguments provided by \code{cells.size} and \code{grid.type}
}
  \item{catchment.name}{
name of the catchment that will be analysed. Possible values are: \cr
-)\kbd{all}       : ALL the stations in the \code{x.gis} will be used \cr
-)other string: ONLY those stations in \code{x.gis} with a \code{bname} field value equal to \code{catchment.name} will be used .
}
  \item{type}{
Character, indicating the type of interpolation required by the user. \cr
When \code{x.ts} is a data.frame, the ONLY possible value is \kbd{block}. For all the other cases, possible values are: \cr
-) \kbd{cells} : the interpolated values are computed for each cell \cr
-) \kbd{block} : the interpolated values are computed for each subcatchment, where the value for each subcatchment is computed as the mean value over all the cells that belong to each subcatchment \cr
-) \kbd{both} : \kbd{cells} and \kbd{block} are computed.
}
  \item{formula}{
OPTIONAL. Formula to be used in case of ordinary kriging or kriging with external drift. Requires the \pkg{automap} package. All the variables to be used within \code{formula} has to be present both in \code{x.gis} and \code{predictors}. See the \code{formula} argument in \code{\link[gstat]{krige}}. \cr
\code{formula} defines the dependent variable as a linear model of independent variables. \bold{Within this function, the dependent variable is always called \kbd{value}}, therefore, for ordinary and simple kriging use the formula \kbd{value~1}; for simple kriging also define \env{beta}; for universal kriging, suppose \env{value} is linearly dependent on \code{x} and \code{y}, use the formula \kbd{value~x+y}.
}
  \item{subcatchments}{
OPTIONAL. Only required when \code{predictors} is missing. \cr
Spatial polygon with all the subcatchments to be used as interpolation domain. The polygons are used to create the grid that will be used as prediction/simulation locations, from sampling it according to the arguments provided by \code{cells.size} and \code{grid.type}. Valid values are: \cr
1) \code{Character}, indicating the filename (with path) of the shapefile that contains all the subcatchments to be used as interpolation domain. It HAS TO BE of 'polygon' type \cr
2) \link[sp]{SpatialPolygonsDataFrame-class} resulting from reading the shapefile (e.g., with the command \code{\link[maptools]{readShapePoly}} of the \pkg{maptools} package)  that contains all the subcatchments to be used as interpolation domain.
}
  \item{IDvar}{
See \code{\link[maptools]{readShapePoly}}. a character string with the name of a field in the \code{subcatchments} shapefile containing the ID values used to identify each one of the subcatchments. When \code{type} is \kbd{block}, the values stored in this field will be used for labelling the computed values in each one of the subcatchments, therefore, if you don't provide this value, it could be difficult to identify which computed value corresponds to which subcatchment, because the ID is assigned by the \code{\link[maptools]{readShapePoly}} function.
}
  \item{p4s}{
OPTIONAL. a character with information about the projection of the GIS files, usually created by the \code{CRS} function of the \pkg{sp} package.
}
  \item{cell.size}{
OPTIONAL. Only required when \code{predictors} is missing. Size of the cells [m] to be used for sampling the polygons specified by the user in \code{subcatchments}, to create a grid to be used as prediction/simulation locations .
}
  \item{grid.type}{
OPTIONAL. Only required when \code{predictors} is missing. See \code{\link[sp]{spsample}}. Character, indicating the type of grid to be computed over the area defined by \code{subcatchments}. Valid values are: \cr
-) \kbd{regular}    : for regular (systematically aligned) sampling; Default option \cr
-) \kbd{random}     : for completely spatial random; \cr
-) \kbd{stratified} : for stratified random (one single random location in each "cell" \cr
-) \kbd{nonaligned} : for non-aligned systematic sampling (nx random y coordinates, ny random x coordinates);  \cr
-) \kbd{hexagonal}  : for sampling on a hexagonal lattice; \cr
-) \kbd{clustered}  : for clustered sampling
}
  \item{nmin}{
OPTIONAL. See \code{\link[gstat]{krige}}. For local interpolation: if the number of nearest observations within distance \code{maxdist} is less than \code{nmin}, a missing value will be generated; see \code{maxdist}. By default \code{nmin=0}.
}
  \item{nmax}{
OPTIONAL. See \code{\link[gstat]{krige}}. For local interpolation: the number of nearest observations that should be used for a kriging prediction, where nearest is defined in terms of the space of the spatial locations. By default, all observations are used.
}
  \item{maxdist}{
OPTIONAL. See \code{\link[gstat]{krige}}. For local interpolation: only observations within a distance of \code{maxdist} from the prediction location are used for prediction or simulation; if combined with \code{nmax}, both criteria apply. By default, all observations are used.
}
  \item{ColorRamp}{
Function defining the colour ramp to be used for plotting the maps OR character representing the colours to be used in the plot. In the latter case, valid values are in: \code{c('Precipitation', 'Temperature', 'PCPAnomaly', 'PCPAnomaly2', 'TEMPAnomaly', 'TEMPAnomaly2', 'TEMPAnomaly3')}
}
  \item{plot}{
Logical, indicating if the interpolated values have to be plotted or not
}
  \item{col.nintv}{
integer, number of colours that have to be used for plotting the interpolated values
}
  \item{col.at}{
Specify at which interpolated values colours change.[MJ m-2 day-1] or [mm d-1]
   \code{at   <- seq(min, max,length.out=col.nintv)} \cr
   \code{min  <- floor(min(idw["var1.pred"]@data, na.rm=TRUE))} \cr
   \code{max  <- ceiling(max(idw["var1.pred"]@data, na.rm=TRUE))}
}
  \item{main}{
Character with the title to be used for the plot.
}
  \item{stations.plot}{
Logical, indicating if the gauging stations, defined by \code{x.gis} have to be plotted
}
  \item{stations.offset}{
2D list with the numeric coordinates in which the label with the amount of gauging stations have to be plotted.  e.g., \code{stations.offset = c(450000, 4600000)}.
}
  \item{arrow.plot}{
Logical, indicating if a North Arrow have to be plotted
}
  \item{arrow.offset}{
2D list with the numeric coordinates in which the north arrow has to be plotted. e.g., \code{arrow.offset = c(690000,4760000)}
}
  \item{arrow.scale}{
Scale (in the map units) to be used for plotting the north arrow, e.g., \code{scale = 20000}
}
  \item{scalebar.plot}{
Logical, indicating if a Scale Bar has to be plotted
}
  \item{sb.offset}{
2D list with the numeric coordinates in which the North Arrow has to be plotted. e.g., \code{sb.offset = c(400000,4490000)}
}
  \item{sb.scale}{
Scale (in the map units) to be used for plotting the Scale Bar, e.g., \code{scale = 100000}, means that the scale bar will have a length of 100km
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
}
  \item{allNA.action}{
Action to be executed when all the values in \code{x.ts} are NA. Valid values are: \cr
-) \kbd{"error"}: it will produce an error message. Default option \cr
-) a single numeric value that will replace all the NA values in \code{x.ts}, giving place to a map with a constant value. \bold{At your own risk !}
}
  \item{dates}{
numeric, factor, or character object indicating how to obtain the ID (usually dates) that will be used to identify the interpolation carried out for each row of \code{x.ts}. \cr
If \code{dates} is a single number (default), it indicates the index of the column in \code{x.ts} that stores the dates \cr
If \code{dates} is a factor or character vector, its values will be used as ID for the interpolations carried out in each row of \code{x.ts}.
}
  \item{from}{
Character indicating the starting date for the values stored in all the files that will be read.
}
  \item{to}{
Character indicating the ending date for the values stored in all the files that will be read.
}
  \item{write2disk}{
Logical. Indicates if we want to write the output into a CSV file. Default value is TRUE
}
  \item{out.fmt}{
OPTIONAL, only needed when \code{write2disk==TRUE}. \cr
Character indicating the type of csv file to be written with the results. Valid values are \kbd{csv} and \kbd{csv2}. For more information, see \code{\link[utils]{write.table}}
}
  \item{fname}{
OPTIONAL. Character with the filename of the output file. Only needed when \code{write2disk=TRUE}
}
  \item{\dots}{
further arguments passed to or from other methods. In particular, these further arguments are passed to the function \env{idw} (\pkg{gstat} package) OR \code{\link[automap]{autoKrige}} (\pkg{automap} package), depending on the value passed to \code{formula} (see 'details' below): \cr
-) for IDW, the arguments are passed to    : \code{idw(formula, locations, newdata, nmin, nmax, maxdist, ...)} \cr
-) for OK, KED, the arguments are passed to: \code{autoKrige(formula, input_data, new_data, nmin, nmax, maxdist, verbose=verbose, ...)}
}
}

\details{
The type of interpolation (IDW, OK, KED) is obtained from the argument \code{formula}: \cr

-) When \code{formula} is missing, an IDW interpolation, by calling the \code{idw} function in the \pkg{gstat} package, with \code{formula} = \code{value~1}. \cr
-) When \code{formula} = \code{value~1}, an OK interpolation, by calling the \code{\link[automap]{autoKrige}} function, with \code{formula} = \code{value~1}. \cr
-) When \code{formula} = \code{value~pred1 + pred2 + ...}, a KED interpolation, by calling the \code{\link[automap]{autoKrige}} function, with the \code{formula} specified by the user. \cr

When \code{type} is \kbd{block} or \kbd{both}, a block interpolation is carried out for each subcatchment defined by \code{subcatchments}, so far, computing the average value over all the cells belonging to each subcatchment. \cr

The automatic kriging is carried out by using a variogram generated automatically with the \code{\link[automap]{autofitVariogram}} function of the \pkg{automap} package. \cr
}
\value{
\item{Cells}{When \code{type} is \kbd{cells}, the output object is a \link[sp]{SpatialPixelsDataFrame-class}, which slot 'data' has two variables: 'var1.pred' and 'var1.var' with the predictions and its variances, respectively} \cr

\item{Block}{When \code{type} is \kbd{block}, the output object is a \link[sp]{SpatialPolygonsDataFrame-class}, which slot 'data' has four variables: 'x', 'y'  with the easting and northing coordinate of the centroid of the catchments specified by \code{subcatchments} , and 'var1.pred' and 'var1.var' with the predictions and its variances, respectively} \cr

\item{list(Cells, Block)}{When \code{type} is \kbd{both}, the resulting object is a list, with the two elements previously described.}
}

\references{
\cite{N.A.C. Cressie, 1993, Statistics for Spatial Data, Wiley.} \cr

\cite{Applied Spatial Data Analysis with R. Series: Use R. Bivand, Roger S., Pebesma, Edzer J., Gomez-Rubio, Virgilio. 2008. ISBN: 978-0-387-78170-9} \cr

\cite{Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30: 683-691} \cr

\url{http://www.gstat.org/} \cr

\url{http://rspatial.r-forge.r-project.org/}
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
\note{
IMPORTANT: It is you responsibility to check the validity of the fitted variogram !!.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[gstat]{krige}, \link[automap]{autoKrige}, \link[maptools]{readShapePoly}, \link[sp]{spsample}
}
\examples{
############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Loading the gis data
data(EbroPPgis)

## Loading the shapefile with the subcatchments
data(EbroCatchmentsCHE)

## Projection for the Subcatchments file
# European Datum 50, Zone 30N
require(sp)
p4s <- CRS("+proj=utm +zone=30 +ellps=intl +units=m +no_defs")

## Selecting the first day of 'EbroPPtsMonthly' for all the stations.
# The first column of 'EbroPPtsMonthly' has the dates
x.ts <- as.numeric(EbroPPtsMonthly[1, 2:ncol(EbroPPtsMonthly)])

## Setting the name of the gauging stations
names(x.ts) <- colnames(EbroPPtsMonthly[1,2:ncol(EbroPPtsMonthly)])

##################################################
## 1) IDW interpolation and plot
## Probably you will need to resize your window 
\dontrun{
x.idw <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                    X="EAST_ED50", Y="NORTH_ED50", sname="ID", bname="CHE_BASIN_NAME",
                    type= "both",
                    subcatchments= EbroCatchmentsCHE,
                    cell.size= 3000, 
                    ColorRamp= "Precipitation",	
                    main= "IDW Precipitation on the Ebro")
}
           
##################################################
## 2) Ordinary Kriging interpolation and plot, in catchments defined by a shapefile
## Probably you will need to resize your window
\dontrun{ 

# Computing OK, over of 3000x3000m, sampled withinthe subcatchments defined by 'subcatchments'
x.ok <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                   X="EAST_ED50", Y="NORTH_ED50", sname="ID", bname="CHE_BASIN_NAME",
                   type= "both", formula=value~1,
                   subcatchments= EbroCatchmentsCHE,
                   p4s= p4s, 
                   cell.size= 3000, 
                   ColorRamp= "Precipitation", 
                   main= "OK Precipitation on the Ebro", arrow.plot= TRUE, 
                   arrow.offset= c(900000,4750000), arrow.scale= 20000,
                   scalebar.plot= TRUE, 
                   sb.offset= c(400000,4480000), sb.scale= 100000)


# Getting the interpolated values in each polygon 
# ('var1.pred' field in the output data.frame object)
x.ok.block <- slot(x.ok[["Block"]], "data")

# Getting the interpolated values in each cell (SpatialPixelsDataFrame object)
# (the pedicted values in each cell are stored in the 'var1.pred' field of the 
#  'data' slot)
x.ok.cells <- x.ok[["Cells"]]

# Plotting the interpolated values in each cell
spplot(x.ok.cells, "var1.pred")
}

##################################################
## 3)  Ordinary Kriging interpolation and plot, in an area defined by a raster map.
## The raster map may be any \link[sp]{SpatialGridDataFrame-class} object, read with 
## the \code{\link[rgdal]{readGDAL}} function of the \pkg{rgdal} package or similar. 
## Probably you will need to resize your window

#Loading the DEM
data(EbroDEM1000m)

#Giving a meaningful name to the predictor
EbroDEM1000m$ELEVATION <- EbroDEM1000m$band1

# Saving memory
EbroDEM1000m$band1 <- NULL

# Computing OK, over the spatial grid defined by the DEM
\dontrun{
x.ok <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                   X="EAST_ED50", Y="NORTH_ED50", sname="ID", 
                   formula=value~1,
                   p4s= p4s, 
                   predictors=EbroDEM1000m,
                   ColorRamp= "Precipitation", 
                   main= "OK Precipitation on the Ebro",
                   arrow.plot= TRUE, 
                   arrow.offset= c(900000,4750000), arrow.scale= 20000,
                   scalebar.plot= TRUE, 
                   sb.offset= c(400000,4480000), sb.scale= 100000)
}

##################################################
## 4) Kriging with External Drift interpolation and plot
## Probably you will need to resize your window

#Loading the DEM
data(EbroDEM1000m)

#Giving a meaningful name to the predictor
EbroDEM1000m$ELEVATION <- EbroDEM1000m$band1

# Saving memory
EbroDEM1000m$band1 <- NULL

# Forcing the projection of the DEM to be the same of the 'subcatchments' argument
# (just because I know in advance they are the same)
proj4string(EbroDEM1000m) <- p4s

# Computing KED
\dontrun{
x.ked <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                    X="EAST_ED50", Y="NORTH_ED50", sname="ID", 
                    bname="CHE_BASIN_NAME", elevation="ELEVATION",
                    type= "cells", 
                    formula=value~ELEVATION,
                    subcatchments= EbroCatchmentsCHE,
                    predictors=EbroDEM1000m,
                    cell.size= 3000, 
                    ColorRamp= "Precipitation", 
                    main= "KED Precipitation on the Ebro",
                    arrow.plot= TRUE, 
                    arrow.offset= c(900000,4750000), arrow.scale= 20000,
                    scalebar.plot= TRUE, 
                    sb.offset= c(400000,4480000), sb.scale= 100000)
}

##################################################
## 5) Block IDW interpolation and plot of 'EbroPPtsMonthly' for 3 months
\dontrun{
x.idw <- hydrokrige(x.ts= EbroPPtsMonthly, x.gis=EbroPPgis, 
                  X="EAST_ED50", Y="NORTH_ED50", sname="ID", 
                  bname="CHE_BASIN_NAME",
                  type= "cells", #'both'
                  subcatchments= EbroCatchmentsCHE,
                  cell.size= 3000,
                  ColorRamp= "Precipitation",
                  arrow.plot= TRUE, 
                  arrow.offset= c(900000,4750000), arrow.scale= 20000,
                  scalebar.plot= TRUE, 
                  sb.offset= c(400000,4480000), sb.scale= 100000,
                  dates=1, 
                  from="1942-01-01", to="1942-03-01")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
%%\keyword{manip}% __ONLY ONE__ keyword per line
