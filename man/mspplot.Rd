%%File mspplot.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{mspplot}
\Rdversion{1.1}
\alias{mspplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple spplot
}
\description{
Plots several spatial maps in the same plotting window, with options to easily add a scale bar and north arrow. \cr
Originally, it was though to make easier the display of several maps (of the same spatial extent) obtained by different interpolation methods or for different time steps.
}
\usage{

mspplot(x, subcatchments, IDvar = NULL, p4s=CRS(as.character(NA)), 
        plot = TRUE, ColorRamp="PCPAnomaly",col.nintv = 10, col.at = "auto", 
        main, stations.plot = FALSE, stations.gis, X, Y, 
        arrow.plot = FALSE, arrow.offset, arrow.scale, 
        scalebar.plot = FALSE, sb.offset, sb.scale, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ 
\link[sp]{SpatialPixelsDataFrame-class} object that stores (in the \code{@data} slot) all the maps that will be plotted.
}
  \item{subcatchments}{
\link[sp]{SpatialPolygonsDataFrame-class} with all the subcatchments within the study area, \bold{OR} character representing the filename (with path) of the shapefile ('polygon' type) that stores those subcatchments.
}
  \item{IDvar}{ 
See \link[maptools]{readShapePoly}. Character string with the name of a column in the \code{subcatchments} shapefile DBF containing the ID values of the catchments - the values will be converted to a character vector.
}
  \item{p4s}{
Character with information about the projection of the GIS files, usually created by the CRS function of the \pkg{sp} package
}
  \item{plot}{
Logical, indicating if the interpolated values have to be plotted or not
}
  \item{ColorRamp}{
Character or function defining a personalized color ramp for plotting the maps. \cr
Valid character values are in \kbd{c("Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2" "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3")}.
}
 \item{col.nintv}{
integer, number of colors that have to be used for plotting the interpolated values
}
  \item{col.at}{
Specify at which interpolated values colours change. Valid values are:  \cr
-) \kbd{R}   : uses the default setting of 'spplot'  \cr
-) \kbd{auto}: default option. \cr
          \code{at   <- seq(min, max,length.out=col.nintv)} \cr
          \code{min  <- floor( min(idw["var1.pred"]@data, na.rm=TRUE))} \cr
          \code{max  <- ceiling( max(idw["var1.pred"]@data, na.rm=TRUE))} \cr
-) numeric: vector giving the exact values in which the colors have to change.\cr
            Useful when the user desires the same color for the same value when comparing two or more maps with different range of values
}
  \item{main}{
Character with the title to be used for the plot
}
  \item{stations.plot}{
Logical, indicating if the gauging stations, defined by \code{stations.gis} have to be plotted
}
  \item{stations.gis}{
OPTIONAL. data.frame with the stations that will be added to the plot. ONLY required when \code{stations.plot == TRUE}.
}
  \item{X}{
OPTIONAL. character, field name in \code{x.gis} that stores the easting coordinate of the stations. ONLY required when \code{stations.plot == TRUE}.
}
  \item{Y}{
OPTIONAL. character, field name in \code{x.gis} that stores the northing coordinate of the stations. ONLY required when \code{stations.plot == TRUE}.
}
  \item{arrow.plot}{
Logical, indicating if a North Arrow have to be plotted
}
  \item{arrow.offset}{
OPTIONAL. 2D list with the numeric coordinates in which the North Arrow have to be plotted. e.g., \kbd{arrow.offset = c(690000,4760000)}. ONLY required when \code{arrow.plot=TRUE}
}
  \item{arrow.scale}{
OPTIONAL. Scale (in the map units) to be used for plotting the North Arrow, e.g., scale = 20000. ONLY required when \code{arrow.plot=TRUE}.
}
  \item{scalebar.plot}{
Logical, indicating if a Scale Bar have to be plotted
}
  \item{sb.offset}{
OPTIONAL. 2D list with the numeric coordinates in which the North Arrow have to be plotted. e.g., \kbd{sb.offset = c(400000,4490000)}. ONLY required when \code{scalebar.plot=TRUE}
}
  \item{sb.scale}{
OPTIONAL. Scale (in the map units) to be used for plotting the Scale Bar, e.g., \kbd{scale = 100000}, means that the scale bar will have a length of 100km. ONLY required when \code{scalebar.plot=TRUE}.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\references{
\cite{Applied Spatial Data Analysis with R. Series: Use R. Bivand, Roger S., Pebesma, Edzer J., Gomez-Rubio, Virgilio. 2008. ISBN: 978-0-387-78170-9} \cr
\url{http://r-spatial.sourceforge.net/gallery/}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
%%\note{
%%
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[sp]{spplot}}, \code{\link[gstat]{krige}}, \code{\link[automap]{autoKrige}}

}
\examples{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Loading the gis data
data(EbroPPgis)

## Loading the shapefile with the subcatchments
data(EbroCatchmentsCHE)

## Projection for the Subcatchments file
require(sp)
p4s <- CRS("+proj=utm +zone=30 +ellps=intl +units=m +no_defs")

## Field name in 'x.gis' with the ID of the station
sname <- "ID"
## Field name in 'x.gis'with the name of the catchment to which each station belongs
bname <- "CHE_BASIN_NAME"
## Field name in 'x.gis' with the Easting spatial coordinate
X     <- "EAST_ED50" 
## Field name in 'x.gis' with the Northing spatial coordinate
Y     <- "NORTH_ED50" 
## Field name in 'x.gis' with the Elevation
elevation <- "ELEVATION" 



#######
## Selecting Jan/1961 (first row) of 'EbroPPtsMonthly' in all the stations
x.ts <- as.numeric(EbroPPtsMonthly[1, 2:ncol(EbroPPtsMonthly)])

## Setting the name of the stations
names(x.ts) <- colnames(EbroPPtsMonthly[ ,2:ncol(EbroPPtsMonthly)])


########
## IDW interpolation and plot (Jan/61)
x.idw <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                    X=X, Y=Y, sname=sname, bname=bname, elevation=elevation, 
                    type= "cells", #'both'
                    subcatchments= EbroCatchmentsCHE, p4s= p4s, 
                    cell.size= 3000, nmax= 50,
		    ColorRamp= "Precipitation",
		    main= "IDW Mean Annual Precipitation on the Ebro, Jan/1961")

## Storing the interpolated values
x.idw@data["Jan1961"]   <- x.idw@data["var1.pred"]
x.idw@data["var1.pred"] <- NULL
x.idw@data["var1.var"]  <- NULL


## Selecting the Jul/1961 of 'EbroPPtsMonthly' for all the stations
x.ts <- as.numeric(EbroPPtsMonthly[7, 2:ncol(EbroPPtsMonthly)])

## Setting the name of the stations
names(x.ts) <- colnames(EbroPPtsMonthly[ , 2:ncol(EbroPPtsMonthly)])

\dontrun{
## IDW interpolation and plot (Jul/1961)
x.idw2 <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                     X=X, Y=Y, sname=sname, bname=bname, elevation=elevation,
                     type= "cells", #'both'
                     subcatchments= EbroCatchmentsCHE, p4s= p4s, 
                     cell.size= 3000, nmax= 50,
                     ColorRamp= "Precipitation",
                     main= "IDW Mean Annual Precipitation on the Ebro, Jul/1961")

# Adding the interpolated value for Jul/1961 to 'x.idw'
x.idw@data["Jul1961"] <- x.idw2@data["var1.pred"]


## Plotting in the same graph the 2 interpolated fields
mspplot(x=x.idw, 
        subcatchments=EbroCatchmentsCHE, 
        IDvar=NULL, p4s, 
        col.nintv=50, 
        main="IDW Monthly Precipitation on the Ebro River basin, [mm]",
        stations.plot=FALSE,
        arrow.plot=TRUE, arrow.offset=c(900000,4750000), arrow.scale=20000,
        scalebar.plot=TRUE, sb.offset=c(400000,4480000), sb.scale=100000)	
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
