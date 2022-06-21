%% File hypsometric.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hypsometric}
\Rdversion{1.1}
\alias{hypsometric}
\alias{hypsometric.SpatialGridDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hypsometric Curve
}
\description{
Computes and plots the hypsometric curve corresponding to the data provided by a digital elevation model (DEM)
}
\usage{
hypsometric(x, ...)

\method{hypsometric}{SpatialGridDataFrame}(x, band=1, main="Hypsometric Curve",
            xlab="Relative Area above Elevation, (a/A)",
            ylab="Relative Elevation, (h/H)", col="blue",
            out.type=c("absolute", "realtive"), dec=2, ...)

}
%- maybe also 'usage' for other objects documented here. 
\arguments{
  \item{x}{
\link[sp]{SpatialGridDataFrame-class} object with the elevations of the catchment.
}
  \item{band}{
integer or character indicating the band in \code{x} that stores the elevation data.
}
  \item{main}{
See \code{\link[graphics]{plot}}. An overall title for the plot: see \code{\link[graphics]{title}}.
}
  \item{xlab}{
See \code{\link[graphics]{plot}}. A title for the x axis: see \code{\link[graphics]{title}}.
}
  \item{ylab}{
See \code{\link[graphics]{plot}}. A title for the y axis: see \code{\link[graphics]{title}}.
}
  \item{col}{
See \code{\link[graphics]{plot.default}}. The colors for lines and points.
}
  \item{out.type}{
character indicating whether the resulting hypsometric figure should show absolute or relative values of both elevation and area.
}
  \item{dec}{
numeric indicating the amount o decimal places to be used for rounding the hypsometric values returned by this function.
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\note{
Based on \url{https://lists.osgeo.org/pipermail/grass-stats/2009-January/000916.html}              
}
\details{
The hypsometric curve and the hypsometric integral are non-dimensional measures of the proportion of the catchment above a given elevation. \cr

Strahler (1952, 1964) further asserted that different types of landform have different characteristic shape of their hypsometric curves, dividing landforms into 'young' and 'mature' with decreasing hypsometric integral -the area under the hypsometric curve- with age.
}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\references{
\cite{Strahler, A. N. 1952. Hypsometric (area-altitude) analysis of erosional topography, Geological Society of America Bulletin, 63, 1117-1142} \cr

\cite{Strahler, A. N. 1964. Quantitative geomorphology of drainage basins and channel networks, in Chow, V. T. (Ed.) Handbook of Applied Hydrology, McGraw Hill, New York, 4-39--4-76} \cr

\cite{Luo, W. 1998. Hypsometric analysis with a geographic information system, Computers & Geosciences 24, pp. 815-821} \cr

\cite{Willgoose, G. and Hancock, G. 1998. Revisiting the hypsometric curve as an indicator of form and process in transport-limited catchment, Earth Surface Processes and Landforms 23, pp. 611-623}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}, with contribution of Jan Schwanbeck.
}
%%\note{
%%
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
\dontrun{
# Loading the DEM
require(rgdal)
data(EbroDEM1000m)
dem <- EbroDEM1000m

# Plotting the DEM
require(sp)
spplot(dem, scales=list(draw=TRUE, y=list(rot=90)))

# Computing and plotting the hypsometric curve
hypsometric(dem)

# If the raster file has more than 1 band, and the elevation data are in a 
# band different from 1:
dem$ELEVATION <- EbroDEM1000m$band1 # dummy example
hypsometric(dem, band= 2)
hypsometric(dem, band= "ELEVATION") # same as before, but user-friendly
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
\keyword{manip}
