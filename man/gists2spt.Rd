%% File gis2spt.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{gists2spt}
\Rdversion{1.1}
\alias{gists2spt}
\title{
Time Series and GIS Info -> (pseudo)Spatio-Temporal Object
}
\description{
 Given a data.frame (\code{x.gis}) with the spatial coordinates of a set of measurement points (e.g., gauging stations) and the measurements in those stations (\code{x.ts}) this function merges the measurements in \code{x.ts} with the corresponding spatial location in \code{x.gis}, even if they are not in the same order. \cr

If the spatial coordinates \code{X} and \code{Y} are given for \code{x.gis}, the resulting object will be a \link[sp]{SpatialPointsDataFrame-class} with coordinates given by the \code{X} and \code{Y} fields \cr

If \code{p4s} is given, the resulting object will be projected according to the specification provided by \code{p4s}.
}
\usage{
gists2spt(x.gis, x.ts, sname, bname, X, Y, elevation,
          catchment.name="all", na.rm=TRUE, p4s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.gis}{
data.frame with the spatial information for all the measurement points in \code{x.gis}. The name of each station, stored in the field \code{sname}, have to be equal to the corresponding ID used in \code{x.ts} \cr
-) It MAY contain as many measurement points as you want, e.g., all the gauging stations in your database, but \cr
-) It HAVE TO contain -at least- the location of the measurement points of \code{x.ts} that will be used for the interpolations. \cr
   The MINIMUM fields that HAVE TO be present in this file, and their corresponding column index are \code{X}, \code{Y}, \code{sname}.
}
 \item{x.ts}{
numeric or data.frame with the measured value at each station for a given time. Each value of \code{x.ts} has to have as name (\code{names(x.ts)}) the ID of the station. \cr
1) It MAY contain as many stations as you want, e.g., all the gauging stations in the your database, but \cr
2) It HAVE TO contain -at least- some stations of \code{x.gis} \cr
}
  \item{sname}{
character, field name in \code{x.gis} that stores the name of the stations ( \bold{the name of the stations have to start by a letter !!})
}
  \item{bname}{
OPTIONAL. character, field name in \code{x.gis} that stores the name of the subcatchment in \code{x.gis} that will be analysed. \cr
ONLY necessary when 'catchment.name' is not "all"
}
  \item{X}{
character, field name in \code{x.gis} that stores the easting coordinate of the measurement points. The expected name is 'x', but if the value provided by the user is different, a new 'x' field is created and is used as the easting coordinate of \code{x.gis}
}
  \item{Y}{
character, field name in \code{x.gis} that stores the northing coordinate of the measurement points. The expected name is 'y', but if the value provided by the user is different, a new 'y' field is created and is used as the northing coordinate of \code{x.gis}
}
  \item{elevation}{
OPTIONAL. character, field name \code{x.gis} that stores the elevation of the stations (m a.s.l.).
}

  \item{catchment.name}{
name of the catchment that will be analysed. Possible values are: \cr
-)\kbd{all}          : ALL the stations in the \code{x.gis} will be used \cr
-)other character: ONLY those stations in \code{x.gis} with a \code{bname} field value == \code{catchment.name} will be used.
}
  \item{p4s}{
Character with information about the projection of the GIS files, usually created by the CRS function of the \pkg{sp} package
}
  \item{na.rm}{
a logical value indicating whether 'NA' values should be stripped before delivering the resulting object.
}
 
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
If \code{p4s} is given, the returning object will be a \link[sp]{SpatialPointsDataFrame-class}, if not, it will be a data.frame
}
\references{
\cite{Applied Spatial Data Analysis with R. Series: Use R. Bivand, Roger S., Pebesma, Edzer J., Gomez-Rubio, Virgilio. 2008. ISBN: 978-0-387-78170-9} \cr

\url{http://rspatial.r-forge.r-project.org/}
     
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[gstat]{krige}, \link[sp]{spplot}
}
\examples{
############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Loading the gis data
data(EbroPPgis)    

## Putting the measurements of the first row of 'EbroPPtsMonthly' into their 
## corresponding spatial location given by 'x.gis'
require(sp)
x.spt <- gists2spt(x.ts=EbroPPtsMonthly[1,], x.gis=EbroPPgis, X="EAST_ED50", 
                   Y="NORTH_ED50", na.rm=FALSE, sname="ID")

## Plotting the measured values (only the first row of 'EbroPPtsMonthly') at their 
## corresponding spatial location
spplot(x.spt, zcol="value")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
