\name{EbroPPgis}
\docType{data}
\alias{EbroPPgis}
\title{
Spatial location of raingauges on the Ebro River basin
}
\description{
Spatial location of the 349 raingauges with daily precipitation on the Ebro River Basin (dataset \var{'EbroPPts'}), Spain, with more than 70\% of days with information (without missing values) 
}
\usage{
data(EbroPPgis)
}

\details{
Projection: European Datum 50, Zone 30N.
}
\format{
A data.frame with seven fields: \cr
*) 'ID             : identifier of each gauging station. \cr
*) 'STATION_NAME'  : name of the gauging station. \cr
*) 'EAST_ED50'     : easting coordinate of the gauging station. European Datum 50, Zone 30 North. \cr
*) 'NORTH_ED50'    : northing coordinate of the gauging station. European Datum 50, Zone 30 North. \cr 
*) 'ELEVATION'     : elevation of the gauging station, [m a.s.l.]. \cr
*) 'CHE_BASIN_ID'  : identifier of the subbasin in which the gauging station s located. \cr
*) 'CHE_BASIN_NAME': name of the subbasin in which the gauging station s located.
}
\source{
Downloaded ('Red de Control Meteorologico') from the web site of the Confederacion Hidrografica del Ebro (CHE) \url{http://www.chebro.es/} (original link http://oph.chebro.es/ContenidoCartoClimatologia.htm, last accessed [March 2008]), and then the name of 7 selected fields were translated into English language.  \cr

These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{datasets}
