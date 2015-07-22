\name{EbroCatchmentsCHE}
\docType{data}
\alias{EbroCatchmentsCHE}
\title{
Subcatchments of the Ebro River basin (Spain)
}
\description{
Shapefile with 57 subcatchments of the Ebro River basin (Spain)
}
\usage{
data(EbroCatchmentsCHE)
}

\details{
Projection: European Datum 50, Zone 30N.
}

\format{
\link[sp]{SpatialPolygonsDataFrame-class}. \cr

The fields stored in the \code{@data} slot of this object are: \cr
-) \var{CUECHE}: sequential counter \cr
-) \var{CUECHE_ID}: ID of each subcatchment \cr
-) \var{NOMBRE}: name of each subcatchment \cr 
-) \var{AREA_KM2}: are of eac subcatchment, [km2]
}
\source{
Downloaded ('Divisorias Hidrograficas 1:50.000 - CHE') from the web site of the Confederacion Hidrografica del Ebro (CHE) \url{http://www.chebro.es/} (original link http://oph.chebro.es/ContenidoCartoHidrologia.htm, last accessed [March 2008]). \cr
These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

\note{
A small subcatchment of 0.3 km2 was removed of the original shapefile, due to the fact that it did not have name and it was too small for the purposes of this dataset.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{datasets}
