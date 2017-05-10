%%File MaquehueTemuco.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{MaquehueTemuco}
\docType{data}
\alias{MaquehueTemuco}
\title{
San Martino, ts of daily precipitation.
}
\description{
Daily time series of precipitation, maximum and minimun air temperature at station Maquehue Temuco Ad. (DMC_ID:380013), Araucania Region, Chile (Lat:-38.770, Lon:-72.637), with data from 01/Jan/1950 to 31/Dec/2015 (including some gaps).
}
\usage{
data(MaquehueTemuco)
}

%%\details{
%%
%%}
\format{
zoo matrix with 3 columns: \cr
-) \var{pcp}: daily precipitation, [mm/day]. \cr
-) \var{tmx}: daily maximum air temperature, [\U00B0 C]. \cr
-) \var{tmn}: daily minimum air temperature, [\U00B0 C]. \cr 
}
\source{
Provided by Center for Climate and Resilience Research, Universidad de Chile, Santiago, Chile (\url{http://www.cr2.cl/recursos-y-publicaciones/bases-de-datos/}, last accessed [May 2017]). \cr
These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{datasets}
