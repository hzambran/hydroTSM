%%File KarameaAtGorgeQts.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{KarameaAtGorgeQts}
\docType{data}
\alias{KarameaAtGorgeQts}
\title{
Karamea at Gorge, time series of hourly streamflows
}
\description{
Time series with hourly streamflows for the Karamea River(New Zealand) measured at the gauging station "Gorge", for the period 01/Jan/1980 to 31/Dec/1985.

Station Number: 95102, Easting Coordinate (NZMG): 2444629.0, Northing Coordinate (NZMG): 5994427.0, Catchment Area (km2): 1160.0.

In November 25th, 2023 (for hydroTSM v0.7-0), the time zone of this data sete was changed from "none" (i.e., your local time zone was used every time you loaded this dataset)
to GMT+12, in order to avoid missing datetimes at times where daylight saving time ocurred. 
However, due to CRAN requirements, in January 2024 (for hydroTSM v0.7-1), the time zone of this data sete was changed from "GMT+12" to UTM, in order to avoid missing datetimes at times where daylight saving time ocurred. 
After the previous change, the initial DateTime of this dataset changed 
from "1980-01-01 08:15:00 -03" (in my local computer) to "1979-12-31 20:15:00 UTC" (everywhere). 

Only two NA elements were removed from the original dataset: the first one and the last one.
}
\usage{
data(KarameaAtGorgeQts)
}

%%\details{
%%
%%}
\format{
zoo object.
}
\source{
Provided by the National Institute of Water and Atmospheric Research \url{https://niwa.co.nz/}, thanks to the gentle collaboration of Shailesh Singh \cr

These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{datasets}
