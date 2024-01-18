%% File hydroTSM-package.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hydroTSM-package}
\Rdversion{1.1}
\alias{hydroTSM-package}
\alias{hydroTSM}
\docType{package}
\title{
Management, analysis, and plot of hydrological time series, with focus on hydrological modelling
}
\description{
S3 functions for management, analysis, interpolation and plotting of time series used in hydrology and related environmental sciences. In particular, this package is highly oriented to hydrological modelling tasks. 
The focus of this package has been put in providing a collection of tools useful for the daily work of hydrologists (although an effort was made to optimise each function as much as possible, functionality has had priority over speed). Bugs / comments / questions / collaboration of any kind are very welcomed, and in particular, datasets that can be included in this package for academic purposes.

}
\details{
\tabular{ll}{
Package: \tab hydroTSM\cr
Type: \tab Package\cr
Version: \tab 0.7-0\cr
Date: \tab 2024-01-14\cr
License: \tab GPL >= 2 \cr
LazyLoad: \tab yes\cr
Packaged: \tab Wed 17 Jan 2024 20:43:17 -03 ; MZB \cr
BuiltUnder: \tab R version 4.3.2 (2023-10-31) ;x86_64-pc-linux-gnu (64-bit) \cr
}

--------------------------------------------------------------------------------------------------------------------------- \cr
Datasets:

  \tabular{ll}{
    \code{\link{Cauquenes7336001}}  Hydrometeorological time series for "Cauquenes en El Arrayan" catchment.\cr
    \code{\link{EbroPPtsMonthly}}   Ebro Monthly Precipitation Time Series. \cr
    \code{\link{KarameaAtGorgeQts}} Karamea at Gorge, time series of hourly streamflows. \cr
    \code{\link{MaquehueTemuco}}    San Martino, ts of daily precipitation. \cr
    \code{\link{OcaEnOnaQts}}       Oca in "Ona" (Q0931), time series of daily streamflows \cr
    \code{\link{SanMartinoPPts}}    San Martino, ts of daily precipitation. \cr
--------------------------------------------------------------------------------------------------------------------------- \cr
}

Temporal aggregation: 

  \tabular{ll}{
    \code{\link{annualfunction}}    single representative annual value of a zoo object.\cr
    \code{\link{weeklyfunction}}    single representative weekly value of a zoo object. \cr
    \code{\link{monthlyfunction}}   single representative monthly value of a zoo object. \cr
    \code{\link{seasonalfunction}}  representative values of each weather season of a zoo object. \cr
    \code{\link{daily2annual}}      Aggregation from daily to annual \cr
    \code{\link{subdaily2annual}}   Aggregation from subdaily to annual. \cr
    \code{\link{monthly2annual}}    Aggregation from monthly to annual. \cr
    \code{\link{daily2monthly}}     Aggregation from daily to monthly. \cr
    \code{\link{subdaily2monthly}}  Aggregation from subdaily to monthly. \cr
    \code{\link{daily2weekly}}      Aggregation from daily to weekly. \cr
    \code{\link{dm2seasonal}}       Aggregation from daily or monthly to seasonal. \cr
    \code{\link{subdaily2seasonal}} Aggregation from subdaily to seasonal. \cr
    \code{\link{subdaily2daily}}    Aggregation from subdaily to daily. \cr
    \code{\link{subdaily2weekly}}   Aggregation from subdaily to weekly. \cr
    \code{\link{subhourly2hourly}}  Aggregation from subhourly to hourly. \cr
    \code{\link{subhourly2nminutes}}Aggregation from subhourly to n-minutes. \cr
--------------------------------------------------------------------------------------------------------------------------- \cr
}

Temporal manipulation: 

  \tabular{ll}{
    \code{\link{dip}}         Days in period.\cr
    \code{\link{diy}}         Days in year. \cr
    \code{\link{hip}}         Hours in period. \cr
    \code{\link{mip}}         Months in period. \cr
    \code{\link{yip}}         Years in period. \cr
    \code{\link{izoo2rzoo}}   Irregular zoo object to regular zoo objectl. \cr
    \code{\link{time2season}} Time to weather season. \cr
    \code{\link{vector2zoo}}  Numeric and date/times to zoo object. \cr
--------------------------------------------------------------------------------------------------------------------------- \cr
}

Hydrological functions: 

  \tabular{ll}{
    \code{\link{baseflow}}   Baseflow computation.\cr
    \code{\link{climograph}} Climograph \cr
    \code{\link{dwdays}}     Dry and wet days. \cr
    \code{\link{fdc}}        Flow duration curve. \cr
    \code{\link{fdcu}}       Flow duration curve with uncertainty bounds. \cr
    \code{\link{hydroplot}}  Exploratory figure for hydrological time series. \cr
    \code{\link{sname2plot}} Hydrological time series plotting and extraction. \cr
    \code{\link{plot_pq}}    Plot precipitation and streamflow time series in the same figure. \cr
    \code{\link{si}}         Seasonality Index for precipitation. \cr
    \code{\link{sname2ts}}   Station Name -> Time Series. \cr
    \code{\link{zoo2RHtest}} Zoo object -> RHTest. \cr
--------------------------------------------------------------------------------------------------------------------------- \cr
}

Miscelaneous functions:

  \tabular{ll}{
    \code{\link{calendarHeatmap}} Calendar heat map.\cr
    \code{\link{cmv}}             Counting missing values. \cr
    \code{\link{drawxaxis}}       Draw a temporal horizontal axis. \cr
    \code{\link{dwi}}             Days with information. \cr
    \code{\link{extract}}         Extract a subset of a zoo object. \cr
    \code{\link{hydropairs}}      Visual correlation matrix. \cr
    \code{\link{infillxy}}        Infills NA values. \cr
    \code{\link{istdx}}           Inverse standarization. \cr
    \code{\link{ma}}              Moving average. \cr
    \code{\link{matrixplot}}      2D color matrix. \cr
    \code{\link{rm1stchar}}       Remove first character. \cr
    \code{\link{sfreq}}           Sampling frequency. \cr
    \code{\link{smry}}            Improved summary function. \cr
    \code{\link{stdx}}            Standarization. \cr
--------------------------------------------------------------------------------------------------------------------------- \cr
}

}
\author{
Mauricio Zambrano-Bigiarini

Maintainer: Mauricio Zambrano-Bigiarini <mzb.devel@gmail>
}
%%\references{
%%~~ Literature or other references for background information ~~
%%}

\keyword{ package }
\seealso{
\url{https://github.com/hzambran/hydroGOF}. \cr
\url{https://github.com/hzambran/hydroPSO}. \cr
}
\examples{
## Loading the monthly time series (10 years) of precipitation for the Ebro River basin.
data(EbroPPtsMonthly)

#######
## Ex1) Graphical correlation among the ts of monthly precipitation of the first 
##      3 stations in 'EbroPPtsMonthly' (its first column stores the dates).
hydropairs(EbroPPtsMonthly[,2:4])

#######
## Ex2) Annual values of precipitation at the station "P9001"
sname2ts(EbroPPtsMonthly, sname="P9001", dates=1, var.type="Precipitation", 
         tstep.out="annual")

#######
## Ex3) Monthly and annual plots
sname2plot(EbroPPtsMonthly, sname="P9001", var.type="Precipitation", pfreq="ma")


#######
## Ex5)  Mean monthly values of streamflows
## Loading daily streamflows (3 years) at the station 
## Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
monthlyfunction(OcaEnOnaQts, FUN=mean, na.rm=TRUE)

}
