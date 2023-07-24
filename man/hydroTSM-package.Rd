%% File hydroTSM-package.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2020 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hydroTSM-package}
\Rdversion{1.1}
\alias{hydroTSM-package}
\alias{hydroTSM}
\docType{package}
\title{
Management, analysis, interpolation and plot of hydrological time series, with focus on hydrological modelling
}
\description{
S3 functions for management, analysis, interpolation and plotting of time series used in hydrology and related environmental sciences. In particular, this package is highly oriented to hydrological modelling tasks. 
The focus of this package has been put in providing a collection of tools useful for the daily work of hydrologists (although an effort was made to optimise each function as much as possible, functionality has had priority over speed). Bugs / comments / questions / collaboration of any kind are very welcomed, and in particular, datasets that can be included in this package for academic purposes.
}
\details{
\tabular{ll}{
Package: \tab hydroTSM\cr
Type: \tab Package\cr
Version: \tab 0.6-0\cr
Date: \tab 2017-08-07\cr
License: \tab GPL >= 2 \cr
LazyLoad: \tab yes\cr
Packaged: \tab Wed Mar 11 12:18:13 -03 2020; MZB \cr
BuiltUnder: \tab R version 3.6.2 (2019-12-12) ; x86_64-pc-linux-gnu (64-bit) \cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
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
