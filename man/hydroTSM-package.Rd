\name{hydroTSM-package}
\Rdversion{1.1}
\alias{hydroTSM-package}
\alias{hydroTSM}
\docType{package}
\title{
Management, analysis, interpolation and plot of hydrological time series, with focus on hydrological modelling
}
\description{
S3 functions for management, analysis, interpolation and plot of hydrological time series, mainly oriented to hydrological modelling tasks. So far, it only works with daily / monthly / seasonal / annual time series. \cr
The focus of this package has been put in providing a collection of tools useful for the daily work of hydrologists (although an effort was made to optimize each function as much as possible, functionality has had priority over speed). \cr
Bugs / comments / questions / collaboration of any kind are very welcomed, and in particular, datasets that can be included in this package for academic purposes.
}
\details{
\tabular{ll}{
Package: \tab hydroTSM\cr
Type: \tab Package\cr
Version: \tab 0.2-2\cr
Date: \tab 2011-03-17\cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
Packaged: \tab Tue Nov 30 16:59:37 CET 2010; MZB \cr
Built: \tab R version 2.12.0 (2010-10-15); i486-pc-linux-gnu \cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano-Bigiarini

Maintainer: Mauricio Zambrano-Bigiarini <mauricio.zambrano@ing.unitn.it>
}
%%\references{
%%~~ Literature or other references for background information ~~
%%}

\keyword{ package }
\seealso{
\url{http://cran.r-project.org/web/packages/hydroGOF/}. \cr

\url{http://rwiki.sciviews.org/doku.php?id=guides:tutorials:hydrological_data_analysis}
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
sname2plot(EbroPPtsMonthly, sname="P9001", var.type="Precipitation", dates=1, pfreq="ma")

#######
## Ex4) IDW interpolation and plot

## Loading the spatial data corresponding to the measurements given by 'EbroPPtsMonthly'
data(EbroPPgis)

## Loading the shapefile (polygon) with the subcatchments
data(EbroCatchmentsCHE)

## Selecting the first day of 'EbroPPtsMonthly' for all the stations
x.ts <- as.numeric(EbroPPtsMonthly[1, 2:ncol(EbroPPtsMonthly)])

## Setting the name of the gauging stations
names(x.ts) <- colnames(EbroPPtsMonthly[1,2:ncol(EbroPPtsMonthly)])

# Computing the interpolated values and plotting them
# Probably you will need to resize your window
\dontrun{
x.idw <- hydrokrige(x.ts= x.ts, x.gis=EbroPPgis, 
                    X="EAST_ED50" , Y="NORTH_ED50" , sname="ID", 
                    bname= "CHE_BASIN_NAME", elevation="ELEVATION",
                    type= "both",
                    subcatchments= EbroCatchmentsCHE,
                    cell.size= 1000)
}

#######
## Ex5)  Mean monthly values of streamflows
## Loading daily streamflows (3 years) at the station 
## Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
monthlyfunction(OcaEnOnaQts, FUN=mean, na.rm=TRUE)

}
