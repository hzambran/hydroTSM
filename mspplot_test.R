# This piece of code was removed from the 'Examples' section of 'mspplot' because it took more than 5 seconds to execute

library(hydroTSM)

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
