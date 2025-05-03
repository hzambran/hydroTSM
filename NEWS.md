NEWS/ChangeLog for hydroTSM

--------------------------
# Changes in version 0.7-5  Ongoing

## New features   
        o 'matrixplot'        :  -) new arguments: cuts,  cuts.dec=2, cuts.labels, cuts.style, legend.cex, legend.title, legend.title.cex, legend.fontsize.
        o 'daily2annual'      :  -) new argument 'start.month' to choose the starting month to be used in the computation of annual values.
                                    By default 1 (Jan)
        o 'cmv'               :  -) new argument 'start.month' to choose the starting month to be used in the computation of annual values.
                                    By default 1 (Jan)

## Bug fixes
        o 'izoo2rzoo'         :  -) argument 'tz' is now able to correctly handle sub-daily zoo objects, with or without specification of a value by the user.
        o 'subhourly2hourly'  :  -) new argument 'tz' to correctly handle sub-daily zoo objects, with or without specification of a value by the user.
                                 -) 'na.rm.max' is now correctly verified to be in [0, 1]
                                 -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
        o 'subhourly2nminutes':  -) new argument 'tz' to correctly handle sub-daily zoo objects, with or without specification of a value by the user.
        o 'subdaily2daily'    :  -) 'na.rm.max' is now correctly verified to be in [0, 1]
                                 -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
        o 'daily2monthly'     :  -) 'na.rm.max' is now correctly verified to be in [0, 1]
                                 -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
        o 'daily2annual'      :  -) 'na.rm.max' is now correctly verified to be in [0, 1]. Thanks to Hector Garces for reporting it !.
                                 -) now it correctly aggregates the daily values into annual ones when 'na.rm' is a missing argument
                                 -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
                                    corresponding annual value will be ‘NA’.
        o 'daily2weekly'      :  -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
        o 'subdaily2weekly'   :  -) now it correctly set as NA annual values where the percentage of missing values is larger (not larger or equal) than 'na.rm.max'
        o 'hydroplot'         :  -) now correctly works with monthly objects when 'pfreq=o'

## Package files
        o Now hydroTSM imports the 'timechange' package, in order to allow the correct handling of time zones in the 'izoo2rzoo' function.
        o "new internal function '.shiftyears' to allow the computation of annual values starting in a month different from january.



# Changes in version 0.7-0  17-Jan-2024

## New functions
        o 'baseflow'          : for computing baseflow using the filter proposed by Arnold and Allen (1999). 

        o 'plot_pq'           : for plotting precipitation and streamflow time series in the same figure, or for plotting monthly variation of streamflow and precipitation values, with uncertainty bounds around median values.

        o 'calendarHeatmap'   : for displaying time-series as a calendar heatmap

        o 'subhourly2hourly'  : for aggregating sub-hourly values into hourly ones.

        o 'subhourly2nminutes': for aggregating sub-hourly values into n-minutes ones.

        o 'daily2weekly'      : for aggregating (sub)daily values into weekly ones.

        o 'subdaily2weekly'   : for aggregating subdaily values into weekly ones, with optional sarting time for a day.

        o 'cmv'               : to compute the percentage/amount of missing values in a zoo object, using a user-defined temporal scale.

        o 'si'                : to compute the seasonality index for precipitation (Walsh and Lawler, 1981).

## New features                       
        o '(sub)daily2monthly':  -) new argument 'na.rm.max' to define the maximum percentage of missing values allowed in each month to keep the monthly aggregated value in the output object.

        o 'subdaily2daily'    :  -) new argument 'na.rm.max' to define the maximum percentage of missing values allowed in each day.
                                 -) new argument 'start.fmt' to indicate the format used in 'start'.
                                 -) new argument 'tz' to indicate the time zone used for both 'x' and start' arguments.
                                
        o 'daily2annual'      :  -) new argument 'na.rm.max' to  to define the maximum percentage of missing values allowed in each year.
        
        o 'daily2monthly'     :  -) new argument 'na.rm.max' to  to define the maximum percentage of missing values allowed in each month.
        
        o 'subdaily2annual'   :  -) new argument 'na.rm.max' to  to define the maximum percentage of missing values allowed in each year.

        o 'hip'               : -) new argument 'tz' to allow the user to specify the desired time zone.

        o 'hydroplot'         : -) new argument 'dates' in order to facilitate the transformation into zoo when working with data.frame objects
                                -) 'from' argument has a default value of NULL
                                -) 'to' argument has a default value of NULL    
                       
        o 'sname2plot'        : -) 'from' argument has a default value of NULL
                                -) 'to' argument has a default value of NULL

        o 'climograph'        :  -) now it plots the minimum and maximum temperature when they are provided 
                                 -) new argument 'start.month' to choose the starting month for the climograph (1=Jan, 12=Dec).
                                 -) new argument 'tmn.col' to choose the color to be used to plot the minimum monthly temperature
                                 -) new argument 'tmx.col' to choose the color to be used to plot the maximum monthly temperature
                                 -) new argument 'pcp.labels' to decide whether to show the numeric values above the monthly precipitation bars
                                 -) new argument 'tmean.labels' to decide whether to show the numeric values above the monthly mean temperature lines
                                 -) new argument 'tmx.labels' to decide whether to show the numeric values above the monthly maximum temperature lines
                                 -) new argument 'tmn.labels' to decide whether to show the numeric values above the monthly minimum temperature lines
                                 -) new argument 'pcp.labels.cex' to specify the relative scaling of the monthly precipitation values shown in the climograph
                                 -) new argument 'temp.labels.cex' to specify the relative scaling of the monthly air temperature values (mean, maximum, minimum) shown in the climograph
                                 -) new arguments 'temp.labels.dx' and 'temp.labels.dy' to specify the position of air temperature labels
                                 -) new argument 'plot.pcp.probs' to decide whether to show uncertainty values around the monthly mean precipitation values
                                 -) new argument 'pcp.probs' to define the quantile values used to show uncertainty values around the monthly mean precipitation values
                                 -) new argument 'plot.temp.probs' to decide whether to show uncertainty values around the monthly mean temperature values (and also around maximum and minimum temperature, when provided) 
                                 -) new argument 'temp.probs' to define the quantile values used to show uncertainty values around the monthly mean temperature values (and also around maximum and minimum temperature, when provided) 
                                 -) new argument 'temp.probs.col' to define the colors used to show uncertainty values around the monthly mean temperature values (and also around maximum and minimum temperature, when provided) 
                                 -) new argument 'temp.probs.alpha' to define the transparency level applied to 'temp.probs.col'
                                 -) new arguments 'lat' and 'lon' to define and show the latitude and longitude, respectively, for which the climograph was plotted.

        o 'izoo2rzoo'         :  -) improved handling of sub-daily time series
                                 -) new argument 'tz' to correctly handle sub-daily zoo objects
                                 -) default value for 'date.fmt' argument was changed from "%Y-%m-%d" to missing (it is automatically detected based on the sampling frequency of 'x')
                                 -) default value for 'tstep' argument was changed from "days" to missing (it is automatically detected based on the sampling frequency of 'x')
        

## New datasets
        o 'Cauquenes7336001'  :  Daily time series of P, Tmx, Tmn, PET and Q for the catchment draining into the 'Cauquenes en El Arrayan' streamflow station.

## Changes in datasets
        o 'KarameaAtGorgeQts' : The time zone of this data sete was changed from "none" (i.e., your local time zone was used every time you loaded this dataset) to "UTC", in order to avoid missing datetimes at times where daylight saving time ocurred.


## Bug fixes
        o 'hydroplot'    : -) now it works correctly with zoo objects that have multiple columns, and separate (internal) functions are provided for zoo and data.frame objects.
     
        o 'sname2plot'   : -) now it works correctly with zoo objects that have multiple columns, and separate (internal) functions are provided for zoo and data.frame objects.
    
        o 'daily2annual' : -) for zoo objects, dates are correctly given in the output when FUN=min or FUN=max (e.g. for getting the date of the annual maximum or minimum)
                           -) the '...' argument is now passed to FUN (thanks to Marfa Saldivia !))
      
        o 'daily2monthly': -) the '...' argument is now passed to FUN (thanks to Marfa Saldivia !))
      
        o 'season2names' : fixed typo: 'autumm' -> 'autumn' (thanks to Belinda Wilson !)

## Removed functions
        o hydrokrige, mspplot, gists2spt, hypsometric: they were deleted due to CRAN warning about the retirement (archiving) of rgdal, rgeos and maptools during October 2023, which forced to remove 'sp', 'gstat', 'automap', 'maptools', 'rgdal' packages from from DESCRIPTION and NAMESPACE files. These functions should be available in other spatial R packages (e.g., terra)

## Removed datasets
        o EbroCatchmentsCHE, EbroDEM1000m, EbroPPgis: they were deleted due to CRAN warning about the retirement (archiving) of rgdal, rgeos and maptools during October 2023, which forced to remove 'sp', 'gstat', 'automap', 'maptools', 'rgdal' packages from from DESCRIPTION and NAMESPACE files. These datasets will be available in other upcoming spatial R package developed for managing raster time series  (e.g., terra)

## Package files
        o Now hydroTSM requires R >= 3.5.0, due to the use of serialized objects (KarameaAtGeorge hourly streeamflows)

        o sp, gstat, automap, maptools, rgdal: removed from DESCRIPTION and NAMESPACE files, due to CRAN warning about the retirement (archiving) of rgdal, rgeos and maptools during October 2023

        o NAMESPACE file : 'hydroplot', 'sname2plot' and 'hypsometric' are now exported S3 methods

        o new vignette: 'Tutorial for Introductory Analysis of Daily Streamflow Data with hydroTSM'

        o Github Actions are used now to test the source code against stable and development R versions on Windows, Ubuntu and MacOS.

        o pkgdown is now used to created the webpage of the package: https://hzambran.github.io/hydroTSM.

        o CITATION file: citEntry changed to bibentry, after notes that prevented acceptance on CRAN.

# Changes in version 0.6-0   11-Mar-2020
        o Package tested against R Under development (unstable) (2020-03-10 r77920) -- "Unsuffered Consequences", following an imperative request made by CRAN.
        o Vignette on Introductory Analysis of Daily Precipitation was moved from Sweave to Knitr and now includes a climograph example
        o 'subdaily2daily'   : new argument 'start' to allow daily observations start at any time different from 00:00:00 UTC.
        o 'time2season'      : class of objects is now tested in a way compatible with the upcoming R 4.0.0
        o 'dm2seasonal'      : class of objects is now tested in a way compatible with the upcoming R 4.0.0
        o 'matrixplot'       : class of objects is now tested in a way compatible with the upcoming R 4.0.0

# Changes in version 0.5-1   07-Aug-2017 (after CRAN comments)
        o DESCRIPTION file   : -) 'Keywords' field completly removed
        o 'dwi'              : -) some examples were wrapped into 'dontrun' to avoid running times larger than 10s in CRAN.

# Changes in version 0.5-0   07-Aug-2017
        o repository management moved from SVN to GIT, including rforge.
        o citation with DOI is now possible (and new CITATION file).
        o new function 'climograph' to draw a climograph based on precipitation and temperature data.
        o 'subdaily2daily'   : -) now it works with several (all?) FUN functions, including 'sum', 'min', 'max'. (thanks to Jamis Bruening !))
        o 'subdaily2daily'   : -) now it works with several (all?) FUN functions, including 'sum', 'min', 'max'. (thanks to Jamis Bruening !))
        o 'monthlyfunction'  : -) argument '...' is now taken into account.
        o 'seasonalfunction' : -) argument '...' is now taken into account.
        o 'annualfunction'   : -) argument '...' is now taken into account.
        o 'hydrokrige'       : -) 'require(maptools)' was replaced by 'requireNamespace("maptools", quietly = TRUE)'
                               -) calls to 'overlay' were replaced by calls to 'over' because 'overaly' was deprecated in 'sp' 1.1-0.
                               -) new examples about how to get the interpolated values in each cell and polygon when 'type="both"' is used (thanks to Jochen Scholtes !)
        o 'mspplot   '       : -) 'require(maptools)' was replaced by 'requireNamespace("maptools", quietly = TRUE)'
        o 'hypsometric'      : -) fixed (very unusual) error: "Error en integrate(f = f, lower = 0, upper = 1) : maximum number of subdivisions reached"
                                  (thanks to Jose Martinez Batlle !)
        o 'extract'          : -) 'trgt' argument allows multiple months or years to be selected (when 'class(trgt) %in% c("integer", "numeric")')
        o 'fdc'              : -) 'cex.lab'  argument is working now (thanks to Elcin Tan !).
                               -) new example about computing streamflow values corresponding to a given percentage of time equalled or exceeded (thanks to Prajjwal Panday !) 
                               -) corrected small error in the documentation of 'lQ.thr' and 'hQ.thr' (thanks to Luis Andrés Guillén !)
        o 'daily2annual.zoo' : -) fixed bug appeared in R 3.2.x (or 3.1.2) which raised an error for data.frames when 'out.fmt="%Y"': Error in format.default(structure(as.character(x), names = names(x), dim = dim(x),  : 
        o 'drawTimeAxis      : -) new argument 'mgp' to provide greater flexibility.
        o DESCRIPTION file   : -) 'sp' now requires version >= 1.1-0, because 'overlay' was deprecated in that version of 'sp'
                               -) 'lattice' package was moved from 'Suggests' to 'Imports' (use of 'levelplot' function in 'matrixplot').
                               -) 'maptools' package was moved from 'Suggests' to 'Imports' (use of 'readShapePoly' function in 'hydrokrige')
                               -) Packages 'stats', 'utils', 'methods', 'graphics', 'grDevices' are now added in the Imports section.
                               -) CRAN website removed from URL field in DESCRIPTION file (requested by CRAN). Only https://github.com/hzambran/hydroTSM is shown now.
        o NAMESPACE file     : -) several functions explicitly imported from the c(stats, utils, methods, graphics, grDevices) to be compatible with R 3.3.0.
                               -) 'plot.xts' NOT used anymore, to avoid error raised by xts>=0.10-0
                               -) 'index' and 'time<-' imported from 'zoo' package.
                               -) 'over', 'coordinates<-', 'proj4string<-' imported from 'sp' package.
                               -) 'readShapePoly' imported from 'maptools' package.
                               -) 'start', 'end' and 'window', 'aggregate' are now explicitly imported from the 'stats' package (affects 'dwi', 'izoo2rzoo' and 'monthlyfunction')
                               -) Fifty three (53) S3 methods are now registered.

# Changes in version 0.4-2-1	22-Jan-2014 (after CRAN comments)
        o 'Author' field was removed from DESCRIPTION file
        o 'WhatsNew.txt' file was renamed 'ChangeLog'
        o 'dwi'    : part of the 'Examples' section was marked as 'dontrun', because it execution took more than 5 seconds.
        o 'mspplot': part of the 'Examples' section was marked as 'dontrun', because it execution took more than 5 seconds.

# Changes in version 0.4-2	21-Jan-2014
        o 'fdc'                        : -) 'lwd' argument is now used (thanks to Bernard Bisselink !)
                                         -) fixed error raised when some element(s) in x were equal to zero, 'plot=TRUE' and 'log=y' or 'log=xy'
                                            (thanks to David Young !). The error was: 
                                            Error in xy.coords(x, y, xlabel, ylabel, log) : 'x' and 'y' lengths differ
        o 'sname2plot'                 : 'dates' argument is not required any more
        o 'sname2ts'                   : 'dates' argument is not required any more
        o 'monthlyfunction.matrix'     : 'dates' argument is not required any more
        o 'monthlyfunction.data.frame' : 'dates' argument is not required any more
        o 'annualfunction.matrix'      : 'dates' argument is not required any more
        o 'annualfunction.data.frame'  : 'dates' argument is not required any more
        o 'seasonalfunction.matrix'    : 'dates' argument is not required any more
        o 'seasonalfunction.data.frame': 'dates' argument is not required any more
        o 'daily2annual.matrix'        : 'dates' argument is not required any more
        o 'daily2annual.data.frame'    : 'dates' argument is not required any more
        o 'daily2monthly.matrix'       : 'dates' argument is not required any more
        o 'daily2monthly.data.frame'   : 'dates' argument is not required any more
        o 'dm2seasonal.matrix'         : 'dates' argument is not required any more
        o 'dm2seasonal.data.frame'     : 'dates' argument is not required any more
        o 'hydrokrige.data.frame'      : 'dates' argument is not required any more
        o 'dwi.data.frame'             : 'dates' argument is not required any more
        o 'maptools:::readShapePoly' was replaced by 'maptools::readShapePoly' in all the package (functions 'hydrokrige' and 'mspplot')
        o 'require(xts)' was removed from 'hydroplot'
        o 'zoo' package removed from 'Imports' and 'Enhances' (due to changes in CRAN policies)
        o 'xts' package removed from 'Imports' (due to changes in CRAN policies)
        o improved vignette (moved from Sweave to knitr)

# Changes in version 0.4-1	31-May-2013
        o DESCRIPTION : 'xts' package was added to the 'Imports' section
        o 'fdc'       :  truncation of some lines in the PDF manual are fixed now.
        o 'sname2plot':  truncation of some lines in the PDF manual are fixed now.

# Changes in version 0.4-0	31-May-2013
        o major changes: -) all the functions should now be compatible with sub-daily time series. See details below.
                         -) package is now tested against R 3.0.X
                         -) updated vignette. Now it includes some examples about computation of extreme indices of daily precipitation.
                         -) new dataset: 'KarameaAtGorgeQts', with hourly streamflows for the Karamea River(New Zeland), provided by the 
                            National Institute of Water and Atmospheric Research (http://www.niwa.co.nz/), thanks to the kind collaboration 
                            of Shailesh Singh.
                         -) e-mail address of maintainer was changed from the @ing.unit.it to the @gmail.com domain
                         -) DESCRIPTION: new 'MailingList' field, indicating the r-sig mailing list to be used for questions related to this 
                            package
 
        o new function: 'subdaily2daily' for aggregating sub-daily values into daily ones
        o new function: 'subdaily2monthly' for computing monthly values from sub-daily time series        
        o new function: 'subdaily2annual' for computing annual values from sub-daily time series 
        o new function: 'subdaily2seasonal' for computing seasonal values for every year of a sub-daily time series
        o new function: 'hip' for creating a sequence (or counting the number) of hours between two date/time objects   
        o 'sname2plot'                 : -) new argument 'season.names', to customise the names of the weather seasons used for plotting 
                                         -) default value for 'dates' argument was set to 1 (before it was missing)  
        o 'hydroplot'                  : new argument 'season.names', to customise the names of the weather seasons used for plotting 
        o 'daily2monthly'              : now it handles sub-daily and weekly values         
        o 'daily2annual'               : now it handles sub-daily, weekly and quarterly values      
        o 'monthlyfunction'            : now it handles sub-daily values 
        o 'dm2seasonal'                : -) now it handles sub-daily and weekly values 
                                         -) improved examples
        o 'time2season'                : now it handles "POSIXct" and "POSIXt" values       
        o 'seasonalfunction.zoo'       : season names are now correctly given (instead of just numbers) for zoo objects that are matrix/data.frame 
                                         as well
        o 'annualfunction.zoo'         : 'na.rm' argument is now used when the input zoo object is a matrix/data.frame (previously a single value 
                                          of 1 was artificially introduced to the annual ts of each column when 'na.rm=TRUE', while a single value 
                                          of 0 was artificially introduced to the annual ts of each column when 'na.rm=FALSE') => generally a very 
                                          small difference
        o 'dip'                        : more informative error messages  
        o 'mip'                        : more informative error messages      
        o 'yip'                        : more informative error messages                                                        
        o 'monthlyfunction.matrix'     : default value for 'dates' argument was set to 1 (before it was missing)
        o 'monthlyfunction.data.frame' : default value for 'dates' argument was set to 1 (before it was missing)  
        o 'annualfunction.matrix'      : default value for 'dates' argument was set to 1 (before it was missing)
        o 'annualfunction.data.frame'  : default value for 'dates' argument was set to 1 (before it was missing)  
        o 'seasonalfunction.matrix'    : default value for 'dates' argument was set to 1 (before it was missing)
        o 'seasonalfunction.data.frame': default value for 'dates' argument was set to 1 (before it was missing)                                              
        o 'daily2annual.matrix'        : default value for 'dates' argument was set to 1 (before it was missing)
        o 'daily2annual.data.frame'    : default value for 'dates' argument was set to 1 (before it was missing)   
        o 'daily2monthly.matrix'       : default value for 'dates' argument was set to 1 (before it was missing)
        o 'daily2monthly.data.frame'   : default value for 'dates' argument was set to 1 (before it was missing)  
        o 'dm2seasonal.matrix'         : default value for 'dates' argument was set to 1 (before it was missing)
        o 'dm2seasonal.data.frame'     : default value for 'dates' argument was set to 1 (before it was missing) 
        o 'hydrokrige.data.frame'      : default value for 'dates' argument was set to 1 (before it was missing)    
        o 'sname2ts'                   : default value for 'dates' argument was set to 1 (before it was missing)  
        o 'zoo2RHtest'                 : broken links were removed and replaced by new ones. Thanks to Gang Dong and Yan Li !
        o NAMESPACE                    : functions from packages zoo and xts are now imported explicitly        	


-- Previous Releases: see old file 'ChangeLog'


# version 0.3-6	 18-Oct-2012
# version 0.3-5	 04-Jul-2012
# version 0.3-4	 03-May-2012
# version 0.3-3	 07-Nov-2011
# version 0.3-2	 15-Sep-2011
# version 0.3-1	 14-Sep-2011
# version 0.3-0  01-Sep-2011
# version 0.2-2	 14-Apr-2011
# version 0.2-1	 30-Nov-2010
# version 0.2-0	 10-Oct-2010
# version 0.1.7	 05-Mar-2009 (It was never released, because it finally becomes v0.2-0)
# version 0.1.6	 30-Nov-2009
# version 0.1.5	 16-Nov-2009
# version 0.1.4	 02-Nov-2009
# version 0.1.3	 13-Oct-2009
# version 0.1.2	 01-Oct-2009
# version 0.1.1	 15-Sep-2009
# version 0.1.0	 07-Sep-2009 (but the functions were developed since 2007)
