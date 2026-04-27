# Management, analysis, and plot of hydrological time series, with focus on hydrological modelling

S3 functions for management, analysis, interpolation and plotting of
time series used in hydrology and related environmental sciences. In
particular, this package is highly oriented to hydrological modelling
tasks. The focus of this package has been put in providing a collection
of tools useful for the daily work of hydrologists (although an effort
was made to optimise each function as much as possible, functionality
has had priority over speed). Bugs / comments / questions /
collaboration of any kind are very welcomed, and in particular, datasets
that can be included in this package for academic purposes.

## Details

|             |                                                            |
|-------------|------------------------------------------------------------|
| Package:    | hydroTSM                                                   |
| Type:       | Package                                                    |
| Version:    | 0.7-0                                                      |
| Date:       | 2024-01-14                                                 |
| License:    | GPL \>= 2                                                  |
| LazyLoad:   | yes                                                        |
| Packaged:   | Wed 17 Jan 2024 20:43:17 -03 ; MZB                         |
| BuiltUnder: | R version 4.3.2 (2023-10-31) ;x86_64-pc-linux-gnu (64-bit) |

—————————————————————————————————————————  
Datasets:

|                                                                                                                                                                  |                                                                                                                               |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| [`Cauquenes7336001`](https://hzambran.github.io/hydroTSM/reference/Cauquenes7336001.md) Hydrometeorological time series for "Cauquenes en El Arrayan" catchment. | [`EbroPPtsMonthly`](https://hzambran.github.io/hydroTSM/reference/EbroPPtsMonthly.md) Ebro Monthly Precipitation Time Series. |
| [`KarameaAtGorgeQts`](https://hzambran.github.io/hydroTSM/reference/KarameaAtGorgeQts.md) Karamea at Gorge, time series of hourly streamflows.                   | [`MaquehueTemuco`](https://hzambran.github.io/hydroTSM/reference/MaquehueTemuco.md) San Martino, ts of daily precipitation.   |
| [`OcaEnOnaQts`](https://hzambran.github.io/hydroTSM/reference/OcaEnOnaQts.md) Oca in "Ona" (Q0931), time series of daily streamflows                             | [`SanMartinoPPts`](https://hzambran.github.io/hydroTSM/reference/SanMartinoPPts.md) San Martino, ts of daily precipitation.   |
| —————————————————————————————————————————                                                                                                                        |                                                                                                                               |

Temporal aggregation:

|                                                                                                                                            |                                                                                                                                                       |
|--------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`annualfunction`](https://hzambran.github.io/hydroTSM/reference/annualfunction.md) single representative annual value of a zoo object.    | [`weeklyfunction`](https://hzambran.github.io/hydroTSM/reference/weeklyfunction.md) single representative weekly value of a zoo object.               |
| [`monthlyfunction`](https://hzambran.github.io/hydroTSM/reference/monthlyfunction.md) single representative monthly value of a zoo object. | [`seasonalfunction`](https://hzambran.github.io/hydroTSM/reference/seasonalfunction.md) representative values of each weather season of a zoo object. |
| [`daily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md) Aggregation from daily to annual                           | [`subdaily2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md) Aggregation from subdaily to annual.                               |
| [`monthly2annual`](https://hzambran.github.io/hydroTSM/reference/daily2annual.md) Aggregation from monthly to annual.                      | [`daily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md) Aggregation from daily to monthly.                                  |
| [`subdaily2monthly`](https://hzambran.github.io/hydroTSM/reference/daily2monthly.md) Aggregation from subdaily to monthly.                 | [`daily2weekly`](https://hzambran.github.io/hydroTSM/reference/daily2weekly.md) Aggregation from daily to weekly.                                     |
| [`dm2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md) Aggregation from daily or monthly to seasonal.               | [`subdaily2seasonal`](https://hzambran.github.io/hydroTSM/reference/dm2seasonal.md) Aggregation from subdaily to seasonal.                            |
| [`subdaily2daily`](https://hzambran.github.io/hydroTSM/reference/subdaily2daily.md) Aggregation from subdaily to daily.                    | [`subdaily2weekly`](https://hzambran.github.io/hydroTSM/reference/subdaily2weekly.md) Aggregation from subdaily to weekly.                            |
| [`subhourly2hourly`](https://hzambran.github.io/hydroTSM/reference/subhourly2hourly.md) Aggregation from subhourly to hourly.              | [`subhourly2nminutes`](https://hzambran.github.io/hydroTSM/reference/subhourly2nminutes.md)Aggregation from subhourly to n-minutes.                   |
| —————————————————————————————————————————                                                                                                  |                                                                                                                                                       |

Temporal manipulation:

|                                                                                                       |                                                                                                                        |
|-------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------|
| [`dip`](https://hzambran.github.io/hydroTSM/reference/dip.md) Days in period.                         | [`diy`](https://hzambran.github.io/hydroTSM/reference/diy.md) Days in year.                                            |
| [`hip`](https://hzambran.github.io/hydroTSM/reference/hip.md) Hours in period.                        | [`mip`](https://hzambran.github.io/hydroTSM/reference/mip.md) Months in period.                                        |
| [`yip`](https://hzambran.github.io/hydroTSM/reference/yip.md) Years in period.                        | [`izoo2rzoo`](https://hzambran.github.io/hydroTSM/reference/izoo2rzoo.md) Irregular zoo object to regular zoo objectl. |
| [`time2season`](https://hzambran.github.io/hydroTSM/reference/time2season.md) Time to weather season. | [`vector2zoo`](https://hzambran.github.io/hydroTSM/reference/vector2zoo.md) Numeric and date/times to zoo object.      |
| —————————————————————————————————————————                                                             |                                                                                                                        |

Hydrological functions:

|                                                                                                                              |                                                                                                                                         |
|------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| [`baseflow`](https://hzambran.github.io/hydroTSM/reference/baseflow.md) Baseflow computation.                                | [`climograph`](https://hzambran.github.io/hydroTSM/reference/climograph.md) Climograph                                                  |
| [`dwdays`](https://hzambran.github.io/hydroTSM/reference/dwdays.md) Dry and wet days.                                        | [`fdc`](https://hzambran.github.io/hydroTSM/reference/fdc.md) Flow duration curve.                                                      |
| [`fdcu`](https://hzambran.github.io/hydroTSM/reference/fdcu.md) Flow duration curve with uncertainty bounds.                 | [`hydroplot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md) Exploratory figure for hydrological time series.              |
| [`sname2plot`](https://hzambran.github.io/hydroTSM/reference/hydroplot.md) Hydrological time series plotting and extraction. | [`plot_pq`](https://hzambran.github.io/hydroTSM/reference/plot_pq.md) Plot precipitation and streamflow time series in the same figure. |
| [`si`](https://hzambran.github.io/hydroTSM/reference/si.md) Seasonality Index for precipitation.                             | [`sname2ts`](https://hzambran.github.io/hydroTSM/reference/sname2ts.md) Station Name -\> Time Series.                                   |
| [`zoo2RHtest`](https://hzambran.github.io/hydroTSM/reference/zoo2RHtest.md) Zoo object -\> RHTest.                           | —————————————————————————————————————————                                                                                               |

Miscelaneous functions:

|                                                                                                            |                                                                                                        |
|------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| [`calendarHeatmap`](https://hzambran.github.io/hydroTSM/reference/calendarHeatmap.md) Calendar heat map.   | [`cmv`](https://hzambran.github.io/hydroTSM/reference/cmv.md) Counting missing values.                 |
| [`drawxaxis`](https://hzambran.github.io/hydroTSM/reference/drawxaxis.md) Draw a temporal horizontal axis. | [`dwi`](https://hzambran.github.io/hydroTSM/reference/dwi.md) Days with information.                   |
| [`extract`](https://hzambran.github.io/hydroTSM/reference/extract.md) Extract a subset of a zoo object.    | [`hydropairs`](https://hzambran.github.io/hydroTSM/reference/hydropairs.md) Visual correlation matrix. |
| [`infillxy`](https://hzambran.github.io/hydroTSM/reference/infillxy.md) Infills NA values.                 | [`istdx`](https://hzambran.github.io/hydroTSM/reference/istdx.md) Inverse standarization.              |
| [`ma`](https://hzambran.github.io/hydroTSM/reference/ma.md) Moving average.                                | [`matrixplot`](https://hzambran.github.io/hydroTSM/reference/matrixplot.md) 2D color matrix.           |
| [`rm1stchar`](https://hzambran.github.io/hydroTSM/reference/rm1stchar.md) Remove first character.          | [`sfreq`](https://hzambran.github.io/hydroTSM/reference/sfreq.md) Sampling frequency.                  |
| [`smry`](https://hzambran.github.io/hydroTSM/reference/smry.md) Improved summary function.                 | [`stdx`](https://hzambran.github.io/hydroTSM/reference/stdx.md) Standarization.                        |
| —————————————————————————————————————————                                                                  |                                                                                                        |

## Author

Mauricio Zambrano-Bigiarini

Maintainer: Mauricio Zambrano-Bigiarini \<mzb.devel@gmail\>

## See also

<https://github.com/hzambran/hydroGOF>.  
<https://github.com/hzambran/hydroPSO>.  

## Examples

``` r
## Loading the monthly time series (10 years) of precipitation for the Ebro River basin.
data(EbroPPtsMonthly)

#######
## Ex1) Graphical correlation among the ts of monthly precipitation of the first 
##      3 stations in 'EbroPPtsMonthly' (its first column stores the dates).
hydropairs(EbroPPtsMonthly[,2:4])
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter
#> Warning: argument 1 does not name a graphical parameter


#######
## Ex2) Annual values of precipitation at the station "P9001"
sname2ts(EbroPPtsMonthly, sname="P9001", dates=1, var.type="Precipitation", 
         tstep.out="annual")
#>   1941   1942   1943   1944   1945   1946   1947   1948   1949   1950 
#> 1094.7 1059.5  978.2  936.4  848.1  700.7  869.7  571.7  762.7  807.1 

#######
## Ex3) Monthly and annual plots
sname2plot(EbroPPtsMonthly, sname="P9001", var.type="Precipitation", pfreq="ma")



#######
## Ex5)  Mean monthly values of streamflows
## Loading daily streamflows (3 years) at the station 
## Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
monthlyfunction(OcaEnOnaQts, FUN=mean, na.rm=TRUE)
#>       Jan       Feb       Mar       Apr       May       Jun       Jul       Aug 
#> 12.881613  9.035119  9.015484  7.243778  4.982366  3.907444  2.448387  1.529355 
#>       Sep       Oct       Nov       Dec 
#>  1.909556  1.860215  5.932556  6.895591 
```
