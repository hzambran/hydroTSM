# Hydrometeorological time series for "Cauquenes en El Arrayan" catchment

Daily time series of precipitation, maximum air temperature, minimum air
temperature, potential evapotranspiration, and observed streamflows for
the catchment draining into the 'Cauquenes en El Arrayan' streamflow
station (Cod.BNA: 7336001, Lat:-36.02, Lon:-72.38). This catchment is
located in El Maule Region in Chile, with a pluvial regime, a total
drainage area of 622.1 km2, and elevations ranging from 134 to 736 m
a.s.l. Data were downloaded from the CAMELS-CL dataset
(<https://camels.cr2.cl>) from 01/Jan/1979 to 31/Dec/2019 (including
some missing values).

## Usage

``` r
data(Cauquenes7336001)
```

## Format

zoo matrix with 5 time series:  
-) `P_mm`: Spatially-averaged mean daily values of precipitation
computed based on the CR2met dataset, \[mm/day\].  
-) `Tmx_degC`: Spatially-averaged maximum daily values of air
temperature computed based on the CR2met dataset, \[degree Celsius\].  
-) `Tmin_degC`: Spatially-averaged minimum daily values of air
temperature computed based on the CR2met dataset, \[degree Celsius\].  
-) `PET_mm`: Spatially-averaged mean daily values of precipitation
computed based on the Hargreaves-Samani equation and daily maximum and
minimum air temperatures obtained from the CR2met dataset, \[mm/day\].  
-) `Qobs_mm`: Daily sreamflows, \[mm\], measured at the "Cauquenes en El
Arrayan" (7336001) station.  
-) `Qobs_m3s`: Daily sreamflows, \[m3/s\], measured at the "Cauquenes en
El Arrayan" (7336001) station.  

## Source

Provided by Center for Climate and Resilience Research, Universidad de
Chile, Santiago, Chile (<https://camels.cr2.cl/>, last accessed \[Nov
2023\]).  
These data are intended to be used for research purposes only, being
distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY.

## References

Alvarez-Garreton, C., Mendoza, P. A., Boisier, J. P., Addor, N.,
Galleguillos, M., Zambrano-Bigiarini, M., Lara, A., Puelma, C., Cortes,
G., Garreaud, R., McPhee, J., and Ayala, A (2018). The CAMELS-CL
dataset: catchment attributes and meteorology for large sample
studies-Chile dataset. Hydrology and Earth System Sciences, 22(11),
5817-5846. doi:10.5194/hess-22-5817-2018.
