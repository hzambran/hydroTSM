# San Martino, ts of daily precipitation.

Daily time series of precipitation, maximum and minimun air temperature
at station Maquehue Temuco Ad. (DMC_ID:380013), Araucania Region, Chile
(Lat:-38.770, Lon:-72.637), with data from 01/Jan/1950 to 31/Dec/2015
(including some gaps).

## Usage

``` r
data(MaquehueTemuco)
```

## Format

zoo matrix with 3 columns:  
-) `pcp`: daily precipitation, \[mm/day\].  
-) `tmx`: daily maximum air temperature, \[degree Celsius\].  
-) `tmn`: daily minimum air temperature, \[degree Celsius\].  

## Source

Provided by Center for Climate and Resilience Research, Universidad de
Chile, Santiago, Chile (CR2,
<https://dataclima.cr2.cl/bases_de_datos?database_db_type_id=5>.
Original data available at the Chilean Meteorological Organization (DMC,
<https://climatologia.meteochile.gob.cl/application/diariob/visorDeDatosEma/380013>).
Last accessed \[May 2025\]).  
These data are intended to be used for research purposes only, being
distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY.
