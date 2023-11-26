%%File Cauquenes7336001.Rd
%% Part of the hydroPSO R package, https://github.com/hzambran/hydroTSM
%%                                 http://cran.r-project.org/web/packages/hydroTSM
%%                                 http://www.rforge.net/hydroTSM/
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{Cauquenes7336001}
\docType{data}
\alias{Cauquenes7336001}
\title{
Hydrometeorological time series for "Cauquenes en El Arrayán" catchment
}
\description{
Daily time series of precipitation, maximum air temperature, minimum air temperature, potential evapotranspiration, and observed streamflows for the catchment draining into the 'Cauquenes en El Arrayán' streamflow station (Cod.BNA: 7336001, Lat:-36.02, Lon:-72.38). This catchment is located in El Maule Region in Chile, with a pluvial regime, a total drainage area of 622.1 km2, and elevations ranging from 134 to 736 m a.s.l. Data were downloaded from the CAMELS-CL dataset (\url{https://camels.cr2.cl}) from 01/Jan/1979 to 31/Dec/2020 (including some missing values).
}
\usage{
data(Cauquenes7336001)
}

%%\details{
%%
%%}
\format{
zoo matrix with 5 time series: \cr
-) \var{P\_mm}: Spatially-averaged mean daily values of precipitation computed based on the CR2met dataset, [mm/day]. \cr
-) \var{Tmx\_degC}: Spatially-averaged maximum daily values of air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{Tmin\_degC}: Spatially-averaged minimum daily values of air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{PET_mm}: Spatially-averaged mean daily values of precipitation computed based on the Hargreaves-Samani equation and daily maximum and minimum air temperatures obtained from the CR2met dataset, [mm/day]. \cr 
-) \var{Qobs_mm}: Daily sreamflows, [mm], measured at the "Cauquenes en El Arrayán" (7336001) station. \cr 
-) \var{Qobs_m3s}: Daily sreamflows, [m3/s], measured at the "Cauquenes en El Arrayán" (7336001) station. \cr 
}
\source{
Provided by Center for Climate and Resilience Research, Universidad de Chile, Santiago, Chile (\url{https://camels.cr2.cl/}, last accessed [Nov 2023]). \cr
These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

\references{
 \cite{Alvarez-Garreton, C., Mendoza, P. A., Boisier, J. P., Addor, N., Galleguillos, M., Zambrano-Bigiarini, M., Lara, A., Puelma, C., Cortes, G., Garreaud, R., McPhee, J., and Ayala, A (2018). The CAMELS-CL dataset: catchment attributes and meteorology for large sample studies-Chile dataset. Hydrology and Earth System Sciences, 22(11), 5817-5846. doi:10.5194/hess-22-5817-2018.}
}
\keyword{datasets}
