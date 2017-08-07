# hydroTSM
[![Research software impact](http://depsy.org/api/package/cran/hydroTSM/badge.svg)](http://depsy.org/package/r/hydroTSM) [![Build Status](https://travis-ci.org/hzambran/hydroTSM.svg?branch=master)](https://travis-ci.org/hzambran/hydroTSM)

hydroTSM provides S3 functions for management, analysis, interpolation and plotting of time series used in hydrology and related environmental sciences. In particular, this package is highly oriented to hydrological modelling tasks.

The focus of this package has been put in providing a collection of tools useful for the daily work of hydrologists (although an effort was made to optimize each function as much as possible, functionality has had priority over speed).

Bugs / comments / questions / collaboration of any kind are very welcomed, and in particular, datasets that can be included in this package for academic purposes.


## Installation
Installing the latest stable version from [CRAN](https://CRAN.R-project.org/package=hydroTSM):
```{r}
install.packages("hydroTSM")
```

Alternatively, you can also try the under-development version from [Github](https://github.com/hzambran/hydroTSM):
```{r}
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hzambran/hydroTSM")
```


## Citation 
```{r}
citation("hydroTSM")
```

To cite hydroTSM in publications use:

>  Mauricio Zambrano-Bigiarini. hydroTSM: Time Series Management, Analysis and Interpolation for Hydrological ModellingR package version 0.5-0. URL https://github.com/hzambran/hydroTSM. DOI:10.5281/zenodo.839566


A BibTeX entry for LaTeX users is

>  @Manual{hydroTSM,  
>    title = {hydroTSM: Time Series Management, Analysis and Interpolation for Hydrological Modelling},  
>    author = {{Mauricio Zambrano-Bigiarini}},  
>    note = {R package version 0.5-0},  
>    url = {https://github.com/hzambran/hydroTSM},  
>    note = {DOI:10.5281/zenodo.839566},  
>  }


## Vignette 
[Here](https://cran.r-project.org/web/packages/hydroTSM/vignettes/hydroTSM_Vignette.pdf) you can find an introductory vignette showing the use of several hydroTSM functions.



## Related Material 

* *R: a statistical environment for hydrological analysis* (**EGU-2010**)  [abstract](http://meetingorganizer.copernicus.org/EGU2010/EGU2010-13008.pdf), [poster](http://www.slideshare.net/hzambran/egu2010-ra-statisticalenvironmentfordoinghydrologicalanalysis-9095709).

* *Using R for analysing spatio-temporal datasets: a satellite-based precipitation case study* (**EGU-2017**) [abstract](http://meetingorganizer.copernicus.org/EGU2017/EGU2017-18343.pdf), [poster](https://doi.org/10.5281/zenodo.570145).



## See Also 

* [hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series](https://github.com/hzambran/hydroGOF).

* [hydroPSO: Model-independent Particle Swarm Optimisation (PSO) for environmental/hydrological models](https://github.com/hzambran/hydroPSO).


## Visitors 

Since version 0.5-1 (07 August 2017):


Copy

<a href="https://clustrmaps.com/site/19pyt" title="Visit tracker"><img src="//www.clustrmaps.com/map_v2.png?d=v1akGPl4UpLsxa2kycQ79LEAUb7gI_Cxwnf23kaCJsE&cl=ffffff"></a>



