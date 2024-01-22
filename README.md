# hydroTSM
[![Research software impact](http://depsy.org/api/package/cran/hydroTSM/badge.svg)](http://depsy.org/package/r/hydroTSM) [![CRAN](http://www.r-pkg.org/badges/version/hydroTSM)](https://cran.r-project.org/package=hydroTSM) [![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html) [![monthly](http://cranlogs.r-pkg.org/badges/hydroTSM)](https://www.rpackages.io/package/hydroTSM) [![total](http://cranlogs.r-pkg.org/badges/grand-total/hydroTSM)](https://www.rpackages.io/package/hydroTSM) [![Build Status](https://travis-ci.org/hzambran/hydroTSM.svg?branch=master)](https://travis-ci.org/hzambran/hydroTSM) [![dependencies](https://tinyverse.netlify.com/badge/hydroTSM)](https://CRAN.R-project.org/package=hydroTSM)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/hzambran/hydroTSM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hzambran/hydroTSM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

hydroTSM provides S3 functions for management, analysis and plotting of time series used in hydrology and related environmental sciences. In particular, this package is highly oriented to hydrological modelling tasks.

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

## Reporting bugs, requesting new features

If you find an error in some function, or want to report a typo in the documentation, or to request a new feature (and wish it be implemented :) you can do it [here](https://github.com/hzambran/hydroTSM/issues)


## Citation 
```{r}
citation("hydroTSM")
```

To cite hydroTSM in publications use:

> Zambrano-Bigiarini, Mauricio (2024). hydroTSM: Time Series Management and Analysis for Hydrological Modelling. R package version 0.7-0. URL:https://cran.r-project.org/package=hydroTSM. doi:10.5281/zenodo.839565.


A BibTeX entry for LaTeX users is

>  @Manual{hydroTSM,  
>    title = {hydroTSM: Time Series Management, Analysis and Interpolation for Hydrological Modelling},  
>    author = {Zambrano-Bigiarini, Mauricio},  
>    note = {R package version 0.7-0},  
>    year = {2024},
>    url = {https://cran.r-project.org/package=hydroTSM},  
>    doi = {doi:10.5281/zenodo.839565},  
>  }


## Vignettes
1. [Daily precipitation](https://cran.r-project.org/package=hydroTSM/vignettes/hydroTSM_Daily_P_Vignette-knitr.pdf). Here you can find an introductory vignette showing the use of several hydroTSM functions for analysing daily precipitation data.

2. [Daily streamflows](https://cran.r-project.org/package=hydroTSM/vignettes/hydroTSM_Daily_Q_Vignette-knitr.pdf). Here you can find an introductory vignette showing the use of several hydroTSM functions for analysing daily streamflow data.


## Related Material 

* *R: a statistical environment for hydrological analysis* (**EGU-2010**)  [abstract](http://meetingorganizer.copernicus.org/EGU2010/EGU2010-13008.pdf), [poster](http://www.slideshare.net/hzambran/egu2010-ra-statisticalenvironmentfordoinghydrologicalanalysis-9095709).

* *Using R for analysing spatio-temporal datasets: a satellite-based precipitation case study* (**EGU-2017**) [abstract](http://meetingorganizer.copernicus.org/EGU2017/EGU2017-18343.pdf), [poster](https://doi.org/10.5281/zenodo.570145).



## See Also 

* [hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series](https://cran.r-project.org/package=hydroGOF).

* [hydroPSO: Model-independent Particle Swarm Optimisation (PSO) for environmental/hydrological models](https://cran.r-project.org/package=hydroPSO).

* [RFmerge: Merging of Satellite Datasets with Ground Observations using Random Forests](https://cran.r-project.org/package=RFmerge).
