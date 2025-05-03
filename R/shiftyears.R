# File shiftyears.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2023-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# .shiftyears: Shifting years backwards for correct computation of annual      # 
#              values when the hydrological year does not start in 1           #
################################################################################
# This function shift backwards a vector of dates/times (Date/POSIXct/POSIXlt),#
# by 'lstart.month' (number representing the starting month to be used in the  #
# computation of annual values, by default 1:Jan)                              #
################################################################################
# This is an internal function, just developed to be used by some function that#
# needs to compute annual values with a year starting in a month different     #
# from January.                                                                #
################################################################################
# Output: numeric vector representing the year corresponing to each element of 
#         'ltime', considering 'lstart.month' as the starting month for annual
#         computations
shiftyears <- function(ltime,       # vector with the date/times of each element 
                                    # of a zoo object
                       lstart.month # numeric in [1,..,12], representing the 
                                    # starting month to be used in the  
                                    # computation of annual values.
                                    # By default 1 (Jan)
                       ) {
  syears.bak        <- as.numeric(format( ltime, "%Y" ))
  syears            <- syears.bak
  smonths           <- as.numeric(format( ltime, "%m"))
  months2moveback   <- 1:(lstart.month-1)
  N                 <- length(months2moveback)
  for (i in 1:N) {
    m.index          <- which(smonths == months2moveback[i])
    syears[m.index]  <- syears[m.index] - 1
  } # FOR end

  return(syears)
} # 'shiftyears' END