# File sfreq.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# sfreq: Sampling frequency of a ts/zoo object                                 #
################################################################################
# This function generates a table indicating the number of days                #
# with information (<>NA's) within a data.frame                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-May-2009                                                         #
# Updates: Mar 2009                                                            #
#          Nov 2010                                                            #
#          Apr 2011 ; 09-Aug-2011                                              #
#          18-Oct-2012                                                         #
#          29-May-2013                                                         #
################################################################################
sfreq <- function(x, min.year=1800) {

  # Checking that 'class(x)'
  valid.class <- c("xts", "zoo")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0) 
     stop("Invalid argument: 'x' must be in c('xts', 'zoo')" )
     
   out <- periodicity(x)$scale # xts::periodicity
   
   if (out == "yearly") out <- "annual"

  return(out)

} # 'sfreq' END
