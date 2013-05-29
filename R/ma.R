# File ma.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                               'ma'                                           #
################################################################################
# Generic Moving (sliding) Average function                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008
# Updates: 
################################################################################
ma <-function(x, ...) UseMethod("ma")


################################################################################
# Default Moving (sliding) Average function                                    #
################################################################################
# A vector with the moving average computed using a window of 'win.len' elements

# x      : time series with n elements
# win.len: number of terms that will be considered in the mean. It have to be odd

# result : a vector with the moving average termns. The length of the resulting vector
#          is the same of 'x', but the first and last (win.len-1)/2 elements will
#          be NA.

ma.default <- function (x, win.len, FUN=mean,...) {

  if (ceiling(win.len)/2 != win.len/2)
      stop("Invalid argument: 'win.len' must be of odd")

  return( filter(x, rep(1/win.len, win.len), method="convolution", sides=2) )

} # 'ma.default' end


################################################################################
# 	ma.zoo:	Moving Average of a DAILY regular time series,                 #
#           by default using a window width =365 (Annual Average)              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates: 29-May-2013                                                         #
################################################################################

# 'x'       : zoo variable
# 'win.len' : window width. It have to be odd
# 'FUN'     : Function that have to be applied for computing the moving average. Usually, FUN MUST be "mean"

# 'result'  : a vector with the moving average termns. The length of the resulting vector
#             is less than the length of 'x', with (win.len-1)/2 missing elements at the begining and end of 'x'
ma.zoo <- function(x, win.len, FUN=mean,... ) {

  if (ceiling(win.len)/2 != win.len/2)
      stop("Invalid argument: 'win.len' must be of odd")

  # Generating an Moving Average time series, with a window width win.len1
  return ( rollapply(x, width=win.len, FUN, by.colum=FALSE) ) # zoo::rollapply

} # 'ma.zoo' end
