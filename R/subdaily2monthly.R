# File subdaily2monthly.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          subdaily2monthly                                                    #
################################################################################
# This function transform a (sub)DAILY regular time series into a MONTHLY one

# 'x'   : daily values that will be converted into monthly ones.
#         class(x) must be zoo/xts
# 'FUN' : Function that have to be applied for transforming from daily into 
#         monthly time step
#         For precipitation FUN MUST be "sum"
#         For temperature and flow time series, FUN MUST be "mean"
# 'na.rm': Logical. Should missing values be removed?
#          TRUE : the monthly and annual values  are computed considering only those values different from NA
#          FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA

subdaily2monthly <-function(x, ...) UseMethod("daily2monthly")
