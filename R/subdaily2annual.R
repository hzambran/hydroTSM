# File subdaily2annual.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#          subdaily2annual                                                     #
################################################################################
# Generic function for transforming a DAILY (sub-daily, weekly, monthly or quarterly) 
# regular time series into an ANNUAL one

# 'x'      : zoo/xts object which values will be converted into annual ones
# 'FUN'    : Function that have to be applied for aggregating into Annual time step
#            For Precipitation FUN MUST be 'sum'
#            For Temperature and Flow time series, FUN MUST be 'mean'
# 'na.rm'  : TRUE : the annual mean  value is computed considering only those values different from NA
#            FALSE: if there is AT LEAST one NA within a year, the monthly mean value is NA
# 'out.fmt': character indicating the format for the output time series. Possible values are:
#            -) "%Y"      : only the year will be used for the time. Default option. (e.g., "1961" "1962"...)
#            -) "%Y-%m-%d": a complete date format will be used for the time. Default option. (e.g., "1961" "1962"...)

subdaily2annual <-function(x, ...) UseMethod("daily2annual")
