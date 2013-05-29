# File monthly2annual.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                               monthly2annual                                 #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-May-2009                                                         #
################################################################################
# This function transform a MONTHLY regular time series into an ANNUAL one .
# It is only a wrapper to 'daily2annual' and it was not merged as unique function just for clarity

# 'x'      : Monthly zoo object that will be converted into annual ones.
#            class(x) must be 'zoo'
# 'FUN'    : Function that have to be applied for transforming from daily to monthly time step
#            For precipitation FUN MUST be "sum"
#            For temperature and flow time series, FUN MUST be "mean"
# 'na.rm'  : Logical. Should missing values be removed?
#            TRUE : the monthly and annual values  are computed considering only those values different from NA
#            FALSE: if there is AT LEAST one NA within a year, the monthly and annual values are NA
# 'out.fmt': date format for the output time series. Possible values are:
#            -) "%Y"      : only the year will be used for the time. Default option. (e.g., "1961" "1962"...)
#            -) "%Y-%m-%d": a complete date format will be used for the time. Default option. (e.g., "1961" "1962"...)

monthly2annual <-function(x,...) UseMethod("daily2annual")

#monthly2annual.default <- function(x, FUN, na.rm=TRUE, out.fmt="%Y",...) {


#daily2annual.default(x, FUN, na.rm=na.rm, out.fmt=out.fmt )

#} # 'monthly2annual.default' end


#monthly2annual.data.frame <- function(x, FUN, na.rm=TRUE, out.fmt="%Y",
                                      #dates, date.fmt="%Y-%m-%d",
				      #out.type="data.frame",
				      #verbose=TRUE,...) {

#daily2annual.data.frame(x, FUN, na.rm=na.rm, out.fmt=out.fmt,
                          #dates=dates, date.fmt=date.fmt, out.type=out.type,
                          #verbose=verbose )

#} # 'monthly2annual.data.frame' END
