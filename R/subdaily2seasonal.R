# File subdaily2seasonal.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# subdaily2seasonal: Generic function for computing seasonal values for every  #
#                    year of a sub-daily xts object                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 05-Abr-2013                                                         #
# Updates: 08-Apr-2013                                                         #
################################################################################

# 'x   '    : variable of type 'xts' or 'data.frame'
# 'season'  : character, indicating the weather season to be used for selecting the data
#             Valid values are:
#             -) "DJF": December, January, February
#             -) "MAM": March, April, May
#             -) "JJA": June, July, August
#             -) "SON": September, October, November
# 'FUN'      : Function that will be applied to ALL the values of 'x' belonging to the given weather season
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the seasonal values  are computed considering only those values different from NA
#              FALSE: if there is AT LEAST one NA within a season, the corresponding values are NA
# 'out.fmt'  : character indicating the format for the output time series. Possible values are:
#              -) "%Y"      : only the year will be used for the time. Default option. (e.g., "1961" "1962"...)
#              -) "%Y-%m-%d": a complete date format will be used for the time. Default option. (e.g., "1961" "1962"...)

subdaily2seasonal <-function(x, ...) UseMethod("dm2seasonal")
