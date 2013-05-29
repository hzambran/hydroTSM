# File rm1stchar.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'rm1stchar' : deletes the first n characther(s) of 'x'                       #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 01-Oct-2010                                                         #
################################################################################

# 'x'   : Character, e.g, each element may represent the name of a single gauging station
# 'n'   : numeric, indicating the number of characters that have to be removed from the beginning of 'x'
rm1stchar <- function(x, n=1) {

   if (n<0) stop("'n' must be a positive integer")

  L <- length(x)

  start.col <- n + 1

  x[start.col:length(x)] <- sapply(start.col:length(x), function(j,x) {
                      x[j] <- substr(x[j], start=start.col, stop=nchar(x[j]))
                  }, x = x)

  return(x)

} # 'rm1stchar' END
