# File istdx.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                                  'istdx'                                     #
################################################################################
# This function transforms back a standarized vector/matrix into their original#
# values, i.e., re-scales all the values in the [0,1] interval to the original #
# range of values                                                              #
#           x = re-scale(z) = z*[ zmax - zmin ] + xmin                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 19-Feb-2009                                                         #
################################################################################
#

# 'z'     : standarized vector or matriz to be re-scaled, all the values
#           have to be in the range [0,1]
# 'xmin'  : numeric with the minimum value(s) in the original 'x'
#           -) if 'z' is a vector, 'xmin' has to be a real
#           -) if 'z' is a matrix/data.frame, 'xmin' has to be a vector, with
#              the minimum values for each column of the original 'x'
#              in this case, the vector of minimums can be otained as:
#               xmin <-  apply(x,2,min, na.rm=TRUE)
# 'xrange'  : numeric with the range of value(s) in the original 'x'
#           -) if 'z' is a vector, 'xrange' has to be a real
#           -) if 'z' is a matrix/data.frame, 'xrange' has to be a vector, with
#              the range of values for each column of the original 'x'
#              in this case, the vector of ranges can be otained as:
#               xrange <-  apply(x, 2, range, na.rm=TRUE)
#               xrange <-  apply(xrange,2, diff, na.rm=TRUE)
# 'result': re-scaled 'x', where all the values of each column of 'x'
#           are within the original range of x values

istdx <-function(x,...) UseMethod("istdx")

istdx.default <- function(x, xmin, xrange,...) {

   z <- x*xrange + xmin

   names(z) <- names(x)

   return(as.numeric(z))

} # 'istdx.default' end


istdx.matrix <- function(x, xmin, xrange,...) {

  z <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))

  z <- sapply(1:ncol(z), function(i) {
		 z[,i] <- istdx.default( x[,i], xmin[i], xrange[i] )
		 })

  colnames(z) <- colnames(x)
  rownames(z) <- rownames(x)

  return(z)

}  # 'istdx.matrix' end


istdx.data.frame <- function(x, xmin, xrange,...) {

  x <- as.matrix(x)
  NextMethod("istdx.matrix")

}  # 'istdx.data.frame' end
