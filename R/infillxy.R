# File infillxy.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                                'infillxy'                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Fills in ALL the 'NA' values in 'x' with the corrensponding values in 'sim'. #
# Started: 16-Dic-2008                                                         #
# Updates: 04-Sep-2009                                                         #
################################################################################
#
# 'x'   : 'data.frame' or 'matrix' in which some (observed) values are 'NA'
# 'sim' : 'data.frame' or 'matrix', with the same dimension of 'obs',
#          that contains the values that will be used for filling in
#          the 'NA' values in 'obs'
# Result: a 'data.frame' or 'matrix', with the same dimension of 'obs',
#         without 'NA' values.

infillxy <-function(x,...) UseMethod("infillxy")

infillxy.default <- function(x, sim,...) {

  if (length(x) != length(sim))
    stop("'x' and 'sim' does not have the same dimension !!")

  # vector with the index of all the elements in 'x' that are 'NA'
  na.index <- which( is.na(x) )

  # Replacing the 'NA' values in 'filled' by the correponding values in 'sim'
  x[na.index] <- sim[na.index]

  return(x)

} # 'infillxy.default' END


infillxy.matrix <- function(x, sim, ...) {

    if ( !identical(dim(x), dim(sim) ) )
      stop("'x' and 'sim' does not have the same dimension !!")

    if (is.na(match(class(sim), c("matrix"))))
        stop("Invalid argument: 'sim' must be of class 'matrix'")

    # Creating a copy of the original observed values
	z <- x

	z[,1:ncol(z)] <- sapply(1:ncol(z), function(j,y) {

		# Putting the monthly values in the output data.frame
		# The first column of 'x' corresponds to the Year
		z[,j] <- infillxy.default(x= y[,j], sim=sim[, j])

	}, y = x) # sapply END

 return(z)

} # 'infillxy.matrix' END


infillxy.data.frame <- function(x, sim, ...) {

    x   <- as.matrix(x)
    sim <- as.matrix(sim)

    NextMethod("infillxy.matrix")

} # 'infillxy.data.frame' END
