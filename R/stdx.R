################################################################################
#                                    'stdx'                                    #
################################################################################
# This function standarizes a vector or matrix, i.e., scales all the values in #
# a way that the transformed values will be within the range [0,1].            #
# z = scale(x) = [ x - xmin ] / [ xmax - xmin ]                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 19-Feb-2009                                                         #
################################################################################

# 'x'     : vector or matrix to be scaled
# 'result': standarized 'x', where all the values of each column of 'x'
#           are within the range [0,1]

# If you are (very likely) interested in Back transforming this standarized
# values into the original ranges, you sould keep the following values:
#        xmin   <-  apply(x,2,min, na.rm=TRUE)
#        xrange <-  apply(x,2,range, na.rm=TRUE)
#        xrange <-  apply(xrange,2, diff, na.rm=TRUE)

stdx <-function(x,...) UseMethod("stdx")

stdx.default <- function (x,...) {

     if (is.na(match(class(x), c("ts", "zoo") ) ) ) x <- as.numeric(x)

     # range of 'x', i.e., r = xmax - xmin
     r <- diff( range(x, na.rm=TRUE) )

     if ( r ==0 ) { std <- x

     } else { std <- scale(x, center=min(x, na.rm=TRUE), scale= r) }

     names(std) <- names(x)

     return( as.numeric(std) )

  } # 'stdx.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 19-Feb-2009                                                         #
################################################################################
stdx.matrix <- function(x,...) {

  std <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))

  std <- sapply(1:ncol(x), function(i) {
		 std[,i] <- stdx.default( x[,i] )
		 })

  colnames(std) <- colnames(x)
  rownames(std) <- rownames(x)

  return(std)

}  # 'stdx.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 19-Feb-2009                                                         #
################################################################################
stdx.data.frame <- function(x,...) {

  x <- as.matrix(x)
  NextMethod("stdx.matrix")

}  # 'stdx.data.frame' end
