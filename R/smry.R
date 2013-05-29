# File smry.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                                 'smry'                                       # 
################################################################################
# Min, 1stQ, Mean, Median, 3rdQ, Max, IQR, sd, cv, skewness, kurtosis, amount  #
# of elements and amount of NA's                                               #
#       For numerical variables.                                               #
#       Skewness and Kurtosis are computed with the e1071 package              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 14-Jun-2008;                                                        #
# Updates: 11-Sep-2009 ; 30-Aug-2011  ; 14-Sep-2011 ; 15-Sep-2011              #
#          29-May-2013                                                         #
################################################################################
smry <-function(x, ...) UseMethod("smry")


smry.default <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    if ( class(x) %in% c("zoo", "xts") ) {
        x <- coredata(x) # zoo::coredata
    } # IF end

    xname <- deparse(substitute(x))
    
    # Creating the resulting object
    z <- as.data.frame(matrix(NA, ncol=1, nrow=13))  

    n <- length(x)

    nas <- is.na(x)
    nna <- sum(nas)

    if (na.rm) {
          if (nna > 0) x  <- x[!nas]
    } # IF end
        
    s    <- summary(x, ..., digits=digits)

    if ( class(x) %in% c("numeric", "integer") ) {
        
        # min, q1, median, mean, q3, max
        z[1:6, 1] <- s

        z[7,1] <- IQR(x, na.rm = na.rm)	                         # Interquantile Range IQR = Q(0.75) – Q(0.25)
        z[8,1] <- sd(x, na.rm = na.rm)	                         # Standard Deviation
        z[9,1] <- sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm)) # Coefficient of variation ( coef. of variation = sd / |mean| )

        #require(e1071) # for the following 2 functions
        z[10,1] <- e1071::skewness(x, na.rm = na.rm)  # Skewness (using  e1071 package)
        z[11,1] <- e1071::kurtosis(x, na.rm = na.rm)  # Kurtosis (using  e1071 package)

        z <- round( z, digits)
            
        z[12,1] <- nna # Amount of NA's
        z[13,1] <- n   # Number of elements


    } else if ( class(x) == "Date" ) { 
              z <- smry.Date( x, na.rm=na.rm, digits=digits, ... ) 
           } else z[1:13, 1] <- NA

    row.names(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                      "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                      "NA's", "n")

    names(z) <- xname
    return(z)

} # 'smry.default' end


smry.Date <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

        # Creating the resulting object
        z <- as.data.frame(matrix(NA, ncol=1, nrow=13))

        n <- length(x)

        nas <- is.na(x)
        nna <- sum(nas)

        if (na.rm) {
          if (nna > 0) {
            x  <- x[!nas] }
        } # IF end

        s <- summary(x, ..., digits=digits)

        # min, q1, median, mean, q3, max            
        z[1:6, 1] <- as.character(s)

        z[12, 1] <- nna # Amount of NA's
        z[13, 1] <- n   # Number of elements

 
    row.names(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                      "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                      "NA's", "n")

    names(z) <- deparse(substitute(x))
    return(z)

} # 'smry.Date' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 14-Jun-2008;                                                        #
# Updates: 11-Sep-2009 ; 30-Aug-2011  ; 14-Sep-2011                            #
################################################################################
smry.data.frame <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    # Creating a copy of the original observed values
    z <- as.data.frame( matrix(NA, nrow=13, ncol=ncol(x)) )

    z[,1:ncol(z)] <- sapply(1:ncol(x), function(j,y) {

       z[,j] <- smry.default(x= y[,j], na.rm=na.rm, digits=digits, ...)

    }, y = x) # sapply END


    rownames(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                     "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                     "NA's", "n")
    colnames(z) <- colnames(x)

    return(z)

} # 'smry.data.frame' end



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 14-Jun-2008;                                                        #
# Updates: 11-Sep-2009                                                         #
################################################################################
smry.matrix <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    x <- as.data.frame(x)
    smry.data.frame(x, na.rm=na.rm, digits=digits,...)
    #NextMethod("smry", x, na.rm=TRUE, digits=digits,...)

} # 'smry.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 30-Aug-2011                                                         #
# Updates: 29-May-2013                                                         #
################################################################################
smry.zoo <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    dates <- time(x)  
    z     <- coredata(x) # zoo::coredata
    
    # Giving meaningful names to the output
    if ( (is.matrix(x)) | (is.data.frame(x)) ) {
        zname <- colnames(z)
        z     <- apply(z, MARGIN=2, FUN=smry.default, na.rm=na.rm, digits=digits, ...)      
    } else {
        zname <- deparse(substitute(x))
        z     <- smry.default(z, na.rm=na.rm, digits=digits,...) 
      } # ELSE end    
    
    z  <- data.frame(Index=c(summary(dates), rep(NA,7)), Data=z)
    
    # Giving meaningful names to columns and rows in 'z'
    colnames(z) <- c("Index", zname) 
    rownames(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                     "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                     "NA's", "n")
    return(z)

} # 'smry.zoo' end
