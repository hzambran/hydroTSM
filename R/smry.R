####################################################################
# Smry: Min, 1stQ, Mean, Median, 3rdQ, Max, IQR, sd, cv, skewness, #
#       kurtosis, amount of elements and amount of NA's            #
#       For numerical variables.                                   #
#       Skewness and Kurtosis are computed with the e1071 package  #
####################################################################
#	Date: 14-Jun-2008; 11-Sep-2009                                 #
####################################################################
smry <-function(x, ...) UseMethod("smry")


smry.default <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    if ( class(x) %in% c("ts", "zoo") ) {
        x <- as.numeric(x)
    } # IF end


    # Creating the resulting object
    z <- matrix(NA, ncol=1, nrow=13)

    # If 'x' is not numeric but Date
    if ( class(x) == "Date" ) {
      z <- as.data.frame(z)
    } # IF end


    if ( class(x) %in% c("numeric", "integer", "Date") ) {

        n <- length(x)

        na.index <- which(is.na(x))
        nna      <- length(na.index)

        if (na.rm) {
          if (nna > 0) {
            x  <- x[-na.index] }
        } # IF end

        s    <- summary(x, ..., digits=digits)

        if ( class(x) %in% c("numeric", "integer") ) {

            z[1,1] <- s[1] # min
            z[2,1] <- s[2] # q1
            z[3,1] <- s[3] # median
            z[4,1] <- s[4] # mean
            z[5,1] <- s[5] # q3
            z[6,1] <- s[6] # max

            z[7,1] <- IQR(x, na.rm = na.rm)	                             # Interquantile Range IQR = Q(0.75) – Q(0.25)
            z[8,1] <- sd(x, na.rm = na.rm)	                             # Standard Deviation
            z[9,1] <- sd(x, na.rm = na.rm) / abs(mean(x, na.rm = na.rm)) # Coefficient of variation ( coef. of variation = sd / |mean| )

            #require(e1071) # for the following 2 functions
            z[10,1] <- e1071::skewness(x, na.rm = na.rm)  # Skewness (using  e1071 package)
            z[11,1] <- e1071::kurtosis(x, na.rm = na.rm)  # Kurtosis (using  e1071 package)

            z <- round( z, digits)

        } else { # if 'x' is a Date object

            z[1,1] <- as.character(s[1]) # min
            z[2,1] <- as.character(s[2]) # q1
            z[3,1] <- as.character(s[3]) # median
            z[4,1] <- as.character(s[4]) # mean
            z[5,1] <- as.character(s[5]) # q3
            z[6,1] <- as.character(s[6]) # max

            z[7,1]  <- NA # IQR
            z[8,1]  <- NA # sd
            z[9,1]  <- NA # cv
            z[10,1] <- NA # sk
            z[11,1] <- NA # kur

          } # ELSE end

        z[12,1] <- nna # Amount of NA's
        z[13,1] <- n   # Number of elements

    } # IF end

    row.names(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                      "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                      "NA's", "n")

    return(z)

} # 'smry.default' end


smry.data.frame <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    # Creating a copy of the original observed values
	z <- as.data.frame( matrix(NA, nrow=13, ncol=ncol(x)) )

	z[,1:ncol(z)] <- sapply(1:ncol(x), function(j,y) {

		# Putting the monthly values in the output data.frame
		# The first column of 'x' corresponds to the Year
		z[,j] <- smry.default(x= y[,j], na.rm=na.rm, digits=digits)

	}, y = x) # sapply END


    rownames(z) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                     "Max.", "IQR", "sd", "cv", "Skewness", "Kurtosis",
                     "NA's", "n")

    colnames(z) <- colnames(x)

    return(z)

} # 'smry.data.frame' end




smry.matrix <- function(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)  {

    x <- as.data.frame(x)
    smry.data.frame(x, na.rm=na.rm, digits=digits,...)
    #NextMethod("smry", x, na.rm=TRUE, digits=digits,...)

} # 'smry.data.frame' end
