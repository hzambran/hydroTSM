# File baseflow.R
# File plot_p_q.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
#                                 http://www.rforge.net/hydroTSM/ 
# Copyright 2012-2018 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                               'baseflow'                                     #  
################################################################################       
# Purpose: Given a complete (without missing values) series of streamflow,     #
#          this function computes the baseflow using the filter proposed by    # 
#          Arnold & Allen in 1999                                              #
################################################################################
# Reference: Arnold, J. G., & Allen, P. M. (1999). Automated methods for       #
#            estimating baseflow and ground water recharge from streamflow     #
#            records. JAWRA Journal of the American Water Resources Association, 
#            35(2), 411-424. doi: 10.1111/j.1752-1688.1999.tb03599.x           #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 2013                                                                #
# Updates: 16-May-2018                                                         #
################################################################################

# 'x'       : zoo or numeric object with streamflow records. The suggested time 
#             frequency should be hourly or daily, but the algorithm will work  
#             with any time frequency
# 'beta'    : numeric representing the filter parameter. Default value is 0.925  
#             as recommended by Arnold & Allen (1999)
# 'out.type': Character indicating the type of result that is given by this function.
#             Valid values are:
#             -) "last" => only the baseflow computed after the third pass 
#                          of the filter is returned
#             -) "all " => the 3 baseflows computed after each pass of the 
#                          filter are returned in a matrix or zoo object
# 'na.fill' : Character indicating how to fill any NA present in 'x'.
#             Valid values are:
#             -) "none"   => NAs are not removed, and therefore the algorithm is 
#                            not executed
#             -) "linear" => NAs are removed by linear interpolation, using 
#                            \code{\link[zoo]{na.approx}}
#             -) "spline" => NAs are removed by spline interpolation, using 
#                            \code{\link[zoo]{na.spline}}
# 'plot'    : logical. Indicates if the baseflow should be plotted or not. 
#             If plotted, the original 'x' values area plotted as well
# 'xcol'    : character, representing the color to be used for ploting the 
#             streamflow time series
# 'bfcol'   : character of lenght 3, representing the color(s) to be used for 
#             ploting the baseflow time series. The first, second and third 
#             element are used to represent the baseflow after the third, 
#             second and first pass of the filter, respectively.

baseflow <- function(x, 
                     beta=0.925, 
                     out.type=c("last", "all"), 
                     na.fill=c("none", "linear", "spline"), 
                     plot=FALSE, 
                     xcol="black", 
                     bfcol=c("blue", "darkcyan", "darkorange3")) {

  # Checking 'out.type'
  out.type <- match.arg(out.type)

  # Checking 'na.fill'
  na.fill <- match.arg(na.fill)

  # Chaecking that 'x' does not have any missing value
  na.index <- which(is.na(x))
  if (length(na.index) > 0) {
    if (na.fill == "none") {
      stop("Invalid argument: 'x' has some NA values !!")
    } else if (na.fill == "linear") {
        x <- zoo::na.approx(x)
      } else x <- zoo::na.spline(x)
  } # IF end

  # Storing and  then removing the possible time attribute
  HasTime <- FALSE
  if ( is.zoo(x) ) {
    print("hola2")
    HasTime <- TRUE
    xtime   <- time(x)
    x       <- coredata(x)
  } # if END

  # Initializing some parameters of the algorithm
  print("hola")
  print(x)
  n  <- length(x)
  f1 <- beta
  f2 <- (1+beta) / 2

  # Initializing quickflow values 
  quickflow    <- rep(NA, n)
  quickflow[1] <- x[1] / 2

  # Initializing baseflow values after the first ('baseflow1'), 
  # second ('baseflow2') and third ('baseflow3') pass of the filter
  baseflow1 <- rep(NA, n)
  baseflow2 <- rep(NA, n)
  baseflow3 <- rep(NA, n)

  # 1) First pass of the filter (forward)
  for (i in 2:n) {
    quickflow[i] <- f1 * quickflow[i-1] + f2 * ( x[i] - x[i-1] )
    if ( quickflow[i] < 0) quickflow[i] <- 0
  } # FOR end

  # baseflow1 computation
  baseflow1  <- x - quickflow
  # Removing any possible negative values
  neg.index <- which(baseflow1 < 0)
  if (length(neg.index) > 0) baseflow1[neg.index] <- 0
  # Removing any possible baseflow1 values larger than its corresponding streamflow
  larger.index <- which(baseflow1 > x)
  if (length(larger.index) > 0) baseflow1[larger.index] <- x[larger.index]


  # 2) Second pass of the filter (backward)
  baseflow2[n-1] <- baseflow1[n-1]
  for (i in (n-2):1) {
    quickflow[i] <- f1 * quickflow[i+1] + f2 * ( baseflow1[i] - baseflow1[i+1] )
    if ( quickflow[i] < 0) quickflow[i] <- 0
  } # FOR end

  # baseflow2 computation
  baseflow2  <- baseflow1 - quickflow
  # Removing any possible negative values
  neg.index <- which(baseflow2 < 0)
  if (length(neg.index) > 0) baseflow2[neg.index] <- 0
  # Removing any possible baseflow2 values larger than its previous baseflow1
  larger.index <- which(baseflow2 > baseflow1)
  if (length(larger.index) > 0) baseflow2[larger.index] <- baseflow1[larger.index]


  # 3) Third pass of the filter (forward)
  baseflow3[n-1] <- baseflow1[n-1]
  for (i in 2:n) {
    quickflow[i] <- f1 * quickflow[i-1] + f2 * ( baseflow2[i] - baseflow2[i-1] )
    if ( quickflow[i] < 0) quickflow[i] <- 0
  } # FOR end

  # baseflow3 computation
  baseflow3  <- baseflow2 - quickflow
  # Removing any possible negative values
  neg.index <- which(baseflow3 < 0)
  if (length(neg.index) > 0) baseflow3[neg.index] <- 0
  # Removing any possible baseflow3 values larger than its previous baseflow2
  larger.index <- which(baseflow3 > baseflow2)
  if (length(larger.index) > 0) baseflow3[larger.index] <- baseflow2[larger.index]

  # Giving back the time attribute if the orignal 'x' object had one
  if ( HasTime ) {
    x         <- zoo(x, xtime)
    baseflow1 <- zoo(baseflow1, xtime)
    baseflow2 <- zoo(baseflow2, xtime)
    baseflow3 <- zoo(baseflow3, xtime)
  } # if END

  # Creating the output object
  if (out.type=="last") {
    out <- baseflow3
  } else out <- cbind(baseflow3, baseflow2, baseflow1)


  # Plotting, if necessary
  if (plot) {
    if (HasTime) {
      plot(x, ylab="Q", type="n", xaxt="n", xlab="Time")
      drawTimeAxis(x)
      grid()
      points(x, type="o", col=xcol, pch=15, lty=1, cex=0.5)
      if (out.type=="all") {
        points(baseflow1, type="o", col=bfcol[3], pch=15, lty=1, cex=0.5)
        points(baseflow2, type="o", col=bfcol[2], pch=15, lty=1, cex=0.5)

        legend.text <- c("Streamflow", "Baseflow 3rd pass", "Baseflow 2nd pass", "Baseflow 1st pass")
        legend.cols <- c(xcol, bfcol[1], bfcol[2], bfcol[3])
      } else {
          legend.text <- c("Streamflow", "Baseflow")
          legend.cols <- c(xcol, bfcol[1])
        } # ELSE end
        points(baseflow3, type="o", col=bfcol[1], pch=15, lty=1, cex=0.5)
    } else { # HasTime == FALSE
        plot(1:n, x, ylab="Q", type="n", xlab="Time")
        grid()
        points(1:n, x, type="o", col=xcol, pch=15, lty=1, cex=0.5)
        if (out.type=="all") {
          points(1:n, baseflow1, type="o", col=bfcol[3], pch=15, lty=1, cex=0.5)
          points(1:n, baseflow2, type="o", col=bfcol[2], pch=15, lty=1, cex=0.5)

          legend.text <- c("Streamflow", "Baseflow 3rd pass", "Baseflow 2nd pass", "Baseflow 1st pass")
          legend.cols <- c(xcol, bfcol[1], bfcol[2], bfcol[3])
        } else {
            legend.text <- c("Streamflow", "Baseflow")
            legend.cols <- c(xcol, bfcol[1])
          } # ELSE end
          points(1:n, baseflow3, type="o", col=bfcol[1], pch=15, lty=1, cex=0.5)
      } # ELSE end

    legend("topright", legend = legend.text, bty="n",
           lty=c(1,1), pch=c(15, 15), col=legend.cols,
           #lty = 1:2, xjust = 1, yjust = 1,
           title = "")
  } # IF end

  return(out)

} # 'baseflow' END

#setwd("/dataMZB/2018-UFRO/work/Investigacion/Colaboracion_de_estudiantes/Cristobal_Soto/data/ts")
#library(zoo)
#x <- read.csv(file="Caudales_Curacautin_en_Rari_Ruca.csv")
#dates <- as.Date(x[,1], format="%d-%m-%Y")
#q <- x[1:3597, 2]
#dates <- dates[1:3597]
#q.na.filled <- na.approx(q)
#bf <- baseflow(q.na.filled)
#bf <- zoo(bf, dates)


