# File plot_p_q.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2012-2018 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                              'plot_p_q'                                      #  
################################################################################       
# Purpose: Given a time series of precipitation and streamflow, this function  #
#          plots the two time series, streamflows as a normal time series and  #
#          precipitation as bars comming from the upper part of the plotting   #
#          window                                                              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 09-Jun-2018                                                         #
# Updates:                                                                     #
################################################################################

# 'x'          : zoo object with streamflow records, with any time frequency 
# 'excprobs'   : numeric representing the exceedence probabilities used to compute 
#                the monthly values
# 'na.fill'    : Character indicating how to fill any NA present in 'x'.
#                Valid values are:
#                -) "remove" => NAs are removed from the computation of the 
#                               empirical probabilities
#                -) "linear" => NAs are removed by linear interpolation, using 
#                               \code{\link[zoo]{na.approx}}
#                -) "spline" => NAs are removed by spline interpolation, using 
#                               \code{\link[zoo]{na.spline}}
# 'plot'       : logical. Indicates if the resulting seasonal values should be 
#                plotted or not. 
# 'year.start' : Integer, indicating the numeric value of the month used as first 
#                value of the plot (Jan=1,..., Dec=12)
# 'month.names': character, with the desired names of each one months of the 
#                year, string in January.
# 'xlab'       :
# 'ylab'       :
# 'main'       :
# 'leg.title'  :
# 'cols'       : character, representing the colors to be used for ploting the 
#                monthly values of each one the exceedence probabilities
# 'pchs'       : numeric, representing the symbols used for ploting the 
#                monthly values of each one the exceedence probabilities

plot_p_q <- function(p, 
                     q, 
                     na.fill=c("remove", "linear", "spline"), 
                     plot=FALSE, 
                     xlab="Month", 
                     ylab=c("P", "Q"), 
                     main="Monthly probabilities of exceedence",
                     leg.title="",
                     leg.text=c("P", "Q"),
                     cols=c("blue", "black"),
                     pchs=c(15,16)
                     ) {


  # Checking 'na.fill'
  na.fill <- match.arg(na.fill)

  # Checking that 'p' and 'q' have the same time index
  if ( !all.equal(time(p), time(q)) )
    stop("Invalid argument(s): 'p' and 'q' must have the same time index !")

  # If required, filling in any missing value(s)
  if (na.fill != "remove") {
    p.na.index <- which(is.na(p))
    q.na.index <- which(is.na(q))

    if (length(p.na.index) > 0) {
      if (na.fill == "linear") {
        p <- na.approx(p)
      } else p <- na.spline(p)
    } # IF end
    if (length(q.na.index) > 0) {
      if (na.fill == "linear") {
        q <- na.approx(q)
      } else q <- na.spline(q)
    } # IF end
  } # IF end

 #par(mar=0.1 + c(5, 4, 4, 8), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)
 par(mar=0.1 + c(7, 4, 4, 4), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)

 ylim    <- range(q, na.rm=TRUE)
 ylim[2] <- ylim[2]*1.5
 plot(q, type="n", xaxt="n", xlab=xlab, ylab=ylab[2], ylim=ylim, main=main)
 drawTimeAxis(q)
 points(q, type="o", col=cols[2], pch=pchs[2], lty=1, cex=0.5)
 grid()

 par(new=TRUE)
 ylim    <- rev(range(p, na.rm=TRUE))
 ylim[1] <- ylim[1]*3
 barplot(p, ylim = ylim, xaxt="n", yaxt = "n", col=cols[1], pch=pchs[1], border=NA)
 axis(4, at = pretty(p), col=cols[1], col.axis=cols[1])
 mtext(side = 4, line = 2, text="P", col=cols[1], srt=45)
 #at <- pretty(p)
 #axis(4, at=at, label=FALSE, col=cols[1])
 #text(y=at-2.5, x=par("usr")[2]+30, labels = at, srt = -90, pos = 4, xpd = TRUE, col=cols[1], offset=0.5)

# legend("topright", inset=c(-0.07, 0), legend = leg.text, bty="n",
#        lty=1, pch=pchs, col=cols,
#        #lty = 1:2, xjust = 1, yjust = 1,
#        title=leg.title)

leg.text  <- c(leg.text[1], " ", leg.text[2])
cols      <- c(cols[1], "white", cols[2])
pchs      <- c(pchs[1], 3, pchs[2])

legend("bottom", horiz=TRUE, , bty="n", inset=c(0, -0.22), 
        x.intersp=0.1, y.intersp=3, 
        lty=1, pch=pchs, col=cols,
        title=leg.title, legend= leg.text)


} # 'plot_p_q' END
