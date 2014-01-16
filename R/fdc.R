# File fdc.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2007-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# fdc: Flow Duration Curve, computation and plot                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: June 04, 2009                                                       #
# Updates: 25-Feb-2011 ; 04-Nov-2011                                           #
#          02-May-2012; 16-Oct-2012                                            #
#          05-Aug-2013                                                         #
#          15-Jan-2014                                                         # 
################################################################################

# Plot the flow Duration Curve in the original time units of 'x' and
# also gives the probability of exceedence of each element

# plot   : logical. Indicates if the flow duration curve should be plotted or not
# thr.shw: logical, indicating if the stremflow values corresponding to the user-defined thresholds 'lQ.thr' and 'hQ.thr'  have to be plotted
# new    : logical, indicates if a new graphics device has to be started
# log    : character, indicates which axis has to be plotted with a logarithmic scale. By default is 'y'

fdc <-function(x, ...) UseMethod("fdc")

fdc.default <- function (x,
                         lQ.thr=0.7,
                         hQ.thr=0.2,
                         plot=TRUE,
                         log="y",
                         main="Flow Duration Curve",
                         xlab="% Time flow equalled or exceeded",
                         ylab="Q, [m3/s]",
                         ylim=NULL,
                         yat=c(0.01, 0.1, 1), 
                         xat=c(0.01, 0.025, 0.05),
                         #yaxp=c(range(x),2),
                         col="black",
                         pch=1,
                         lwd=1,
                         lty=1,
                         cex=0.4,
                         cex.axis=1.2,
                         cex.lab=1.2,
                         leg.txt=NULL,
                         leg.cex=1,
                         leg.pos="topright",
                         verbose= TRUE,
                         thr.shw=TRUE,
                         new=TRUE,
                         ...) {

     # Returns the position in the vector 'x' where the scalar 'Q' is located
     Qposition <- function(x, Q) {
       Q.dist  <- abs(x - Q)
       Q.index <- which.min( Q.dist )
       return(Q.index)
     } # end

     # If 'x' is of class 'ts' or 'zoo'
     #if ( !is.na( match( class(x), c("ts", "zoo") ) ) )
     x <- as.numeric(x)

     # Storing the original values
     x.old <- x

     # 1) Sort 'x' in drecreasing order. This is just for avoiding misleading
     #lines when using 'type="o"' for plotting
     x <- sort(x)

     # Detecting zero values
     x.zero.index <- which(x==0)
     nzeros <- length(x.zero.index)

     # Index with the position of the original values
     ind <- match(x.old, x)

     # 2) Compute the length of 'x'
     n <- length(x)

     # 3) Creation of the output vector
     dc <- rep(NA, n)

     # 4) Exceedence Probability
     dc[1:n] <- sapply(1:n, function(j,y) {
                  dc[j] <- length( which(y >= y[j]) )
                }, y = x)
 
     # Computing the probabilitites
     dc <- dc/n

     # Another way
     # Fn <- ecdf(x)
     # dc <- 1 - Fn(x) + 1/n

     if (plot) {

          dc.plot <- dc
  
          if (log == "y") {
            if (nzeros > 0) {
              x       <- x[-x.zero.index]
              dc.plot <- dc.plot[-x.zero.index]
              if (verbose) message("[Note: all 'x' equal to zero (", nzeros, ") will not be plotted ]")
            } # IF end
          } # IF end

          if ( is.null(ylim) ) ylim <- range(x, na.rm=TRUE)
             
          if ( ((log=="y") | (log=="xy") | (log=="yx")) & min(ylim)==0 ) {
             tmp <- x
             tmp[which(tmp==0)] <- NA
             ylim[1] <- min(tmp, na.rm=TRUE)
          } # IF end
          
          # If a new plot has to be created
          if (new) {
               plot(dc.plot, x,  xaxt = "n", yaxt = "n", type="o", col=col, pch=pch, lwd=lwd, lty=lty,
                    cex=cex, cex.axis= cex.axis, cex.lab=cex.lab, main=main, xlab=xlab, ylab=ylab, ylim=ylim, log=log, ...)
          } else lines(dc.plot, x,  xaxt = "n", type="o", col=col, pch=pch, lwd=lwd, lty=lty, cex=cex)

          # Y axis: Drawing the ticks and labels
          ylabels <- pretty(ylim)
          if ( (log=="y") | (log=="xy") | (log=="yx") ) {            
            ylabels <- union( yat, ylabels )            
          } # IF end
          Axis( side = 2, at =ylabels, cex.axis=cex.axis, labels = ylabels)
          
          # X axis: Drawing the ticks and labels
          xpos    <- seq(0.0, 1, by=0.05)
          xlabels <- seq(0.0, 1, by=0.1)
          if ( (log=="x") | (log=="xy") | (log=="yx") ) {            
            xpos    <- union( xat, xpos ) 
            xlabels <- union( xat, xlabels )            
          } # IF end
          Axis(side = 1, at = xpos, cex.axis=cex.axis, labels = FALSE)
          Axis(side = 1, at = xlabels, cex.axis=cex.axis, labels = paste(100*xlabels,"%", sep="") )               

          # If the user provided a value for 'lQ.thr', a vertical line is drawn
          if ( !is.na(lQ.thr) ) abline(v=lQ.thr, col="grey", lty=3, lwd=2)

          # If the user provided a value for 'hQ.thr', a vertical line is drawn
          if ( !is.na(hQ.thr) ) abline(v=hQ.thr, col="grey", lty=3, lwd=2)

          # Drawing a legend. bty="n" => no border
          if ( !is.null(leg.txt) )
           legend(x=leg.pos, legend=leg.txt, cex=leg.cex, col=col, pch=pch, lwd=lwd, lty=lty, bty="n") # cex=cex*1.5,

          if (thr.shw) {
              # Finding the flow values corresponding to the 'lQ.thr' and 'hQ.thr' pbb of excedence
              x.lQ <- x[Qposition(dc.plot, lQ.thr)]
              x.hQ <- x[Qposition(dc.plot, hQ.thr)]

              legend("bottomleft", c(paste("Qhigh.thr=", round(x.hQ, 2), sep=""),
                                     paste("Qlow.thr=", round(x.lQ, 2), sep="") ),
                     cex=0.8, bty="n") #bty="n" => no box around the legend
          } # IF end
     } # IF end

     # Restoring the original positions
     dc <- dc[ind]

     return(dc)

} # 'fdc.default' END


################################################################################
# fdc.matrix: Computation and/or Plot of Multiple Flow Duration Curves,        #
#             mainly for comparison                                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 04-Jun-2009                                                         #
# Updates: 16-Sep-2011 ; 03-Nov-2011                                           #
#          02-May-2012                                                         #
#          05-Aug-2013                                                         #
################################################################################

fdc.matrix <- function (x,
                        lQ.thr=0.7,
                        hQ.thr=0.2,
                        plot=TRUE,
                        log="y",
                        main= "Flow Duration Curve",
                        xlab="% Time flow equalled or exceeded",
                        ylab="Q, [m3/s]",
                        ylim=NULL,
                        yat=c(0.01, 0.1, 1), 
                        xat=c(0.01, 0.025, 0.05),
                        col=palette("default")[1:ncol(x)],
                        pch=1:ncol(x),
                        lwd=rep(1, ncol(x)),
                        lty=1:ncol(x),
                        cex=0.4,
                        cex.axis=1.2,
                        cex.lab=1.2,
                        leg.txt=NULL,
                        leg.cex=1,
                        leg.pos="topright",
                        verbose=TRUE,
                        thr.shw=TRUE,
                        new=TRUE,
                        ...) {

  if (is.null(ylim)) ylim <- range(x, na.rm=TRUE)
  
  if ( ((log=="y") | (log=="xy") | (log=="yx")) & min(ylim)==0 ) {
     tmp <- unlist(x)
     tmp[which(tmp==0)] <- NA
     ylim[1] <- min(tmp, na.rm=TRUE)
  } # IF end
  
  # Computing the FDC for each column of 'x'
  out <- apply(x, FUN=fdc.default, MARGIN=2, plot=FALSE, log="")
  
  # Plotting the FDC for each column of 'x'
  if (plot) {

    if (thr.shw==TRUE) {
      message("[Note: 'thr.shw' was set to FALSE to avoid confusing legends...]")
      thr.shw <- FALSE
    } # IF end
    
    n <- ncol(x)
    j <- 1 # starting column for the analysis

    if (verbose) message("[Column: ", format(j, width=10, justify="left"),
                          " : ", format(j, width=3, justify="left"), "/",
                          n, " => ",
                          format(round(100*j/n,2), width=6, justify="left"),
                          "% ]" )

    # Computing and plotting the Flow Duration Curve for the first column
    fdc(x=x[,1], plot=plot, log=log, col=col[1], pch=pch[1], lwd=lwd[1], lty=lty[1],
        cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, main=main, 
        xlab= xlab, ylab=ylab, ylim=ylim, yat=yat, 
        verbose=verbose, thr.shw=FALSE, new=TRUE, ...)

    # Plotting the Flow Duration Curves for all the columns but the first one
    sapply(2:n, function(j) {

          if (verbose) message("[Column: ", format(j, width=10, justify="left"),
                                " : ", format(j, width=3, justify="left"), "/",
                                n, " => ",
                                format(round(100*j/n,2), width=6, justify="left"),
                                "% ]")

         # Computing and plotting the Flow duration Curve for the other columns
         tmp <- sort(x[,j])
              yval <- fdc(x=tmp, plot=FALSE, log="", verbose=verbose)
              points(yval, tmp, cex=cex, col=col[j], pch=pch[j], lwd=lwd[j], lty=lty[j], type="o")
            
          } )

    # Drawing a legend. bty="n" => no border
    if ( is.null(leg.txt) ) {
      if (!is.null(colnames(x))) {
        leg.txt <- colnames(x)
      } else leg.txt <- paste("Q", 1:ncol(x), sep="")  
    }
    legend(x=leg.pos, legend=leg.txt, cex=leg.cex, col=col, pch=pch, lwd=lwd, lty=lty, bty="n") # cex=cex*2.2,
  
  } # IF end

  return(out)
  
} # 'fdc.matrix' END


################################################################################
# fdc.data.frame: Computation and/or Plot of Multiple Flow Duration Curves,    #
#                 mainly for comparison                                        #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 04-Jun-2009                                                         #
# Updates: 03-Nov-2011                                                         #
#          02-May-2012                                                         #
#          05-Aug-2013                                                         #
################################################################################
fdc.data.frame <- function(x,
                           lQ.thr=0.7,
                           hQ.thr=0.2,
                           plot=TRUE,
                           log="y",
                           main= "Flow Duration Curve",
                           xlab="% Time flow equalled or exceeded",
                           ylab="Q, [m3/s]",
                           ylim,
                           yat=c(0.01, 0.1, 1), 
                           xat=c(0.01, 0.025, 0.05), 
                           col=palette("default")[1:ncol(x)],
                           pch=1:ncol(x),
                           lwd=rep(1, ncol(x)),
                           lty=1:ncol(x),
                           cex=0.4,
                           cex.axis=1.2,
                           cex.lab=1.2,
                           leg.txt=NULL,
                           leg.cex=1,
                           leg.pos="topright",
                           verbose=TRUE,
                           thr.shw=TRUE,
                           new=TRUE,
                           ...) {

   x <- as.matrix(x)
   
   if (missing(ylim)) ylim <- range(x, na.rm=TRUE)

   fdc.matrix(x,
              lQ.thr=lQ.thr,
              hQ.thr=hQ.thr,
              plot=plot,
              log=log,
              main=main,
              xlab=xlab,
              ylab=ylab,
              ylim=ylim,
              yat=yat,   
              xat=xat,
              col=col,
              pch=pch,
              lwd=lwd,
              lty=lty,
              cex=cex,
              cex.axis=cex.axis,
              cex.lab=cex.axis,
              verbose=verbose,
              leg.txt=leg.txt,
              leg.cex=leg.cex,
              leg.pos=leg.pos,
              thr.shw=thr.shw,
              new=new,               
              ...)

} # 'fdc.data.frame' END


################################################################################
# fdc.matrix: Computation and/or Plot of Multiple Flow Duration Curves,        #
#             mainly for comparison                                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 03-Nov-2011                                                         #
# Updates: 02-May-2012                                                         #
#          05-Aug-2013                                                         #
################################################################################
fdc.zoo <- function (x,
                     lQ.thr=0.7,
                     hQ.thr=0.2,
                     plot=TRUE,
                     log="y",
                     main= "Flow Duration Curve",
                     xlab="% Time flow equalled or exceeded",
                     ylab="Q, [m3/s]",
                     ylim=NULL,
                     yat=c(0.01, 0.1, 1), 
                     xat=c(0.01, 0.025, 0.05),
                     col=palette("default")[1:NCOL(x)],
                     pch=1:NCOL(x),
                     lwd=rep(1, NCOL(x)),
                     lty=1:NCOL(x),
                     cex=0.4,
                     cex.axis=1.2,
                     cex.lab=1.2,
                     leg.txt=NULL,
                     leg.cex=1,
                     leg.pos="topright",
                     verbose=TRUE,
                     thr.shw=TRUE,
                     new=TRUE,
                     ...) {

  x <- coredata(x)
  if ( is.matrix(x) | is.data.frame(x) ) {  
    fdc.matrix(x,
               lQ.thr=lQ.thr,
               hQ.thr=hQ.thr,
               plot=plot,
               log=log,
               main=main,
               xlab=xlab,
               ylab=ylab,
               ylim=ylim,
               yat=yat,   
               xat=xat,
               col=col,
               pch=pch,
               lwd=lwd,
               lty=lty,
               cex=cex,
               cex.axis=cex.axis,
               cex.lab=cex.axis,
               verbose=verbose,
               leg.txt= leg.txt,
               leg.cex=leg.cex,
               leg.pos=leg.pos,
               thr.shw=thr.shw,
               new=new,               
               ...)
  } else fdc.default(x,
                     lQ.thr=lQ.thr,
                     hQ.thr=hQ.thr,
                     plot=plot,
                     log=log,
                     main=main,
                     xlab=xlab,
                     ylab=ylab,
                     ylim=ylim,
                     yat=yat,   
                     xat=xat,
                     col=col,
                     pch=pch,
                     lwd=lwd,
                     lty=lty,
                     cex=cex,
                     cex.axis=cex.axis,
                     cex.lab=cex.axis,
                     verbose=verbose,
                     leg.txt=leg.txt,
                     leg.cex=leg.cex,                     
                     leg.pos=leg.pos,
                     thr.shw=thr.shw,
                     new=new,               
                     ...)
  
  
} # 'fdc.zoo' END

