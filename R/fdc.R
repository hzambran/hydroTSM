######################################################
# fdc: Flow Duration Curve, computation and plot     #
######################################################
#                   June 04, 2009                    #
######################################################

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
                         ylim,
                         col="black",
                         pch=1,
                         lwd=1,
                         lty=1,
                         cex=0.4,
                         cex.axis=1.2,
                         cex.lab=1.2,
                         leg.txt=NULL,
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

     if (log == "y") {
       x.zero.index <- which(x==0)
       if (length(x.zero.index) > 0 ) {
        x <- x[-x.zero.index]
        if (verbose) print("[Warning: all 'x' equal to zero will not be plotted]", quote=FALSE)
       } # IF end
     } # IF end

	 # If 'x' is of class 'ts' or 'zoo'
	 #if ( !is.na( match( class(x), c("ts", "zoo") ) ) )
	 x <- as.numeric(x)

	 # Storing the original values
	 x.old <- x

	 # 1) Sort 'x' in drecreasing order. This is just for avoiding misleading
	 #lines when using 'type="o"' for plotting
	 x <- sort(x)

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

          if ( missing(ylim) ) {
            ylim <- range(x)
          } # IF end

          # If a new plot has to be created
          if (new) {

            # Creating the plot, but without anything on it, for allowing the call to polygon
             if (log=="y") {
               plot(dc, x,  xaxt = "n", yaxt="n", type="o", pch=pch, col=col, lty=lty,
                    cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, main=main, xlab=xlab, ylab=ylab, ylim=ylim, log=log, ...)
             } else {
               plot(dc, x,  xaxt = "n", type="o", pch=pch, col=col, lty=lty,
                    cex=cex, cex.axis= cex.axis, cex.lab=cex.lab, main=main, xlab=xlab, ylab=ylab, ylim=ylim, log=log, ...)
               } # ELSE end

          } else {
             lines(dc, x,  xaxt = "n", type="o", pch=pch, col=col, lty=lty,
                   cex=cex)
            } # ELSE end

          # Draws the labels corresponding to Annual ticks in the X axis
          Axis(side = 1, at = seq(0.0, 1, by=0.05), cex.axis=cex.axis, labels = FALSE)
          Axis(side = 1, at = seq(0.0, 1, by=0.1), cex.axis=cex.axis, 
               labels = paste(100*seq(0.0, 1, by=0.1),"%", sep="") )

          if (log=="y") {
            # Draws the labels corresponding to Annual ticks in the Y axis
            ylabels <- union( c(0.01, 0.1, 1,10), pretty(ylim) )
            Axis( side = 2, at =ylabels, cex.axis=cex.axis, labels = ylabels )
          } # IF end

          # If the user provided a value for 'lQ.thr', a vertical line is drawn
          if ( !is.na(lQ.thr) ) {
            abline(v=lQ.thr, col="grey", lty=3, lwd=2)
          } # IF end

          # If the user provided a value for 'hQ.thr', a vertical line is drawn
          if ( !is.na(hQ.thr) ) {
            abline(v=hQ.thr, col="grey", lty=3, lwd=2)
          } # IF end

          # Drawing a legend. bty="n" => no border
          if ( !missing(leg.txt) ) {
           legend("topright", legend=leg.txt, cex=cex*1.5, col=col, lty=lty, pch=pch, bty="n")
          } # IF end

          if (thr.shw) {
              # Finding the flow values corresponding to the 'lQ.thr' and 'hQ.thr' pbb of excedence
              x.lQ <- x[Qposition(dc, lQ.thr)]
              x.hQ <- x[Qposition(dc, hQ.thr)]

              legend("bottomleft", c(paste("Qhigh.thr=", round(x.hQ, 2), sep=""),
                                     paste("Qlow.thr=", round(x.lQ, 2), sep="") ),
                     cex=0.7, bty="n" ) #bty="n" => no box around the legend
          } # IF end
	 } # IF end

	 # Restoring the original positions
	 dc <- dc[ind]

	 return(dc)


} # 'fdc' END


######################################################################
# fdc.matrix: (ONLY) Plot of Multiple Flow Duration Curves,          #
#                  for comparison                                    #
######################################################################
#                   June 04, 2009                                    #
######################################################################

fdc.matrix <- function (x,
                        lQ.thr=0.7,
                        hQ.thr=0.2,
                        plot=TRUE,
                        log="y",
                        main= "Flow Duration Curve",
                        xlab="% Time flow equalled or exceeded",
			                  ylab="Q, [m3/s]",
                        ylim,
                        col=palette("default")[1:ncol(x)],
                        pch=1:ncol(x),
                        lwd=rep(1, ncol(x)),
                        lty=1:ncol(x),
                        cex=0.4,
                        cex.axis=1.2,
                        cex.lab=1.2,
                        leg.txt= colnames(x),
                        verbose=TRUE,
                        thr.shw=TRUE,
                        new=TRUE,
                        ...) {

  n <- ncol(x)

  if (missing(ylim)) { ylim <- range(x) }

  j <- 1 # starting column for the analysis

  if (verbose) print( paste("Column: ", format(j, width=10, justify="left"),
                            " : ", format(j, width=3, justify="left"), "/",
                            n, " => ",
                            format(round(100*j/n,2), width=6, justify="left"),
                            "%", sep=""), quote=FALSE )

  # Computing and plotting the Flow duration Curve for the first vector
  fdc(x=x[,1], plot=plot, pch=pch[1], col=col[1], lty=lty[1],
      cex=cex, main=main, xlab= xlab, ylab=ylab, log=log, verbose=verbose,
      thr.shw=FALSE, new=TRUE, ...)

  # Plotting the Flow Duration Curves
  sapply(2:n, function(j) {

        if (verbose) print( paste("Column: ", format(j, width=10, justify="left"),
                                " : ", format(j, width=3, justify="left"), "/",
                                n, " => ",
                                format(round(100*j/n,2), width=6, justify="left"),
                                "%", sep=""), quote=FALSE )
			# Computing and plotting the Flow duration Curve for the first vector
            fdc(x=x[,j], plot=plot, pch=pch[j], col=col[j], lty=lty[j],
                cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, main=main, 
                xlab= xlab, ylab=ylab, log=log, verbose=verbose,
                thr.shw=FALSE, new=FALSE, ...)
        } )

  if (plot) {
      # Drawing a legend. bty="n" => no border
      if ( is.null(colnames(x)) ) {
        leg.txt <- paste("Q", 1:ncol(x), sep="")
      } # IF end
      legend("topright", legend=leg.txt, cex=cex*2.2, col=col, lty=lty, pch=pch, bty="n")
  } # IF end

} # 'fdc.matrix' END


fdc.data.frame <- function(x,
                           lQ.thr=0.7,
                           hQ.thr=0.2,
                           plot=TRUE,
                           log="y",
                           main= "Flow Duration Curve",
                           xlab="% Time flow equalled or exceeded",
						               ylab="Q, [m3/s]",
                           ylim,
                           col=palette("default")[1:ncol(x)],
                           pch=1:ncol(x),
                           lwd=rep(1, ncol(x)),
                           lty=1:ncol(x),
                           cex=0.4,
                           cex.axis=1.2,
                           cex.lab=1.2,
                           leg.txt= colnames(x),
                           verbose=TRUE,
                           thr.shw=TRUE,
                           new=TRUE,
                           ...) {

   x <- as.matrix(x)

   NextMethod("fdc", x,
               lQ.thr=lQ.thr,
               hQ.thr=hQ.thr,
               plot=plot,
               main=main,
               xlab=xlab,
               ylab=ylab,
               log=log,
               col=col,
               pch=pch,
               lty=lty,
               cex=cex,
               cex.axis=cex.axis,
               cex.lab=cex.axis,
               verbose=verbose,
               leg.txt= leg.txt,
               thr.shw=thr.shw,
               new=new,
               ylim=ylim,
               ...)

} # 'fdc.data.frame' END