# File fdcu.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# fdcu: Flow Duration Curve with Uncertainty Bounds                            #
################################################################################
# Purpose: Plot the flow Duration Curve in the original time units of 'x' and  #
#          also gives the probability of exceedence of each element            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: January 29th, 2010                                                  #
# Updates: February 04th, 2010                                                 #
#          03-May-2012                                                         #
################################################################################

# 'x'            : 'numeric', 'matrix' or 'data.frame' whose columns contains the values of the time series of observed streamflows for which the flow duration curve will be computed.
# 'lband'        : 'numeric', 'matrix' or 'data.frame' whose columns contains the values of the time series with the lower uncertainty bound of 'x'.
# 'uband'        : 'numeric', 'matrix' or 'data.frame' whose columns contains the values of the time series with the upper uncertainty bound of 'x'.
# 'sim'          : 'numeric', 'matrix' or 'data.frame' whose columns contains the values of the time series with the simulated values of 'x', for which the flow duration curve will be computed.
# 'lQ.thr'       : 'numeric', low flows separation threshold. If this value is different from 'NA', a vertical line is drawn in this value, and all the values to the left of it are deemed low flows.
# 'hQ.thr'       : 'numeric', high flows separation threshold. If this value is different from 'NA', a vertical line is drawn in this value, and all the values to the right of it are deemed high flows
# 'plot'         : 'logical'. Indicates if the flow duration curve should be plotted or not
# 'log'          : 'character', indicates which axis has to be plotted with a logarithmic scale. By default is 'y'
# 'main'         : See '?plot'. An overall title for the plot: see 'title'.
# 'xlab'         : See '?plot'. A title for the x axis: see 'title'.
# 'ylab'         : See '?plot'. A title for the y axis: see 'title'.
# 'ylim'         : See '?plot.default'.  The y limits of the plot.
# 'col'          : See '?plot.default'. The colors for lines and points.  Multiple colors can be specified so that each point can be given its own color.  If there are fewer colors than points they are recycled in the standard fashion. Lines will all be plotted in the first colour specified.
# 'pch'          : See '?plot.default'. A vector of plotting characters or symbols: see 'points'.
# 'lwd'          : See '?plot.default'. The line width, see 'par'.
# 'lty'          : See '?plot.default'. The line type, see 'par'.
# 'cex'          : See '?plot.default'. A numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default.  This works as a multiple of 'par("cex")'. 'NULL' and 'NA' are equivalent to '1.0'.  Note that this does not affect annotation
# 'leg.txt'      : vector with the names that have to be used for each column of 'x'.
# 'verbose'      : logical; if TRUE, progress messages are printed
# 'thr.shw'      : logical, indicating if the stremflow values corresponding to the user-defined thresholds 'lQ.thr' and 'hQ.thr' have to be shown in the plot.
# 'border'       : See '?polygon'. The color to draw the border of the polygon with the uncertainty bounds. The default, 'NA', means to omit borders.
# 'bands.col'    : See '?polygon'. The color for filling the polygon. The default, 'NA', is to leave polygons unfilled, unless 'density' is specified. If 'bands.density' is specified with a positive value this gives the color of the shading lines.
# 'bands.density': See '?polygon'. The density of shading lines for the polygon with the uncertainty bounds, in lines per inch.  The default value of 'NULL' means that no shading lines are drawn. A zero value of 'bands.density' means no shading nor filling whereas negative values (and 'NA') suppress shading (and so allow color filling).
# 'bands.angle'  : See '?polygon'. The slope of shading lines for the polygon with the uncertainty bounds, given as an angle in degrees (counter-clockwise).
# 'new'          : logical, if TRUE, a new plotting window is created.

fdcu <-function(x, lband, uband, ...) UseMethod("fdcu")

fdcu.default <- function (x,
                          lband,
                          uband,
                          sim,
                          lQ.thr=0.7,
                          hQ.thr=0.2,
                          plot=TRUE,
                          log="y",
                          main="Flow Duration Curve",
                          xlab="% Time flow equalled or exceeded",
                          ylab="Q, [m3/s]",
                          ylim,
                          yat=c(0.01, 0.1, 1), 
                          xat=c(0.01, 0.025, 0.05),
                          col=c("black", "red"),
                          pch=c(1, 15),
                          lwd=c(1, 0.8),
                          lty=c(1, 3),
                          cex=0.2,
                          cex.axis=1.2,
                          cex.lab=1.2,
                          leg.txt= c("Qobs", "Qsim", "95PPU"),
                          leg.cex=1,
                          leg.pos="auto",
                          verbose= TRUE,
                          thr.shw=TRUE,
                          border=NA,
                          bands.col="lightcyan",
                          bands.density=NULL,
                          bands.angle=45,
                          new=TRUE,
                          ...) {


     #checking that the user provided 'x'
     if (missing(x))  stop("Missing argument: 'x' must be provided")

     #checking that the user provided 'lband'
     if (missing(lband))  stop("Missing argument: 'lband' must be provided")

     #checking that the user provided 'uband'
     if (missing(uband))  stop("Missing argument: 'uband' must be provided")

     # Function for finding the position of 'Q' within 'x'
     Qposition <- function(x, Q) {
       Q.dist  <- abs(x - Q)
       Q.index <- which.min( Q.dist )
       return(Q.index)
     } # end

	 # In case 'x', 'laband' and 'uband' or 'sim' be of 'zoo' or 'ts' class
     x     <- as.numeric(x)
	 lband <- as.numeric(lband)
	 uband <- as.numeric(uband)
     if (!missing(sim)) { sim <- as.numeric(sim)   }

     # Removing zero values when using a logarithmic scale
     if (log == "y") {

       x.zero.index     <- which(x==0)
       lband.zero.index <- which(lband==0)
       uband.zero.index <- which(uband==0)
       if (!missing(sim)) { sim.zero.index <- which(sim==0) }

       if (length(x.zero.index) > 0 ) {
        x <- x[-x.zero.index]
        if (verbose) message("[Warning: all 'x' equal to zero will not be plotted]" )
       } # IF end

       if (length(lband.zero.index) > 0 ) {
        lband <- lband[-lband.zero.index]
        if (verbose) message("[Warning: all 'lband' equal to zero will not be plotted]" )
       } # IF end

       if (length(uband.zero.index) > 0 ) {
        uband <- uband[-uband.zero.index]
        if (verbose) message("[Warning: all 'uband' equal to zero will not be plotted]" )
       } # IF end

       if (!missing(sim)) {
            if (length(sim.zero.index) > 0 ) {
            sim <- sim[-sim.zero.index]
            if (verbose) message("[Warning: all 'sim' equal to zero will not be plotted]" )
           } # IF end
       } # IF end

     } # IF end

	 # 1) Sort 'x' in decreasing order. This is just for avoiding misleading
	 #lines when using 'type="o"' for plotting
	 x.sort     <- sort(x, decreasing=TRUE)
	 lband.sort <- sort(lband, decreasing=TRUE)
	 uband.sort <- sort(uband, decreasing=TRUE)
     if (!missing(sim)) { sim.sort <- sort(sim, decreasing=TRUE) }

	 fdc.x     <- fdc(x.sort, log=log, plot=FALSE,...)
	 fdc.lband <- fdc(lband.sort, log=log, plot=FALSE)
	 fdc.uband <- fdc(uband.sort, log=log, plot=FALSE)
     if (!missing(sim)) { fdc.sim <- fdc(sim.sort, log=log, plot=FALSE) }

	 #na.index <- which(is.na(x))

     # Avoiding plotting the uncertainty bands for the Na's
     #uband[na.index] <- uband[na.index-1]
     #lband[na.index] <- lband[na.index+1]

     if (plot) {

         if ( missing(ylim) ) {
          ylim <- range(lband, uband, na.rm=TRUE)
         } else {
             # In case 'ylim' is 'NA'
             if ( is.na(ylim[1]) ) { ylim[1] <- range(x, na.rm=TRUE)[1] }
             if ( is.na(ylim[2]) ) { ylim[2] <- range(x, na.rm=TRUE)[2] }
           } # ELSE end

         # Logarithmic scale can NOT start in zero
         if (log=="y") { ylim <- c(0.01, ylim[2]) }

         # Creating the 'x' values of the polygons of the bands
         t <- c( fdc.lband, rev(fdc.uband) )

         # Creating the 'y' values of the polygons of the bands
         bands <- c( lband.sort, rev(uband.sort) )

         if (new) {
             # Creating the plot, but without anything on it, for allowing the call to polygon            
             plot(fdc.x, x.sort, type="n", xaxt = "n", yaxt = "n", main=main, xlab=xlab, ylab=ylab, log=log, ylim=ylim, ...)             
         } # IF end

         # Plotting the polygons between the lower and upper bands
         polygon(t, bands, col=bands.col, density=bands.density, angle=bands.angle, border=border)

         # Ploting the OBServations over the polygon
         lines(fdc.x, x.sort, cex=cex, col=col[1], pch=pch[1], lwd=lwd[1], lty=lty[1])
         points(fdc.x, x.sort, cex=cex, col=col[1], pch=pch[1])

         # Ploting the SIMulated values over the polygon
         if (!missing(sim)) {
           lines(fdc.sim, sim.sort, cex=cex, col=col[2], pch=pch[2], lwd=lwd[2], lty=lty[2])
           points(fdc.sim, sim.sort, cex=cex, col=col[2], pch=pch[2])
         } # IF end

         # Drawing the ticks and labels corresponding to the Y axis 
          ylabels <- pretty(ylim)
          if ( (log=="y") | (log=="xy") | (log=="yx") ) {            
            ylabels <- union( yat, ylabels )            
          } # IF end
          Axis( side = 2, at =ylabels, cex.axis=cex.axis, labels = ylabels)
          
          # Drawing the ticks and labels corresponding to the X axis
          xpos    <- seq(0.0, 1, by=0.05)
          xlabels <- seq(0.0, 1, by=0.1)
          if ( (log=="x") | (log=="xy") | (log=="yx") ) {            
            xpos    <- union( xat, xpos ) 
            xlabels <- union( xat, xlabels )            
          } # IF end
          Axis(side = 1, at = xpos, cex.axis=cex.axis, labels = FALSE)
          Axis(side = 1, at = xlabels, cex.axis=cex.axis, labels = paste(100*xlabels,"%", sep="") )

         # Drawing a legend. bty="n" => no border
         if (!is.null(leg.txt)) {

             # Drawing a legend. bty="n" => no border
             if (leg.pos=="auto")
               ifelse (log=="y", leg.pos.x <- "bottomleft", leg.pos.x <- "topright")

             if (!missing(sim)) { #Legend with the OBS + simulations + 95PPU
              legend(x=leg.pos.x, legend=leg.txt,  #inset=0.03,
                     bty="n", cex =leg.cex, col=c(col[1], col[2], bands.col), 
                     lwd=c(lwd[1], lwd[2], 0), lty=c(lty[1], lty[2], 0), 
                     pch=c(NA,NA,15), pt.cex=3)
             } else { #Legend only with the OBS + 95PPU
              legend(x=leg.pos.x, legend=c(leg.txt[1], leg.txt[3]),  #inset=0.03,
                     bty="n", cex =leg.cex, col=c(col[1], bands.col), lwd=c(lwd[1], 0), 
                     lty=c(lty[1], 0), pch=c(NA,15), pt.cex=3)
             }# IF end

         } # IF end

          # If the user provided a value for 'lQ.thr', a vertical line is drawn
         if ( !is.na(lQ.thr) ) {
            abline(v=lQ.thr, col="grey", lty=3, lwd=2)
         } # IF end

         # If the user provided a value for 'hQ.thr', a vertical line is drawn
         if ( !is.na(hQ.thr) ) {
            abline(v=hQ.thr, col="grey", lty=3, lwd=2)
         } # IF end

         # If the user want to see the Q values corresponding to 'lQ.thr' and 'hQ.thr'
         if (thr.shw) {              
            # Drawing a legend. bty="n" => no border
            if (leg.pos=="auto")
              ifelse (log=="y", leg.pos.x <- "topright", leg.pos.x <- "bottomleft")

            # Finding the flow values corresponding to the 'lQ.thr' and 'hQ.thr' pbb of excedence
            x.lQ <- x.sort[Qposition(fdc.x, lQ.thr)]
            x.hQ <- x.sort[Qposition(fdc.x, hQ.thr)]

            legend(x=leg.pos.x, 
                   legend=c(paste("Qhigh.thr=", round(x.hQ, 2), sep=""),
                            paste("Qlow.thr=", round(x.lQ, 2), sep="") ),
                   bty="n", cex=leg.cex ) #bty="n" => no box around the legend
         } # IF end

     } # IF 'plot' end

	 #tmp <- cbind(fdc.lband, fdc.x, fdc.uband)
	 #colnames(tmp) <- c("fdc.l", "fdc.x", "fdc.u")

	 #return(tmp)

} # 'fdcu.default' END



######################################################################
# fdcu.matrix: (ONLY) Plots of Multiple Flow Duration Curves,        #
#                  with uncertainty bands                            #
######################################################################
# Started    :  January 29th, 2010                                   #
# Last updated:  April 06th, 2010                                    #
######################################################################

fdcu.matrix <- function (x,
                         lband,
                         uband,
                         sim,
                         lQ.thr=0.7,
                         hQ.thr=0.2,
                         plot=TRUE,
                         log="y",
                         main="Flow Duration Curve",
                         xlab="% Time flow equalled or exceeded",
                         ylab="Q, [m3/s]",
                         ylim,
                         yat=c(0.01, 0.1, 1), 
                         xat=c(0.01, 0.025, 0.05),
                         col=matrix(c(rep("black", ncol(x)), 
                                    palette("default")[2:(ncol(x)+1)]), byrow=FALSE, ncol=2),
                         pch=matrix(rep(c(1, 15), ncol(x)), byrow=TRUE, ncol=2),
                         lwd=matrix(rep(c(1, 0.8), ncol(x)), byrow=TRUE, ncol=2),
                         lty=matrix(rep(c(1, 3), ncol(x)), byrow=TRUE, ncol=2),
                         cex=rep(0.1, ncol(x)),
                         cex.axis=1.2,
                         cex.lab=1.2,
                         leg.txt=c("OBS", colnames(x), "95PPU"),
                         leg.cex=1,
                         leg.pos="auto",
                         verbose= TRUE,
                         thr.shw=TRUE,
                         border=rep(NA, ncol(x)),
                         bands.col=rep("lightcyan", ncol(x)),
                         bands.density=rep(NULL, ncol(x)),
                         bands.angle=rep(45, ncol(x)),
                         new=TRUE,
                         ...) {
  n <- ncol(x)

  j <- 1 # starting column for the analysis

  if (verbose) message( paste("Column: ", format(j, width=10, justify="left"),
                      " : ", format(j, width=3, justify="left"), "/",
                      n, " => ",
                      format(round(100*j/n,2), width=6, justify="left"),
                      "%", sep="") )

  # Computing and plotting the Flow duration Curve for the first column
  if (missing(sim)) {

      sim.exists <- FALSE
      fdcu(x=x[,1], lband=lband[,1], uband=uband[,1], lQ.thr=lQ.thr, hQ.thr=hQ.thr,
           plot=TRUE, main=main, xlab= xlab, ylab=ylab, yat=yat, xat=xat, log=log, col=col[1,], pch=pch[1,],
           lwd=lwd[1,], lty=lty[1,], cex=cex[1], cex.axis=cex.axis, cex.lab=cex.lab, 
           verbose, leg.txt=NULL, leg.cex=leg.cex, leg.pos=leg.pos, thr.shw=thr.shw, border=border[1],
           bands.col= bands.col[1], bands.density=bands.density[1], bands.angle=bands.angle[1], new=TRUE, ...)
  } else {

    sim.exists <- TRUE
    fdcu(x=x[,1], lband=lband[,1], uband=uband[,1], sim=sim[,1], lQ.thr=lQ.thr, hQ.thr=hQ.thr,
           plot=TRUE, main=main, xlab= xlab, ylab=ylab, yat=yat, xat=xat, log=log, col=col[1,], pch=pch[1,],
           lwd=lwd[1,], lty=lty[1,], cex=cex[1], cex.axis=cex.axis, cex.lab=cex.lab,
           verbose, leg.txt=NULL, leg.cex=leg.cex, leg.pos=leg.pos, thr.shw=thr.shw, border=border[1],
           bands.col= bands.col[1], bands.density=bands.density[1], bands.angle=bands.angle[1], new=TRUE, ...)
    }

  # Plotting the Flow Duration Curves
  sapply(2:n, function(j) {

         if (verbose) message( paste("Column: ", format(j, width=10, justify="left"),
                             " : ", format(j, width=3, justify="left"), "/",
                             n, " => ",
                             format(round(100*j/n,2), width=6, justify="left"),
                             "%", sep="") )

            if (!sim.exists) {
              # Computing and plotting the Flow duration Curve for the first vector
              fdcu(x=x[,j], lband=lband[,j], uband=uband[,j], lQ.thr=lQ.thr, hQ.thr=hQ.thr,
                   plot=TRUE, main=main, xlab= xlab, ylab=ylab, yat=yat, xat=xat, log=log, col=col[j,], pch=pch[j,],
                   lwd=lwd[j,], lty=lty[j,], cex=cex[j], cex.axis=cex.axis, cex.lab=cex.lab, 
                   verbose, leg.txt=NULL, leg.cex=leg.cex, leg.pos=leg.pos, thr.shw=FALSE, border=border[j],
                   bands.col= bands.col[j], bands.density=bands.density[j], bands.angle=bands.angle[j], new=FALSE, ...)
            } else {
              fdcu(x=x[,j], lband=lband[,j], uband=uband[,j], sim=sim[,j], lQ.thr=lQ.thr, hQ.thr=hQ.thr,
                   plot=TRUE, main=main, xlab= xlab, ylab=ylab, yat=yat, xat=xat, log=log, col=col[j,], pch=pch[j,],
                   lwd=lwd[j,], lty=lty[j,], cex=cex[j], cex.axis=cex.axis, cex.lab=cex.lab, 
                   verbose, leg.txt=NULL, leg.cex=leg.cex, leg.pos=leg.pos, thr.shw=FALSE, border=border[j],
                   bands.col= bands.col[j], bands.density=bands.density[j], bands.angle=bands.angle[j], new=FALSE, ...)
              } # ELSE end
    } )

    if (verbose) message("Re-plotting the 'sim' lines")

    # Re-Plotting the lines that were overdrawn by the polygons
    sapply(1:n, function(j) {

            # Ploting the line with observations over the uncertainty polygon
            tmp <- sort(x[,j], decreasing=TRUE)
            if (log == "y") {
               x.zero.index <- which(tmp==0)
               if (length(x.zero.index) > 0 ) {
                tmp <- tmp[-x.zero.index]
               } # IF end
            } # IF end

            tmp.fdc <- fdc(tmp, plot=FALSE, log=log)
            lines( tmp.fdc, tmp, cex=cex, col=col[j, 1], pch=pch[j, 1], lwd=lwd[j, 1], lty=lty[j, 1] )
            points(tmp.fdc, tmp, cex=cex, col=col[j, 1], pch=pch[j, 1])

            # Ploting the lines with simulations over the uncertainty polygon
            if (sim.exists) {
              tmp <- sort(sim[,j], decreasing=TRUE)
              if (log == "y") {
               x.zero.index <- which(tmp==0)
               if (length(x.zero.index) > 0 ) {
                tmp <- tmp[-x.zero.index]
               } # IF end
             } # IF end
            tmp.fdc <- fdc(tmp, plot=FALSE, log=log)
            lines(tmp.fdc, tmp, cex=cex, col=col[j, 2], pch=pch[j, 2], lwd=lwd[j, 2], lty=lty[j, 2])
            points(tmp.fdc, tmp, cex=cex, col=col[j, 2], pch=pch[j, 2])
            }
    } ) # sapply end

    # Drawing a legend. bty="n" => no border
    if (!is.null(leg.txt)) {

        # Legend with bold font
        par(font=2)

        # Drawing a legend. bty="n" => no border
        if (leg.pos=="auto")
          ifelse (log=="y", leg.pos.x <- "bottomleft", leg.pos.x <- "topright")

        if (!missing(sim)) { #Legend with the OBS + SIMs + 95PPU
          legend(x=leg.pos.x, legend=leg.txt,  inset=0.01,
                 bty="n", cex=leg.cex, col=c(col[1,1], col[,2], bands.col), 
                 lwd=c(3*lwd[1,1], 3*lwd[,2], 0), lty=c(lty[1,1], lty[,2], 0),
                 pch=c(rep(NA, (ncol(x)+1)), 15), pt.cex=2.5)

        } else { #Legend only with the OBS + 95PPU
         legend(x=leg.pos.x, legend=c(leg.txt[1], leg.txt[3]),  inset=0.01,
                bty="n", cex=leg.cex, col=c(col[1,1], bands.col), 
                lwd=c(3*lwd[1,1], 0),  lty=c(lty[1,1],0), pch=c(NA,15), pt.cex=2.5)
        }# IF end

    } # IF end


} # 'fdcu.matrix' END


fdcu.data.frame <- function(x,
                         lband,
                         uband,
                         sim,
                         lQ.thr=0.7,
                         hQ.thr=0.2,
                         plot=TRUE,
                         log="y",
                         main="Flow Duration Curve",
                         xlab="% Time flow equalled or exceeded",
                         ylab="Q, [m3/s]",
                         ylim,
                         yat=c(0.01, 0.1, 1), 
                         xat=c(0.01, 0.025, 0.05),
                         col=matrix(c(rep("black", ncol(x)), 
                                    palette("default")[2:(ncol(x)+1)]), byrow=FALSE, ncol=2),
                         pch=matrix(rep(c(1, 15), ncol(x)), byrow=TRUE, ncol=2),
                         lwd=matrix(rep(c(1, 0.8), ncol(x)), byrow=TRUE, ncol=2),
                         lty=matrix(rep(c(1, 3), ncol(x)), byrow=TRUE, ncol=2),
                         cex=rep(0.1, ncol(x)),
                         cex.axis=1.2,
                         cex.lab=1.2,
                         leg.txt=c("OBS", colnames(x), "95PPU"),
                         leg.cex=1,
                         leg.pos="auto",
                         verbose= TRUE,
                         thr.shw=TRUE,
                         border=rep(NA, ncol(x)),
                         bands.col=rep("lightcyan", ncol(x)),
                         bands.density=rep(NULL, ncol(x)),
                         bands.angle=rep(45, ncol(x)),
                         new=TRUE,
                         ...) {

   x <- as.data.frame(x)

   NextMethod("fdcu", x,
                lband,
                uband,
                sim,
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
                lty=lty,
                cex=cex,
                cex.axis=cex.axis, 
                cex.lab=cex.lab,
                leg.txt=leg.txt,
                leg.cex=leg.cex, 
                leg.pos=leg.pos,
                verbose= verbose,
                border=border,
                bands.col=bands.col,
                bands.density=bands.density,
                bands.angle=bands.angle,
                ...)

} # 'fdcu.data.frame' END
