# File climograph.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
#                                 http://www.rforge.net/hydroTSM/
# Copyright 2016-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# climograph: Function for drawing a climograph based on precipitation and     #
#             temperature data.                                                #
#             Precipitation and temperature data used to build the climograph  #
#             should have a monthly time frequency. If the data provided by    #
#             have a time frequency higher than monthly (i.e., daily, subdaily)#
#             the function compute the monthly mean values and then draw the   #
#             climograph                                                       #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 29-Jun-2016                                                         #
# Updates: 30-Jun-2016 ; 04-Jul-2016                                           # 
#          08-May-2017 ; 09-May-2017                                           #
#          10-Mar-2020 ; 07-Nov-2020                                           #
#          May-2022    ; 20-Jun-2022                                           #
################################################################################
# 'pcp'      : variable of type 'zoo' with monthly, daily or subdaily          
#              precipitation data
# 'tmean'    : variable of type 'zoo' with monthly, daily or subdaily          
#              mean temperature data
# 'tmx'      : variable of type 'zoo' with monthly, daily or subdaily          
#              maximum temperature data. 
#              ONLY used (togheter with 'tmn') when 'tmean' is missing
# 'tmn'      : variable of type 'zoo' with monthly, daily or subdaily          
#              minimum temperature data. 
#              ONLY used (togheter with 'tmx') when 'tmean' is missing
# 'date.fmt' : format in which the dates are stored in 'from' and 'to'.
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA
# 'pcp.labels'  : logical. Should monthly precipitation values to be shown above the bars?
# 'tmean.labels': logical. Should monthly mean temperature values to be shown above the lines?
# 'tmx.labels'  : logical. Should monthly maximum temperature values to be shown above the lines?
# 'tmn.labels'  : logical. Should monthly minimum temperature values to be shown above the lines?

climograph <- function(pcp, tmean, tmx, tmn, na.rm=TRUE, 
                       from, to, date.fmt="%Y-%m-%d", 
                       main="Climograph", 
                       pcp.label="Precipitation, [mm]", 
                       tmean.label="Air temperature, [\U00B0 C]",

                       pcp.col="lightblue", 
                       tmean.col="darkred",
                       tmn.col="blue",
                       tmx.col="red",
                       pcp.labels=TRUE,
                       tmean.labels=TRUE,
                       tmx.labels=TRUE,
                       tmn.labels=TRUE,
                       pcp.labels.cex=0.8,
                       temp.labels.cex=0.8,
                       temp.labels.dx=c(rep(-0.2,6), rep(0.2,6)),
                       temp.labels.dy=rep(-0.4, 12),

                       lat, # [OPTIONAL] numeric or character used to show the latitude for which the climograph was plotted for
                       lon, # [OPTIONAL] numeric or character used to show the longitude for which the climograph was plotted for

                       plot.pcp.probs=TRUE,
                       pcp.probs=c(0.25, 0.75),

                       plot.temp.probs=TRUE,
                       temp.probs=c(0.25, 0.75),
                       temp.probs.col=c("#3399FF", "#FF9966", "#FFCC66"), # color of tmn, tmean, tmx
                       temp.probs.alpha=0.3
                       ) {


  .plotbands <- function(x, lband, uband, col="", border=NA) {
    t <- c(x, rev(x))
    bands <- c(as.numeric(lband), rev(as.numeric(uband)))
    polygon(t, bands, col=col, border=border)
  } # .plotbands END

  if (missing(pcp)) {
    stop("Missing argument: 'pcp' must be provided !")
  } else 
      # Checking that 'pcp' is a zoo object
      if ( !is.zoo(pcp) ) stop("Invalid argument: 'class(pcp)' must be in c('zoo', 'xts')")

  if (missing(tmean)) {
    if ( (missing(tmx)) & missing(tmn) ) {
      stop("Missing argument: 'tmean' | ('tmx' & 'tmn') must be provided !")
    } else {
        # Checking that 'tmx' and 'tmn' are zoo objects
        if ( !is.zoo(tmx) ) stop("Invalid argument: 'class(tmx)' must be in c('zoo', 'xts')")
        if ( !is.zoo(tmn) ) stop("Invalid argument: 'class(tmn)' must be in c('zoo', 'xts')")

        # Computing 'tmean'
        if ( all.equal(time(tmn), time(tmx)) ) {
          tmean <- (tmx+tmn)/2
        } else stop("Invalid argument: 'time(tmn) != time(tmx)' !")
      } # ELSE end
  } else 
      # Checking that 'tmean'is a zoo object
      if ( !is.zoo(tmean) ) stop("Invalid argument: 'class(tmean)' must be in c('zoo', 'xts')")

  # Checking the length of 'temp.labels.dx' and 'temp.labels.dy'
  if (length(temp.labels.dx) > 12) temp.labels.dx <- temp.labels.dx[1:12]
  if (length(temp.labels.dy) > 12) temp.labels.dy <- temp.labels.dy[1:12]

  ###########################################
  ## In case 'from' and 'to' are provided  ##
  dates.pcp  <- time(pcp)
  dates.temp <- time(tmean)
     
  # Checking the validity of the 'from' argument
  if (!missing(from)) { 
     from <- as.Date(from, format=date.fmt)

     if (from < dates.pcp[1])
       stop("Invalid argument: 'from' is lower than the first date in 'pcp' !")
     if (from < dates.temp[1])
       stop("Invalid argument: 'from' is lower than the first date in 'tmean' !")

     pcp   <- window(pcp, start=from)
     tmean <- window(tmean, start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     if (to > dates.pcp[length(pcp)])
       stop("Invalid argument: 'to' is greater than the last date in 'pcp' !")
     if (to > dates.temp[length(pcp)])
       stop("Invalid argument: 'to' is greater than the last date in 'tmean' !")

     pcp   <- window(pcp, end=to)
     tmean <- window(tmean, end=to)
   } # ELSE end


  ###########################################
  ## In case 'pcp' and 'tmean' are not average monthly values
  from <- time(pcp)[1]
  to   <- time(pcp)[length(pcp)]

  nyears <- yip(from=from, to=to, date.fmt="%Y-%m-%d", out.type="nmbr")

  if ( (sfreq(pcp) != "monthly") | ( (sfreq(pcp) == "monthly") & ( length(pcp) > 12) ) )
    pcp.m.avg <- monthlyfunction(pcp, FUN=sum, na.rm=na.rm) / nyears

  if ( (sfreq(tmean) != "monthly") | ( (sfreq(tmean) == "monthly") & ( length(tmean) > 12) ) )
    tmean.m.avg <- monthlyfunction(tmean, FUN=mean, na.rm=na.rm)

  if (plot.pcp.probs) {
    pcp.m    <- daily2monthly(pcp, FUN=sum, na.rm=na.rm)
    pcp.m.q1 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[1], na.rm=na.rm)
    pcp.m.q2 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[2], na.rm=na.rm)
  } # IF end

  if (plot.temp.probs) {
    temp.probs.col <- grDevices::adjustcolor(temp.probs.col, alpha.f=temp.probs.alpha)

    tmean.m    <- daily2monthly(tmean, FUN=mean, na.rm=na.rm)
    tmean.m.q1 <- monthlyfunction(tmean.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
    tmean.m.q2 <- monthlyfunction(tmean.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)

    if ( !missing(tmx) & !missing(tmn)) {
      tmx.m    <- daily2monthly(tmx, FUN=mean, na.rm=na.rm)
      tmn.m    <- daily2monthly(tmn, FUN=mean, na.rm=na.rm)
      tmx.m.q1 <- monthlyfunction(tmx.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
      tmx.m.q2 <- monthlyfunction(tmx.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)
      tmn.m.q1 <- monthlyfunction(tmn.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
      tmn.m.q2 <- monthlyfunction(tmn.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)
    } # IF end
  } # IF end

  #######################################
  # Drawing the climograph
  #######################################
  xlim <- c(0.5, 14.5)

  # Monthly precipitation as barplot
  if (plot.pcp.probs) {
    ylim <- range(pretty(pcp.m.avg), pretty(pcp.m.q1), pretty(pcp.m.q2))
  } else ylim <- range(pretty(pcp.m.avg))

  par(mar = c(7,5,3,5)) # c(bottom, left, top, right)
  x <- barplot(pcp.m.avg, col=pcp.col, xlim=xlim, ylim=ylim, ylab=pcp.label, las=1, main=main)

  #legend with lat and lon if they are provided
  legend.text.lab <- c("", "")
  legend.text.val <- c("", "")
  if (!missing(lat)) {
    legend.text.lab[1] <- "Lat:"
    legend.text.val[1] <- lat
  }  # IF end
  if (!missing(lon)) {
    legend.text.lab[2] <- "Lon:"
    legend.text.val[2] <- lon
  } # IF end
  legend("topright", paste(legend.text.lab, legend.text.val), bty="n",
         cex=1.2, ncol=1, title = "")

  # Adding error bars
  if (plot.pcp.probs)
    graphics::arrows(x0 = x, y0 = pcp.m.q2, y1 = pcp.m.q1, angle=90, code=3, length=0.1)

  grid()
  ifelse(plot.pcp.probs, deltax <- 0.25, deltax <- 0.0)
  if (pcp.labels) text(x-deltax, pcp.m.avg+2, cex=pcp.labels.cex, adj=0.5, labels= round(pcp.m.avg,1), col="black" )


  # If provided, computing monthly values of tmx and tmn, and the 
  # ylim for the secondary temperature axis
  if ( !missing(tmx) & !missing(tmn)) {
    if ( (sfreq(tmx) != "monthly") | ( (sfreq(tmx) == "monthly") & ( length(tmx) > 12) ) )
      tmx.m.avg <- monthlyfunction(tmx, FUN=mean, na.rm=na.rm)

    if ( (sfreq(tmn) != "monthly") | ( (sfreq(tmn) == "monthly") & ( length(tmn) > 12) ) )
      tmn.m.avg <- monthlyfunction(tmn, FUN=mean, na.rm=na.rm)

    if (plot.temp.probs) {
      ylim <- range(#pretty(tmx.m.avg), pretty(tmean.m.avg), pretty(tmn.m.avg),
                    pretty(tmx.m.q1), pretty(tmean.m.q1), pretty(tmn.m.q1),
                    pretty(tmx.m.q2), pretty(tmean.m.q2), pretty(tmn.m.q2)
                   )
    } else ylim <- range(pretty(tmx.m.avg), pretty(tmean.m.avg), pretty(tmn.m.avg))
  } else if (plot.temp.probs) {
           ylim <- range(pretty(tmean.m.q1), pretty(tmean.m.avg), pretty(tmean.m.q2))
         } else ylim <- range(pretty(tmean.m.avg))


  # Mean temperature as line
  par(new = TRUE, xpd=TRUE)
  if (plot.temp.probs) {
    plot(x, tmean.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
    .plotbands(x=x, lband=tmean.m.q1, uband=tmean.m.q2, col=temp.probs.col[2], border=NA)
    lines(x, tmean.m.avg, xlim=xlim, ylim=ylim, col= tmean.col, type = "o", lwd=3, pch=15, cex=1.4, bty = "n", xlab = "", ylab = "")
  } else plot(x, tmean.m.avg, xlim=xlim, ylim=ylim, col= tmean.col, type = "o", lwd=3, pch=15, cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")
  if (tmean.labels) text(x+temp.labels.dx, tmean.m.avg+temp.labels.dy, cex=temp.labels.cex, adj=0.5, labels= round(tmean.m.avg,1), col=tmean.col )

  # If provided, tmn as line
  if (!missing(tmn)) {
    par(new = TRUE, xpd=TRUE)
    if (plot.temp.probs) {
      plot(x, tmn.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
      .plotbands(x=x, lband=tmn.m.q1, uband=tmn.m.q2, col=temp.probs.col[1], border=NA) 
      lines(x, tmn.m.avg, xlim=xlim, ylim=ylim, col= tmn.col, type = "o", lwd=3, pch=15, cex=1.4, bty = "n", xlab = "", ylab = "")
    } else plot(x, tmn.m.avg, xlim=xlim, ylim=ylim, col= tmn.col, type = "o", lwd=3, pch=15, cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")

    if (tmn.labels) text(x+temp.labels.dx, tmn.m.avg+temp.labels.dy, cex=temp.labels.cex, adj=0.5, labels= round(tmn.m.avg,1), col=tmn.col )
  } # IF end

  # If provided, tmx as line
  if (!missing(tmx)) {
    par(new = TRUE, xpd=TRUE)
    if (plot.temp.probs) {
      plot(x, tmx.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
      .plotbands(x=x, lband=tmx.m.q1, uband=tmx.m.q2, col=temp.probs.col[3], border=NA)
      lines(x, tmx.m.avg, xlim=xlim, ylim=ylim, col= tmx.col, type = "o", lwd=3, pch=15, cex=1.4, bty = "n", xlab = "", ylab = "")
    } else plot(x, tmx.m.avg, xlim=xlim, ylim=ylim, col= tmx.col, type = "o", lwd=3, pch=15, cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")
    if (tmx.labels) text(x+temp.labels.dx, tmx.m.avg+temp.labels.dy, cex=temp.labels.cex, adj=0.5, labels= round(tmx.m.avg,1), col=tmx.col )
  } # IF end


  # Plotting temperature axis on the right hand side
  if ( !missing(tmx) & !missing(tmn)) {
    axis(side=4, at = pretty(range(tmn.m.avg, tmean.m.avg, tmx.m.avg)), las=1)
  } else axis(side=4, at = pretty(range(tmean.m.avg)), las=1)
  abline(h=axTicks(side=2), col="lightpink", lty = "dotted")
  text(par("usr")[2]*1.05,mean(par("usr")[3:4]), labels= tmean.label, srt = -90, xpd = TRUE, pos = 4)


  # Outter box and legend
  box()
  par(xpd=TRUE)
  if ( !missing(tmx) & !missing(tmn)) {
    legend("bottom", legend = c("Prec.", "Tmn", "Tmean", "Tmx"), bty="n",
           pch=c(15, 15, 15, 15), lty=c(NA, 1, 1, 1), cex=1.2, col=c(pcp.col, tmn.col, tmean.col, tmx.col), ncol=4, inset=c(0.5, -0.2),
           #lty = 1:2, xjust = 1, yjust = 1,
           title = "")
  } else
      legend("bottom", legend = c("Precipitation", "Temperature"), bty="n",
             pch=c(15, 15), lty=c(NA, 1), cex=1.2, col=c(pcp.col, tmean.col), ncol=2, inset=c(0.5, -0.2),
             #lty = 1:2, xjust = 1, yjust = 1,
             title = "")
  
} # 'climograph' END
