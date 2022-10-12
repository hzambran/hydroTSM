# File monthlycurve.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2022-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

###################################################################################
# monthlycurve: Function for drawing a climograph based on precipitation and      #
#                temperature data.                                                #
#                Precipitation and temperature data used to build the climograph  #
#                should have a monthly time frequency. If the data provided by    #
#                have a time frequency higher than monthly (i.e., daily, subdaily)#
#                the function compute the monthly mean values and then draw the   #
#                climograph                                                       #
###################################################################################
# Author : Mauricio Zambrano-Bigiarini                                            #
###################################################################################
# Started: 26-Jul-2022                                                            #
# Updates: 22-Sep-2022 ; 11-Oct-2022                                              #
###################################################################################
# 'q'        : object of type 'zoo' with monthly, daily or subdaily streamflow data.
#              If q is a monthly zoo object, it must have 12 elments and it should be 
#              named with the names of the months (levels(time(q))), otherwise, 
#              automatic names will be asigned from Jan to Dec for each one of the 
#              12 monthly values
# 'date.fmt' : format in which the dates.q are stored in 'from' and 'to'.
# 'na.rm'    : Logical. Should missing values in 'q' be removed when using FUN?. 
#              It is also used when the optional argument 'pcp' is submonthly (e.g., daily, hourly), to decide whether missing values in the optional argument 'pcp' should be removed before aggregated into monthly scale
#              TRUE : the monthly values  are computed considering only those values in 'q' (and 'pcp') different from NA
#              FALSE: if 'q' (and 'pcp') has AT LEAST one NA within a month, the corresponding monthly values are NA

monthlycurve <- function(q, 
                         pcp,
                         na.rm=TRUE, 
                         from, 
                         to, 
                         date.fmt="%Y-%m-%d", 
                         main=ifelse(missing(pcp), "Monthly Streamflows", "Monthly P and Q"), 
                         FUN=mean, # function used to aggregate 'x' from a submonthly time frquency (e.g., daily, hourly) into a monthly time frequency. It must support the 'na.rm' argument
                         start.month=1,

                         q.col="blue", 

                         plot.q.probs=TRUE,
                         q.probs=c(0.25, 0.75),
                         q.probs.col="lightskyblue1",
                         q.probs.alpha=0.8,
                         
                         plot.pcp.probs=TRUE,
                         pcp.probs=c(0.25, 0.75),
                         pcp.col="lightskyblue1",
                         pcp.alpha=0.8,

                         pcp.ylab="P, [mm]",

                         xlab="Month",
                         ylab="Q, [m3/s]",
                         
                         labels=TRUE,
                         labels.cex=0.8,
                         labels.dx=c(rep(-0.2,6), rep(0.2,6)),
                         labels.dy=rep(-median(q, na.rm=TRUE)/10, 12)
                         ) {

  .plotbands <- function(x, lband, uband, col="", border=NA) {
    t <- c(x, rev(x))
    bands <- c(as.numeric(lband), rev(as.numeric(uband)))
    polygon(t, bands, col=col, border=border)
  } # .plotbands END

  .shift <- function(x, imonth) {
    L <- length(x)
    if (imonth>L) stop("[ Invalid value: 'imonth' can not be larger than ", L, " !]")
    delta <- imonth-1
    index.old <- 1:L
    index.new <- index.old-delta
    neg <- which(index.new <=0)
    index.new[neg] <- index.new[neg]+L
    if ( is.zoo(x) == "zoo" ) {
      x.raw    <- zoo::coredata(x)
      x.labels <- as.character(time(x))
      out        <- x.raw[match(index.old, index.new)] 
      names(out) <- x.labels[match(index.old, index.new)] 
    } else out <- x[match(index.old, index.new)] 
    return(out)
  } # .shift END

  if (missing(q)) {
    stop("Missing argument: 'q' must be provided !")
  } else 
      # Checking that 'q' is a zoo object
      if ( !is.zoo(q) ) stop("Invalid argument: 'class(q)' must be in c('zoo', 'xts')")
   
  if (!missing(FUN))
    FUN <- match.fun(FUN)
  ###########################################
  ## In case 'from' and 'to' are provided  ##
    
   dates.q  <- time(q)

  # Checking the validity of the 'from' argument
  if (!missing(from)) { 
     from <- as.Date(from, format=date.fmt)

     if (from < dates.q[1])
       stop("Invalid argument: 'from' is lower than the first date in 'q' !")

     q <- window(q, start=from)

     if (!missing(pcp)) pcp <- window(pcp, start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     if (to > dates.q[length(q)])
       stop("Invalid argument: 'to' is greater than the last date in 'x' !")

     q <- window(q, end=to)

     if (!missing(pcp)) pcp <- window(pcp, end=to)
   } # ELSE end


  # Checking that 'q' and 'pcp' have the same dates
  dates.q  <- time(q)
  if (!missing(pcp)) {
    dates.pcp <- time(pcp)
    if (!all.equal(dates.q, dates.pcp))
      stop("Invalid arguments: 'dates(q)' must be equal to 'dates(pcp)' !!")
  } # IF end

  ###########################################
  ## In case 'q' is not average monthly values
  from <- time(q)[1]
  to   <- time(q)[length(q)]

  if ( (sfreq(q) != "monthly") ) {
    q.m    <- daily2monthly(q, FUN=FUN, na.rm=na.rm)
  } else q.m <- q

  if ( (sfreq(q) != "monthly") | ( (sfreq(q) == "monthly") & ( length(q) > 12) ) ) {
    q.m.med     <- monthlyfunction(q.m, FUN=quantile, probs=0.5, na.rm=na.rm)
    month.names <- levels(time(q.m.med))
  } else {
      q.m.med     <- q
      month.names <- levels(time(q))
      if (is.null(month.names))
        month.names <- month.abb
    } # ELSE end  

  q.m.q1 <- monthlyfunction(q.m, FUN=quantile, probs=q.probs[1], na.rm=na.rm)
  q.m.q2 <- monthlyfunction(q.m, FUN=quantile, probs=q.probs[2], na.rm=na.rm)

  if (start.month != 1) q.m.med     <- .shift(x=q.m.med    , imonth=start.month)
  if (start.month != 1) q.m.q1      <- .shift(x=q.m.q1     , imonth=start.month)
  if (start.month != 1) q.m.q2      <- .shift(x=q.m.q2     , imonth=start.month)
  if (start.month != 1) month.names <- .shift(x=month.names, imonth=start.month)

  if (!missing(pcp)) {

    if ( (sfreq(q) != "monthly") ) {
      pcp.m <- daily2monthly(pcp, FUN=sum, na.rm=na.rm)
    } else pcp.m <- pcp

    if ( (sfreq(pcp) != "monthly") | ( (sfreq(pcp) == "monthly") & ( length(pcp) > 12) ) ) {
      pcp.m.med <- monthlyfunction(pcp.m, FUN=quantile, probs=0.5, na.rm=na.rm)
    } else pcp.m.med <- pcp

    pcp.m.q1 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[1], na.rm=na.rm)
    pcp.m.q2 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[2], na.rm=na.rm)

    if (start.month != 1) pcp.m.med <- .shift(x=pcp.m.med, imonth=start.month)
    if (start.month != 1) pcp.m.q1  <- .shift(x=pcp.m.q1 , imonth=start.month)
    if (start.month != 1) pcp.m.q2  <- .shift(x=pcp.m.q2 , imonth=start.month)
  } # IF end

  
  # the next line is required just in case a previous plot modified the graphical 'layout'
  par(mfrow=c(1,1)) 

  ##############################################################################
  # Definining the plotting area (1 column, 2 rows), where the lower row has 
  # a height 3 times larger than the upper window, AND plotting the monthly 
  # precipitation
  ##############################################################################
  if ( !missing(pcp) ) {
    par(mar=c(3, 4.1, 3, 1.5), xpd=TRUE) # default  c(5.1, 4.1, 4.1, 2.1)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=5, heights=c(1,3))  
 
    ylim <- range(pretty(pcp.m.q1), pretty(pcp.m.q2))
    x <- barplot(pcp.m.med, ylim=rev(ylim), xlab="", ylab=pcp.ylab, axes=TRUE, col=pcp.col, names.arg=month.names, main=main)
    #axis(side=1, at=lx, labels=month.names, line=0.02, outer=TRUE, pos=1)
    if (labels) text(x, -7, cex=labels.cex, adj=0.5, labels= round(pcp.m.med,1), col="black")

    # Adding error bars
    if (plot.pcp.probs) 
      graphics::arrows(x0 = x, y0 = pcp.m.q2, y1 = pcp.m.q1, angle=90, code=3, length=0.1)
  } # IF end


  #######################################
  # 2nd Figure: Drawing the monthly curve
  #######################################
  if (!missing(pcp)) {
    par(mar=c(3, 4.1, 0.5, 1.5)) # default  c(5.1, 4.1, 4.1, 2.1)
    main <- ""
  } else par(mar=c(3, 4.1, 4.1, 1.5))
  lubands.col <- grDevices::adjustcolor(q.probs.col, alpha.f=q.probs.alpha)
  xlim <- c(0.5, 12.5)
  ylim <- range(pretty(q.m.q1), pretty(q.m.q2))

  # Monthly values as lines
  lx   <- 1:12 
  plot(lx, q.m.med, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n", axes=TRUE, xaxt="n", main=main)
  #plot(lx, q.m.med, xlim=xlim, ylim=ylim, col= q.col, type="o", lwd=3, pch=15, cex=1.4, axes=TRUE, xaxt="n", xlab=xlab, ylab=ylab)

  if (plot.q.probs) 
    .plotbands(x=lx, lband=q.m.q1, uband=q.m.q2, col=lubands.col, border=NA)
  
  grid()
  lines(lx, q.m.med, xlim=xlim, ylim=ylim, col= q.col, type = "o", lwd=3, pch=15, cex=1.4)
  axis(side=1, at=lx, labels=month.names)
  if (labels) text(lx+labels.dx, q.m.med+labels.dy, cex=labels.cex, adj=0.5, labels= round(q.m.med,1), col=q.col )
 
} # 'monthlycurve' END
