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
# Updates.x:                                                                        #
###################################################################################
# 'x'        : object of type 'zoo' with monthly, daily or subdaily streamflow data.
#              If x is a monthly zoo object, it must have 12 elments and it should be 
#              named with the names of the months (levels(time(x))), otherwise, 
#              automatic names will be asigned from Jan to Dec for each one of the 
#              12 monthly values
# 'date.fmt' : format in which the dates.x are stored in 'from' and 'to'.
# 'na.rm'    : Logical. Should missing values in 'x' be removed when using FUN?. 
#              It is also used when the optional argument 'pcp' is submonthly (e.g., daily, hourly), to decide whether missing values in the optional argument 'pcp' should be removed before aggregated into monthly scale
#              TRUE : the monthly values  are computed considering only those values in 'x' (and 'pcp') different from NA
#              FALSE: if 'x' (and 'pcp') has AT LEAST one NA within a month, the corresponding monthly values are NA

monthlycurve <- function(x, 
                         pcp,
                         na.rm=TRUE, 
                         from, 
                         to, 
                         date.fmt="%Y-%m-%d", 
                         main=ifelse(missing(pcp), "Monthly Streamflows", "Monthly P and Q"), 
                         FUN=mean, # function used to aggregate 'x' from a submonthly time frquency (e.g., daily, hourly) into a monthly time frequency. It must support the 'na.rm' argument
                         start.month=1,
                         x.col="blue", 
                         ubands.probs=c(0.25, 0.75),
                         ubands.col="lightskyblue1",
                         ubands.alpha=0.8,
                         
                         pcp.probs=c(0.25, 0.75),
                         pcp.col="lightskyblue1",
                         pcp.alpha=0.8,
                         pcp.ylab="P, [mm]",

                         xlab="Month",
                         ylab="Q, [m3/s]",
                         
                         labels=TRUE,
                         labels.cex=0.8,
                         labels.dx=c(rep(-0.2,6), rep(0.2,6)),
                         labels.dy=rep(-median(x, na.rm=TRUE)/10, 12)
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

  if (missing(x)) {
    stop("Missing argument: 'x' must be provided !")
  } else 
      # Checking that 'x' is a zoo object
      if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be in c('zoo', 'xts')")
   
  if (!missing(FUN))
    FUN <- match.fun(FUN)
  ###########################################
  ## In case 'from' and 'to' are provided  ##
  dates.x  <- time(x)
  if (!missing(pcp)) {
    dates.pcp <- time(pcp)
    if (!all.equal(dates.x, dates.pcp))
      stop("Invalid arguments: 'dates(x)' must be equal to 'dates(pcp)' !!")
  } # IF end
     
  # Checking the validity of the 'from' argument
  if (!missing(from)) { 
     from <- as.Date(from, format=date.fmt)

     if (from < dates.x[1])
       stop("Invalid argument: 'from' is lower than the first date in 'x' !")

     x <- window(x, start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     if (to > dates.x[length(x)])
       stop("Invalid argument: 'to' is greater than the last date in 'x' !")

     x <- window(x, end=to)
   } # ELSE end

  ###########################################
  ## In case 'x' is not average monthly values
  from <- time(x)[1]
  to   <- time(x)[length(x)]

  if ( (sfreq(x) != "monthly") ) {
    x.m    <- daily2monthly(x, FUN=FUN, na.rm=na.rm)
  } else x.m <- x

  if ( (sfreq(x) != "monthly") | ( (sfreq(x) == "monthly") & ( length(x) > 12) ) ) {
    x.m.med     <- monthlyfunction(x.m, FUN=quantile, probs=0.5, na.rm=na.rm)
    month.names <- levels(time(x.m.med))
  } else {
      x.m.med     <- x
      month.names <- levels(time(x))
      if (is.null(month.names))
        month.names <- month.abb
    } # ELSE end  

  x.m.q1 <- monthlyfunction(x.m, FUN=quantile, probs=ubands.probs[1], na.rm=na.rm)
  x.m.q2 <- monthlyfunction(x.m, FUN=quantile, probs=ubands.probs[2], na.rm=na.rm)
  if (start.month != 1) x.m.med     <- .shift(x=x.m.med    , imonth=start.month)
  if (start.month != 1) x.m.q1      <- .shift(x=x.m.q1     , imonth=start.month)
  if (start.month != 1) x.m.q2      <- .shift(x=x.m.q2     , imonth=start.month)
  if (start.month != 1) month.names <- .shift(x=month.names, imonth=start.month)

  if (!missing(pcp)) {
    if (!missing(from)) pcp <- window(pcp, start=from)
    if (!missing(to))   pcp <- window(pcp, end=to)

    if ( (sfreq(x) != "monthly") ) {
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

  ##############################################################################
  # Definining theplotting area (2 rows and 1 column, 
  # where the lower window has a height 3 time larger than the upper window
  ##############################################################################
  # the next line is required just in case a previous plot modified the graphical 'layout'
  par(mfrow=c(1,1)) 

  if (!missing(pcp)) {
    par(mar=c(3, 4.1, 3, 1.5)) # default  c(5.1, 4.1, 4.1, 2.1)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=5, heights=c(1,3))  
 
    ylim <- range(pretty(pcp.m.q1), pretty(pcp.m.q2))
    barplot(pcp.m.med, ylim=rev(ylim), xlab="", ylab=pcp.ylab, axes=TRUE, col=pcp.col, names.arg=month.names, main=main)
    #axis(side=1, at=lx, labels=month.names, line=0.02, outer=TRUE, pos=1)
  } # IF end
  #######################################
  # 2nd Figure: Drawing the monthly curve
  #######################################
  if (!missing(pcp)) {
    par(mar=c(3, 4.1, 0.5, 1.5)) # default  c(5.1, 4.1, 4.1, 2.1)
    main <- ""
  } else par(mar=c(3, 4.1, 4.1, 1.5))
  lubands.col <- grDevices::adjustcolor(ubands.col, alpha.f=ubands.alpha)
  xlim <- c(0.5, 12.5)
  ylim <- range(pretty(x.m.q1), pretty(x.m.q2))

  # Monthly values as lines
  lx   <- 1:12 
  plot(lx, x.m.med, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n", axes=TRUE, xaxt="n", main=main)
  #plot(lx, x.m.med, xlim=xlim, ylim=ylim, col= x.col, type="o", lwd=3, pch=15, cex=1.4, axes=TRUE, xaxt="n", xlab=xlab, ylab=ylab)
  .plotbands(x=lx, lband=x.m.q1, uband=x.m.q2, col=lubands.col, border=NA)
  grid()
  lines(lx, x.m.med, xlim=xlim, ylim=ylim, col= x.col, type = "o", lwd=3, pch=15, cex=1.4)
  axis(side=1, at=lx, labels=month.names)
  if (labels) text(lx+labels.dx, x.m.med+labels.dy, cex=labels.cex, adj=0.5, labels= round(x.m.med,1), col=x.col )
  
} # 'monthlycurve' END
