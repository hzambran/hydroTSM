# File monthlycurve.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2022-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

###################################################################################
# .plot_pq_mm.zoo: Function for drawing a figure with mean monthly precipitation  #
#                  and mean monthly streamflows (on the bottom)                   #
###################################################################################
# Originally this function was called 'monthlycurve', but afterwards it was       # 
# merged with the 'plot_pq' function                                              #
###################################################################################
# Author : Mauricio Zambrano-Bigiarini                                            #
###################################################################################
# Started: 26-Jul-2022                                                            #
# Updates: 22-Sep-2022 ; 11-Oct-2022 ; 25-Oct-2022                                #
#          22-Dec-2023                                                            #          
###################################################################################
# 'q'        : object of type 'zoo' with monthly, daily or subdaily streamflow data.
#              If q is a monthly zoo object, it must have 12 elments and it should be 
#              named with the names of the months (levels(time(q))), otherwise, 
#              automatic names will be asigned from Jan to Dec for each one of the 
#              12 monthly values
# 'date.fmt' : format in which the dates.q are stored in 'from' and 'to'.
# 'na.rm'    : Logical. Should missing values in 'q' be removed when using FUN?. 
#              It is also used when the optional argument 'p' is submonthly (e.g., daily, hourly), to decide whether missing values in the optional argument 'p' should be removed before aggregated into monthly scale
#              TRUE : the monthly values  are computed considering only those values in 'q' (and 'p') different from NA
#              FALSE: if 'q' (and 'p') has AT LEAST one NA within a month, the corresponding monthly values are NA

.plot_pq_mm.zoo <- function(p,
                            q, 
                            
                            #na.rm=TRUE, 
                            na.fill=c("remove", "linear", "spline"), 
                            
                            from, 
                            to, 
                            
                            date.fmt, 
                            tz,
                            
                            main="Precipitation and Streamflows", 
                            xlab="Month",
                            ylab=c("P, [mm]", "Q, [m3/s]"),
                            cols=c("lightskyblue1", "blue"),
                            
                            FUN=mean, # function used to aggregate 'x' from a submonthly time frquency (e.g., daily, hourly) into a monthly time frequency. It must support the 'na.rm' argument
                            start.month=1,

                            plot.q.probs=TRUE,
                            q.probs=c(0.25, 0.75),
                            q.probs.col="lightskyblue1",
                            q.probs.alpha=0.8,
                         
                            plot.p.probs=TRUE,
                            p.probs=c(0.25, 0.75),
                            p.alpha=0.8,
                            
                            labels=TRUE,
                            labels.cex=0.8,
                            labels.q.dx=c(rep(-0.2,6), rep(0.2,6)),
                            labels.q.dy=rep(-median(q, na.rm=TRUE)/10, 12),
                            labels.p.dy=ifelse(missing(p), NA, -median(daily2monthly(p, FUN=sum, na.rm=TRUE), na.rm=TRUE)/10)
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
    if ( is.zoo(x) ) {
      x.raw    <- zoo::coredata(x)
      x.labels <- as.character(time(x))
      out        <- x.raw[match(index.old, index.new)] 
      names(out) <- x.labels[match(index.old, index.new)] 
    } else out <- x[match(index.old, index.new)] 
    return(out)
  } # .shift END
   
  if (!missing(FUN))
    FUN <- match.fun(FUN)
  ###########################################
  ## In case 'from' and 'to' are provided  ##
    
   dates.q  <- time(q)

  # Checking the validity of the 'from' argument
  if (!missing(from)) { 
     from <- as.Date(from, format=date.fmt)

     q <- window(q, start=from)

     if (!missing(p)) p <- window(p, start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     q <- window(q, end=to)

     if (!missing(p)) p <- window(p, end=to)
   } # ELSE end


  # Checking that 'q' and 'p' have the same dates
  dates.q  <- time(q)
  if (!missing(p)) {
    dates.p <- time(p)
    if (!all.equal(dates.q, dates.p))
      stop("Invalid arguments: 'dates(q)' must be equal to 'dates(p)' !!")
  } # IF end

  ###########################################
  ## In case 'q' is not average monthly values
  from <- time(q)[1]
  to   <- time(q)[length(q)]

  if ( (sfreq(q) != "monthly") ) {
    q.m    <- daily2monthly(q, FUN=FUN, na.rm=TRUE)
  } else q.m <- q

  if ( (sfreq(q) != "monthly") | ( (sfreq(q) == "monthly") & ( length(q) > 12) ) ) {
    q.m.med     <- monthlyfunction(q.m, FUN=quantile, probs=0.5, na.rm=TRUE)
    month.names <- levels(time(q.m.med))
  } else {
      q.m.med     <- q
      month.names <- levels(time(q))
      if (is.null(month.names))
        month.names <- month.abb
    } # ELSE end  

  q.m.q1 <- monthlyfunction(q.m, FUN=quantile, probs=q.probs[1], na.rm=TRUE)
  q.m.q2 <- monthlyfunction(q.m, FUN=quantile, probs=q.probs[2], na.rm=TRUE)

  if (start.month != 1) q.m.med     <- .shift(x=q.m.med    , imonth=start.month)
  if (start.month != 1) q.m.q1      <- .shift(x=q.m.q1     , imonth=start.month)
  if (start.month != 1) q.m.q2      <- .shift(x=q.m.q2     , imonth=start.month)
  if (start.month != 1) month.names <- .shift(x=month.names, imonth=start.month)

  if (!missing(p)) {

    if ( (sfreq(q) != "monthly") ) {
      p.m <- daily2monthly(p, FUN=sum, na.rm=TRUE)
    } else p.m <- p

    if ( (sfreq(p) != "monthly") | ( (sfreq(p) == "monthly") & ( length(p) > 12) ) ) {
      p.m.med <- monthlyfunction(p.m, FUN=quantile, probs=0.5, na.rm=TRUE)
    } else p.m.med <- p

    p.m.q1 <- monthlyfunction(p.m, FUN=quantile, probs=p.probs[1], na.rm=TRUE)
    p.m.q2 <- monthlyfunction(p.m, FUN=quantile, probs=p.probs[2], na.rm=TRUE)

    if (start.month != 1) p.m.med <- .shift(x=p.m.med, imonth=start.month)
    if (start.month != 1) p.m.q1  <- .shift(x=p.m.q1 , imonth=start.month)
    if (start.month != 1) p.m.q2  <- .shift(x=p.m.q2 , imonth=start.month)
  } # IF end

  
  # the next line is required just in case a previous plot modified the graphical 'layout'
  par(mfrow=c(1,1)) 

  ##############################################################################
  # Definining the plotting area (1 column, 2 rows), where the lower row has 
  # a height 3 times larger than the upper window, AND 
  # Plotting the Monthly Precipitation in the upper panel
  ##############################################################################
  if ( !missing(p) ) {
    par(mar=c(3, 4.1, 3, 1.5), xpd=TRUE) # default  c(5.1, 4.1, 4.1, 2.1)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=5, heights=c(1,3))  
 
    ylim <- range(pretty(p.m.q1), pretty(p.m.q2))
    x <- barplot(p.m.med, ylim=rev(ylim), xlab="", ylab=ylab[1], axes=TRUE, col=col[1], names.arg=month.names, main=main)
    #axis(side=1, at=lx, labels=month.names, line=0.02, outer=TRUE, pos=1)
    if (labels) text(x, labels.p.dy, cex=labels.cex, adj=0.5, labels= round(p.m.med,1), col="black")

    # Adding error bars
    if (plot.p.probs) 
      graphics::arrows(x0 = x, y0 = p.m.q2, y1 = p.m.q1, angle=90, code=3, length=0.1)
  } # IF end


  #######################################
  # 2nd Figure: Drawing the monthly curve
  #######################################
  if (!missing(p)) {
    par(mar=c(3, 4.1, 0.5, 1.5)) # default  c(5.1, 4.1, 4.1, 2.1)
    main <- ""
  } else par(mar=c(3, 4.1, 4.1, 1.5))
  lubands.col <- grDevices::adjustcolor(q.probs.col, alpha.f=q.probs.alpha)
  xlim <- c(0.5, 12.5)
  ylim <- range(pretty(q.m.q1), pretty(q.m.q2))

  # Monthly values as lines
  lx   <- 1:12 
  plot(lx, q.m.med, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab[2], type="n", axes=TRUE, xaxt="n", main=main)
  #plot(lx, q.m.med, xlim=xlim, ylim=ylim, col= col[2], type="o", lwd=3, pch=15, cex=1.4, axes=TRUE, xaxt="n", xlab=xlab, ylab=ylab[2])

  if (plot.q.probs) 
    .plotbands(x=lx, lband=q.m.q1, uband=q.m.q2, col=lubands.col, border=NA)
  
  grid()
  lines(lx, q.m.med, xlim=xlim, ylim=ylim, col= col[2], type = "o", lwd=3, pch=15, cex=1.4)
  axis(side=1, at=lx, labels=month.names)
  if (labels) text(lx+labels.q.dx, q.m.med+labels.q.dy, cex=labels.cex, adj=0.5, labels= round(q.m.med,1), col="black" )
 
} # 'monthlycurve' END
