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
#          May-2022    ; 20-Jun-2022 ; 22-Aug-2022 ; 05-Oct-2022               #
#          25-May-2023 ; 26-May-2023 ; 27-May-2023                             #
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
# 'pcp.solid.thr' :[OPTIONAL]. Only used when using (sub)daily precipitation and 
#                   temperature are gives as input data. \cr
#                  numeric, indicating the temperature, in degrees Celsius, used to 
#                  discriminate between solid and liquid precipitation. \cr
#                  When daily \code{tmean <= pcp.solid.thr} the precipitation for that
#                  day is considered as solid precipitation.

# 'pcp.labels'  : logical. Should monthly precipitation values to be shown above the bars?
# 'tmean.labels': logical. Should monthly mean temperature values to be shown above the lines?
# 'tmx.labels'  : logical. Should monthly maximum temperature values to be shown above the lines?
# 'tmn.labels'  : logical. Should monthly minimum temperature values to be shown above the lines?

climograph <- function(pcp, tmean, tmx, tmn, na.rm=TRUE, 
                       from, to, date.fmt="%Y-%m-%d", 
                       main="Climograph", 
                       pcp.label="Precipitation, [mm]", 
                       tmean.label="Air temperature, [\U00B0 C]",
                       start.month=1,

                       pcp.solid.thr, 

                       pcp.ylim, # if provided, used to define the range of the P axis
                       temp.ylim,# if provided, used to define the range of the Temp axis

                       pcp.col="lightblue", 
                       pcp.solid.col="skyblue2",
                       tmean.col="darkred",
                       tmn.col="blue",
                       tmx.col="red",

                       pcp.labels=TRUE,
                       tmean.labels=TRUE,
                       tmx.labels=TRUE,
                       tmn.labels=TRUE,
                       pcp.labels.cex=0.8,
                       temp.labels.cex=0.8,

                       pcp.labels.dx=c(rep(ifelse(plot.pcp.probs, -0.25,  0.0),6), 
                                       rep(ifelse(plot.pcp.probs, -0.25,  0.0),6)),
                       pcp.labels.dy=rep(2, 12),
                       temp.labels.dx=c(rep(-0.2,6), rep(0.2,6)),
                       temp.labels.dy=rep(-0.4, 12),

                       plot.pcp.probs=TRUE,
                       pcp.probs=c(0.25, 0.75),

                       plot.temp.probs=TRUE,
                       temp.probs=c(0.25, 0.75),

                       temp.probs.col=c("#3399FF", "#FF9966", "#FFCC66"), # color of tmn, tmean, tmx
                       temp.probs.alpha=0.3,

                       lat, # [OPTIONAL] numeric or character used to show the latitude for which the climograph was plotted for
                       lon  # [OPTIONAL] numeric or character used to show the longitude for which the climograph was plotted for
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
    if (inherits(x, "zoo")) {
      x.raw    <- zoo::coredata(x)
      x.labels <- as.character(time(x))
      out        <- x.raw[match(index.old, index.new)] 
      names(out) <- x.labels[match(index.old, index.new)] 
    } else out <- x[match(index.old, index.new)] 
    return(out)
  } # .shift END


  if (missing(pcp)) {
    stop("Missing argument: 'pcp' must be provided !")
  } else {
      # Checking that 'pcp' is a zoo object
      if ( !is.zoo(pcp) ) stop("Invalid argument: 'class(pcp)' must be in c('zoo', 'xts')")

      # Checking that 'pcp' is a subdaily or monthly object
      if ( sfreq(pcp) %in% c("quarterly", "annual")) stop("Invalid argument: 'sfreq(pcp)' must be in c('minute', 'hourly', 'daily', 'weekly')")
    } # ELSE end


  # Checking that 'tmean' is provided, and if not, it computes it using 'tmx' and 'tmn'  
  if (missing(tmean)) {
    if ( (missing(tmx)) & missing(tmn) ) {
      stop("Missing argument: 'tmean' | ('tmx' & 'tmn') must be provided !")
    } else {
        # Checking that 'tmx' and 'tmn' are zoo objects
        if ( !is.zoo(tmx) ) stop("Invalid argument: 'class(tmx)' must be in c('zoo', 'xts')")
        if ( !is.zoo(tmn) ) stop("Invalid argument: 'class(tmn)' must be in c('zoo', 'xts')")

        # Checking that 'tmx' is a subdaily or monthly object
        if ( sfreq(tmx) %in% c("quarterly", "annual")) stop("Invalid argument: 'sfreq(tmx)' must be in c('minute', 'hourly', 'daily', 'weekly')")
        # Checking that 'tmn' is a subdaily or monthly object
        if ( sfreq(tmn) %in% c("quarterly", "annual")) stop("Invalid argument: 'sfreq(tmn)' must be in c('minute', 'hourly', 'daily', 'weekly')")

        # Computing 'tmean'
        if ( all.equal(time(tmn), time(tmx)) ) {
          tmean <- (tmx+tmn)/2
        } else stop("Invalid argument: 'time(tmn) != time(tmx)' !")
      } # ELSE end
  } else {
      # Checking that 'tmean'is a zoo object
      if ( !is.zoo(tmean) ) stop("Invalid argument: 'class(tmean)' must be in c('zoo', 'xts')")

      # Checking that 'tmean' is a subdaily or monthly object
      if ( sfreq(tmean) %in% c("quarterly", "annual")) stop("Invalid argument: 'sfreq(tmean)' must be in c('minute', 'hourly', 'daily', 'weekly')")
    } # ELSE end


  # Computing (sub)daily solid precipitation, only when 'pcp.solid.thr' is provided
  pcp.solid.exists <- FALSE
  if ( !missing(pcp.solid.thr) & !(sfreq(pcp) %in% c("monthly", "quarterly", "annual") ) & 
       !(sfreq(tmean) %in% c("monthly", "quarterly", "annual") ) ) {
  
    pcp.solid.exists       <- TRUE
    pcp.solid              <- pcp*NA # it keeps the time and all the elements are made equal to NA
    solid.index            <- which(tmean <= pcp.solid.thr)
    pcp.solid[solid.index] <- pcp[solid.index]
  } # IF end


  # Checking the length of 'temp.labels.dx' and 'temp.labels.dy'
  if (length(temp.labels.dx) > 12) temp.labels.dx <- temp.labels.dx[1:12]
  if (length(temp.labels.dy) > 12) temp.labels.dy <- temp.labels.dy[1:12]

  # checking 'pcp.ylim', if provided 
  if (!missing(pcp.ylim)) {
    if (length(pcp.ylim) != 2) stop("Invalid argument: 'length(pcp.ylim)' must be 2 !")
    if (pcp.ylim[2] <= 1) stop("Invalid argument: 'pcp.ylim[2]' must be larger than pcp.ylim[1] !")
  } # IF end

  # checking 'temp.ylim', if provided 
  if (!missing(temp.ylim)) {
    if (length(temp.ylim) != 2) stop("Invalid argument: 'length(temp.ylim)' must be 2 !")
    if (temp.ylim[2] <= 1) stop("Invalid argument: 'temp.ylim[2]' must be larger than temp.ylim[1] !")
  } # IF end

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

     pcp   <- window(pcp  , start=from)
     tmean <- window(tmean, start=from)

     if (pcp.solid.exists) pcp.solid <- window(pcp.solid, start=from)
     if ( !missing(tmx) )  tmx       <- window(tmx      , start=from)
     if ( !missing(tmn) )  tmn       <- window(tmn      , start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     if (to > dates.pcp[length(pcp)])
       stop("Invalid argument: 'to' is greater than the last date in 'pcp' !")
     if (to > dates.temp[length(pcp)])
       stop("Invalid argument: 'to' is greater than the last date in 'tmean' !")

     pcp   <- window(pcp  , end=to)
     tmean <- window(tmean, end=to)

     if (pcp.solid.exists) pcp.solid <- window(pcp.solid, end=to)
     if ( !missing(tmx) )  tmx       <- window(tmx      , end=to)
     if ( !missing(tmn) )  tmn       <- window(tmn      , end=to)
   } # ELSE end


  # Detecting if 'pcp' and 'tmean', 'tmx' 'tmn' are already mean monthly values (i.e., 12 values)
  pcp.is.mean.monthly   <- FALSE
  tmean.is.mean.monthly <- FALSE
  tmx.is.mean.monthly   <- FALSE
  tmn.is.mean.monthly   <- FALSE

  if ( ( sfreq(pcp) == "monthly" ) & (length(pcp) == 12) ) {
    pcp.is.mean.monthly <- TRUE

    months     <- format(time(pcp), "%b")
    pcp        <- as.numeric(pcp)
    names(pcp) <- months

    if (start.month != 1)
      pcp <- .shift(x=pcp, imonth=start.month)

    pcp.m.avg <- pcp
    pcp.m.q1  <- pcp
    pcp.m.q2  <- pcp    
  } # IF enbd

  if ( ( sfreq(tmean) == "monthly" ) & (length(tmean) == 12) ) {
    tmean.is.mean.monthly <- TRUE

    months       <- format(time(tmean), "%b")
    tmean        <- as.numeric(tmean)
    names(tmean) <- months

    if (start.month != 1)
      tmean <- .shift(x=tmean, imonth=start.month)

    tmean.m.avg <- tmean
    tmean.m.q1  <- tmean
    tmean.m.q2  <- tmean
  } # IF end

  if (!missing(tmx)) {
    if ( ( sfreq(tmx) == "monthly" ) & (length(tmx) == 12) ) {
      tmx.is.mean.monthly <- TRUE

      months     <- format(time(tmx), "%b")
      tmx        <- as.numeric(tmx)
      names(tmx) <- months

      if (start.month != 1)
        tmx <- .shift(x=tmx, imonth=start.month)

      tmx.m.avg <- tmx
      tmx.m.q1  <- tmx
      tmx.m.q2  <- tmx
    } # IF end
  } # IF end

  if (!missing(tmn)) {
    if ( ( sfreq(tmn) == "monthly" ) & (length(tmn) == 12) ) {
      tmn.is.mean.monthly <- TRUE

      months     <- format(time(tmn), "%b")
      tmn        <- as.numeric(tmn)
      names(tmn) <- months

      if (start.month != 1)
        tmn <- .shift(x=tmn, imonth=start.month)

      tmn.m.avg <- tmn
      tmn.m.q1  <- tmn
      tmn.m.q2  <- tmn
    } # IF end
  } # IF end

  
  ###########################################
  ## In case 'pcp', 'tmean' ('tmx' and 'tmn') were not given as average monthly values
  if ( (!pcp.is.mean.monthly) & (!tmean.is.mean.monthly) ) {

    from <- time(pcp)[1]
    to   <- time(pcp)[length(pcp)]

    nyears <- yip(from=from, to=to, date.fmt="%Y-%m-%d", out.type="nmbr")

    # Computing mean monthly values of 'pcp'
    if ( (sfreq(pcp) != "monthly") | ( (sfreq(pcp) == "monthly") & ( length(pcp) > 12) ) )
      pcp.m.avg <- monthlyfunction(pcp, FUN=sum, na.rm=na.rm) / nyears

    # Computing mean monthly values of 'pcp.solid', only if it exists
    if (pcp.solid.exists){
      if ( (sfreq(pcp.solid) != "monthly") | ( (sfreq(pcp.solid) == "monthly") & ( length(pcp.solid) > 12) ) )
        pcp.solid.m.avg <- monthlyfunction(pcp.solid, FUN=sum, na.rm=na.rm) / nyears
    } # IF end

    # Computing mean monthly values of 'tmean'
    if ( (sfreq(tmean) != "monthly") | ( (sfreq(tmean) == "monthly") & ( length(tmean) > 12) ) )
      tmean.m.avg <- monthlyfunction(tmean, FUN=mean, na.rm=na.rm)

    # If provided, computing mean monthly values of 'tmx' and 'tmn'
    if ( !missing(tmx) & !missing(tmn)) {
      if ( (sfreq(tmx) != "monthly") | ( (sfreq(tmx) == "monthly") & ( length(tmx) > 12) ) ) 
        tmx.m.avg <- monthlyfunction(tmx, FUN=mean, na.rm=na.rm)

      if ( (sfreq(tmn) != "monthly") | ( (sfreq(tmn) == "monthly") & ( length(tmn) > 12) ) )
        tmn.m.avg <- monthlyfunction(tmn, FUN=mean, na.rm=na.rm)
    } # IF end

    # Shifting the monthly values when 'start.month != 1'
    if (start.month != 1) {

      pcp.m.avg         <- .shift(x=pcp.m.avg      , imonth=start.month)
      if (pcp.solid.exists) 
        pcp.solid.m.avg <- .shift(x=pcp.solid.m.avg, imonth=start.month)
      tmean.m.avg       <- .shift(x=tmean.m.avg    , imonth=start.month)
      if ( !missing(tmx) & !missing(tmn)) {
         tmx.m.avg <- .shift(x=tmx.m.avg, imonth=start.month)
         tmn.m.avg <- .shift(x=tmn.m.avg, imonth=start.month)
      } # IF end

    } # IF end

    if (plot.pcp.probs) {
      if ( sfreq(pcp) == "monthly" ) {
        pcp.m <- pcp
      } else pcp.m <- daily2monthly(pcp, FUN=sum, na.rm=na.rm) # 'subdaily2monthly' is a wrapper to 'daily2monthly'
      pcp.m.q1 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[1], na.rm=na.rm)
      pcp.m.q2 <- monthlyfunction(pcp.m, FUN=quantile, probs=pcp.probs[2], na.rm=na.rm)
      if (start.month != 1) pcp.m.q1 <- .shift(x=pcp.m.q1, imonth=start.month)
      if (start.month != 1) pcp.m.q2 <- .shift(x=pcp.m.q2, imonth=start.month)
    } # IF end

    if (plot.temp.probs) {
      temp.probs.col <- grDevices::adjustcolor(temp.probs.col, alpha.f=temp.probs.alpha)

      if ( sfreq(tmean) == "monthly" ) {
        tmean.m <- tmean
      } else tmean.m <- daily2monthly(tmean, FUN=mean, na.rm=na.rm)  # 'subdaily2monthly' is a wrapper to 'daily2monthly'
      tmean.m.q1 <- monthlyfunction(tmean.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
      tmean.m.q2 <- monthlyfunction(tmean.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)
      if (start.month != 1) tmean.m.q1 <- .shift(x=tmean.m.q1, imonth=start.month)
      if (start.month != 1) tmean.m.q2 <- .shift(x=tmean.m.q2, imonth=start.month)

      if ( !missing(tmx) & !missing(tmn)) {
        if ( sfreq(tmx) == "monthly") {
          tmx.m <- tmx
        } else tmx.m <- daily2monthly(tmx, FUN=mean, na.rm=na.rm) # 'subdaily2monthly' is a wrapper to 'daily2monthly'

        if ( sfreq(tmn) == "monthly") {
          tmn.m <- tmn
        } else tmn.m <- daily2monthly(tmn, FUN=mean, na.rm=na.rm) # 'subdaily2monthly' is a wrapper to 'daily2monthly'

        tmx.m.q1 <- monthlyfunction(tmx.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
        tmx.m.q2 <- monthlyfunction(tmx.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)
        tmn.m.q1 <- monthlyfunction(tmn.m, FUN=quantile, probs=temp.probs[1], na.rm=na.rm)
        tmn.m.q2 <- monthlyfunction(tmn.m, FUN=quantile, probs=temp.probs[2], na.rm=na.rm)
        if (start.month != 1) {
          tmx.m.q1 <- .shift(x=tmx.m.q1, imonth=start.month)
          tmx.m.q2 <- .shift(x=tmx.m.q2, imonth=start.month)
          tmn.m.q1 <- .shift(x=tmn.m.q1, imonth=start.month)
          tmn.m.q2 <- .shift(x=tmn.m.q2, imonth=start.month)
        } # IF end
      } # IF end
    } # IF end

  } # IF end

  #######################################
  # Drawing the climograph
  #######################################
  xlim <- c(0.5, 14.5)

  # Monthly precipitation as barplot
  if (missing(pcp.ylim)) {
    if (plot.pcp.probs) {
      ylim <- range(pretty(pcp.m.avg), pretty(pcp.m.q1), pretty(pcp.m.q2))
    } else ylim <- range(pretty(pcp.m.avg))
  } else ylim <- pcp.ylim 

  par(mar = c(7,5,3,5)) # c(bottom, left, top, right)
  x <- barplot(pcp.m.avg, col=pcp.col, xlim=xlim, ylim=ylim, ylab=pcp.label, las=1, main=main)
  if (pcp.solid.exists)
    barplot(pcp.solid.m.avg, col=pcp.solid.col, xlim=xlim, ylim=ylim, ylab=pcp.label, las=1, main=main, add=TRUE)
  # if (pcp.solid.exists){
  #   pcp.total.m.avg <- cbind(pcp.solid.m.avg, pcp.m.avg)
  #   x <- barplot(pcp.total.m.avg, col=c(pcp.solid.col, pcp.col), xlim=xlim, ylim=ylim, ylab=pcp.label, las=1, main=main)
  # } else {
  #     x <- barplot(pcp.m.avg, col=pcp.col, xlim=xlim, ylim=ylim, ylab=pcp.label, las=1, main=main)
  #   } # ELSE end

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
    suppressWarnings( graphics::arrows(x0 = x, y0 = pcp.m.q2, y1 = pcp.m.q1, 
                                       angle=90, code=3, length=0.1) )

  grid()
  #ifelse(plot.pcp.probs, deltax <- 0.25, deltax <- 0.0)
  if (pcp.labels) text(x+pcp.labels.dx, pcp.m.avg+pcp.labels.dy, cex=pcp.labels.cex, adj=0.5, 
                       labels= round(pcp.m.avg,1), col="black" )


  # If provided, computing the ylim for the secondary temperature axis
  if (missing(temp.ylim)) {
    if ( !missing(tmx) & !missing(tmn)) {
      if (plot.temp.probs) {
        ylim <- range(#pretty(tmx.m.avg), pretty(tmean.m.avg), pretty(tmn.m.avg),
                      pretty(tmx.m.q1), pretty(tmean.m.q1), pretty(tmn.m.q1),
                      pretty(tmx.m.q2), pretty(tmean.m.q2), pretty(tmn.m.q2)
                     )
      } else ylim <- range(pretty(tmx.m.avg), pretty(tmean.m.avg), pretty(tmn.m.avg))
    } else if (plot.temp.probs) {
             ylim <- range(pretty(tmean.m.q1), pretty(tmean.m.avg), pretty(tmean.m.q2))
           } else ylim <- range(pretty(tmean.m.avg))
  } else ylim <- temp.ylim 


  # Mean temperature as line
  par(new = TRUE, xpd=TRUE)
  if (plot.temp.probs) {
    plot(x, tmean.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
    .plotbands(x=x, lband=tmean.m.q1, uband=tmean.m.q2, col=temp.probs.col[2], border=NA)
    lines(x, tmean.m.avg, xlim=xlim, ylim=ylim, col= tmean.col, type = "o", lwd=3, pch=15, 
          cex=1.4, bty = "n", xlab = "", ylab = "")
  } else plot(x, tmean.m.avg, xlim=xlim, ylim=ylim, col= tmean.col, type = "o", lwd=3, 
              pch=15, cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")
  if (tmean.labels) text(x+temp.labels.dx, tmean.m.avg+temp.labels.dy, cex=temp.labels.cex, 
                         adj=0.5, labels= round(tmean.m.avg,1), col=tmean.col )

  # If provided, tmn as line
  if (!missing(tmn)) {
    par(new = TRUE, xpd=TRUE)
    if (plot.temp.probs) {
      plot(x, tmn.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
      .plotbands(x=x, lband=tmn.m.q1, uband=tmn.m.q2, col=temp.probs.col[1], border=NA) 
      lines(x, tmn.m.avg, xlim=xlim, ylim=ylim, col= tmn.col, type = "o", lwd=3, pch=15, 
            cex=1.4, bty = "n", xlab = "", ylab = "")
    } else plot(x, tmn.m.avg, xlim=xlim, ylim=ylim, col= tmn.col, type = "o", lwd=3, pch=15, 
                cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")

    if (tmn.labels) text(x+temp.labels.dx, tmn.m.avg+temp.labels.dy, cex=temp.labels.cex, 
                         adj=0.5, labels= round(tmn.m.avg,1), col=tmn.col )
  } # IF end

  # If provided, tmx as line
  if (!missing(tmx)) {
    par(new = TRUE, xpd=TRUE)
    if (plot.temp.probs) {
      plot(x, tmx.m.avg, xlim=xlim, ylim=ylim, type="n", xlab="", ylab="", axes=FALSE)
      .plotbands(x=x, lband=tmx.m.q1, uband=tmx.m.q2, col=temp.probs.col[3], border=NA)
      lines(x, tmx.m.avg, xlim=xlim, ylim=ylim, col= tmx.col, type = "o", lwd=3, pch=15, 
            cex=1.4, bty = "n", xlab = "", ylab = "")
    } else plot(x, tmx.m.avg, xlim=xlim, ylim=ylim, col= tmx.col, type = "o", lwd=3, pch=15, 
                cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")
    if (tmx.labels) text(x+temp.labels.dx, tmx.m.avg+temp.labels.dy, cex=temp.labels.cex, 
                         adj=0.5, labels= round(tmx.m.avg,1), col=tmx.col )
  } # IF end


  # Plotting temperature axis on the right hand side
  if ( !missing(tmx) & !missing(tmn)) {
    axis(side=4, at = pretty(range(tmn.m.avg, tmean.m.avg, tmx.m.avg)), las=1)
  } else axis(side=4, at = pretty(range(tmean.m.avg)), las=1)
  abline(h=axTicks(side=2), col="lightpink", lty = "dotted")
  text(par("usr")[2]*1.05,mean(par("usr")[3:4]), labels= tmean.label, 
       srt=-90, xpd=TRUE, pos=4)


  # Outter box and legend
  box()
  par(xpd=TRUE)
  if ( pcp.solid.exists & !missing(tmx) & !missing(tmn) ) {
    legend("bottom", legend = c("Prec. (total)", "Prec. (solid)", "Tmn", "Tmean", "Tmx"), bty="n",
           pch=c(15, 15, 15, 15, 15), lty=c(NA, NA, 1, 1, 1), cex=1.2, 
           col=c(pcp.col, pcp.solid.col, tmn.col, tmean.col, tmx.col), ncol=5, inset=c(0.5, -0.2),
           #lty = 1:2, xjust = 1, yjust = 1,
           title = "")
  } else
      if ( !missing(tmx) & !missing(tmn)) {
        legend("bottom", legend = c("Prec.", "Tmn", "Tmean", "Tmx"), bty="n",
               pch=c(15, 15, 15, 15), lty=c(NA, 1, 1, 1), cex=1.2, col=c(pcp.col, tmn.col,
               tmean.col, tmx.col), ncol=4, inset=c(0.5, -0.2),
               #lty = 1:2, xjust = 1, yjust = 1,
               title = "")
      } else
          legend("bottom", legend = c("Precipitation", "Temperature"), bty="n",
                 pch=c(15, 15), lty=c(NA, 1), cex=1.2, col=c(pcp.col, tmean.col), ncol=2, 
                 #lty = 1:2, xjust = 1, yjust = 1,
                 inset=c(0.5, -0.2), title = "")
  
} # 'climograph' END
