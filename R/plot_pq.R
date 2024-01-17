# File plot_p_q.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
#                                 http://www.rforge.net/hydroTSM/ 
# Copyright 2023-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                              'plot_pq'                                       #  
#       Plot precipitation and streamflow time series in the same figure       #
################################################################################       
# Purpose: Given a time series of precipitation and streamflow, this function  #
#          plots the two time series in the same figure, streamflows as a      #
#          normal time series and precipitation as bars comming from the upper #
#          part of the plotting window                                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 09-Jun-2018                                                         #
# Updates: 26-Nov-2023 ; 22-Dec-2023                                           #
#          12-Jan-2024 ; 13-Jan-2024 ; 17-Jan-2024                             #
################################################################################

# 'p'          : zoo object with precipitation time series, with any time frequency 
# 'q'          : zoo object with streamflow time series, with any time frequency 
# 'from'       : Character indicating the starting date for creating the regularly 
#                spaced zoo object. The default value corresponds to the date of 
#                the first element of \code{x} \cr
#                It has to be in the format indicated by \code{date.fmt}. 
# 'to'         : Character indicating the ending date for creating the regularly 
#                spaced zoo object. The default value corresponds to the date of 
#                the last element of \code{x} \cr
#                It has to be in the format indicated by \code{date.fmt}.
# 'date.fmt'   : Character indicating the format in which the dates are stored in 
#                \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. 
#                See \sQuote{Details} section in \code{\link[base]{strptime}}.
#                By default, \code{date.fmt} is missing, and it is automatically 
#                set to \kbd{\%Y-\%m-\%d} when \code{time(x)} is \code{Date} 
#                object, and set to \kbd{\%Y-\%m-\%d \%H:\%M:\%S} when \code{x} 
#                is a sub-daily zoo object.
# 'tz'         : Character, with the specification of the time zone used for 
#                \code{from}, \code{to}. System-specific (see time zones), 
#                but \code{""} is the current time zone, and \code{"GMT"} is 
#                UTC (Universal Time, Coordinated). 
#                See \code{\link[base]{Sys.timezone}} and 
#                \code{\link[base]{as.POSIXct}}. \cr
#                If \code{tz} is missing (the default), it is automatically set 
#                to the time zone used in \code{time(x)}. \cr
#                This argument can be used when working with sub-daily zoo objects
#                to force using  time zones other than the local time zone for 
#                \code{from} and \code{to}. It should be used with caution, 
#                being well aware of the time zone of the data. See examples.
# 'na.fill'    : Character indicating how to fill any NA present in 'x'.
#                Valid values are:
#                -) "remove" => NAs are not plotted
#                -) "linear" => NAs are removed by linear interpolation, using 
#                               \code{\link[zoo]{na.approx}}
#                -) "spline" => NAs are removed by spline interpolation, using 
#                               \code{\link[zoo]{na.spline}}
# 'xlab'       : a title for the \code{x} axis: see \code{\link[graphics]{title}}
# 'ylab'       : a two-element title for the \code{y} axis. 
#                See \code{\link[graphics]{title}}.
#                The first element is used for the right \code{y} axis 
#                (i.e., for precipitation). The second element is used for the 
#                left \code{y} axis (i.e., for streamflows).
# 'main'       : The main title (on top) of the figure.
# 'leg.title'  : a character string or length-one expression giving a title to be 
#                placed at the top of the legend. \code{\link[graphics]{legend}}.
# 'leg.text'   : a two-element character to appear in the legend placed at the 
#                bottom of the figure. 
#                The first element is used for precipitation and the second 
#                element is used for streamflows.
# 'cols'       : character, representing the colors to be used for ploting the 
#                precipitation and streamflow time series. 
#                The first element is used for precipitation and the second 
#                element is used for streamflows.
# 'q.pch'      : numeric, representing the symbols used for ploting the streamflow 
#                time series.
# 'q.cex'      : a numerical vector giving the amount by which plotting characters 
#                and symbols should be scaled relative to the default. 
#                This works as a multiple of par("cex"). 
#                See \code{\link[graphics]{plot.default}}
# '...'        : further arguments passed to or from other methods. Not used yet.

plot_pq <- function(p, ...) UseMethod("plot_pq")

plot_pq.zoo <- function(p, 
                        q, 
                        ptype=c("original", "monthly"),
                        
                        na.fill=c("remove", "linear", "spline"), 
                        
                        from=start(p), 
                        to=end(p),

                        date.fmt=NULL, 
                        tz=NULL,

                        main=ifelse(ptype=="original", "Precipitation and Streamflows", "Monthly Precipitation and Streamflows"),
                        xlab=ifelse(ptype=="original", "Time", "Month"), 
                        ylab=c("P, [mm]", "Q, [m3/s]"), 
                        p.col=ifelse(ptype=="original", "blue", "lightblue"),
                        q.col=ifelse(ptype=="original", "black", "blue"),

                        leg.title="",
                        leg.text=c("P", "Q"),
                        
                        q.pch=16,
                        q.cex=0.3,
                            
                        start.month=1,

                        plot.p.probs=TRUE,
                        p.probs=c(0.25, 0.75),
                        p.alpha=0.8,

                        plot.q.probs=TRUE,
                        q.probs=c(0.25, 0.75),
                        q.probs.col="lightskyblue1",
                        q.probs.alpha=0.8,
                            
                        labels=TRUE,
                        labels.cex=0.8,
                        labels.p.dy=-median(daily2monthly(p, FUN=sum, na.rm=TRUE), 
                                     na.rm=TRUE)*1.1,
                        labels.q.dx=c(rep(-0.2,6), rep(0.2,6)),
                        labels.q.dy=rep(median(q, na.rm=TRUE)*1.3, 12),
                        
                        ...
                        ) {

  # Checking 'p'
  if (missing(p)) {
    stop("Missing argument: 'p' must be provided !")
  } else 
      # Checking that 'p' is a zoo object
      if ( !is.zoo(p) ) stop("Invalid argument: 'class(p)' must be 'zoo' !")

  # Checking 'q'
  if (missing(q)) {
    stop("Missing argument: 'q' must be provided !")
  } else 
      # Checking that 'q' is a zoo object
      if ( !is.zoo(q) ) stop("Invalid argument: 'class(q)' must be 'zoo' !")

  # Checking that 'p' and 'q' have the same time index
  if ( !all.equal(time(p), time(q)) )
    stop("Invalid argument(s): 'p' and 'q' must have the same time index !")

  # Checking 'ptype'
  ptype <- match.arg(ptype)

  # Checking 'na.fill'
  na.fill <- match.arg(na.fill)

  # sampling frequency of 'x'           
  p.freq <- sfreq(p)
  
  # Checking if 'x is a sub-daily zoo object  
  if (p.freq %in% c("minute","hourly") ) {
    subdaily.ts <- TRUE
  } else subdaily.ts <- FALSE

  # Checking that 'q' and 'p' have the same dates
  dates.q  <- time(q)
  if (!missing(p)) {
    dates.p <- time(p)
    if (!all.equal(dates.q, dates.p))
      stop("Invalid arguments: 'dates(q)' must be equal to 'dates(p)' !!")
  } # IF end
  
  ####################################################################################
  # Lines 147-231 are taken from izoo2rzoo.R to check 'from' and 'to'
  ####################################################################################
  # Automatic detection of 'date.fmt'
  if ( is.null(date.fmt) ) {
    if ( subdaily.ts ) {
      date.fmt <- "%Y-%m-%d %H:%M:%S"
    } else date.fmt <- "%Y-%m-%d"
  } # IF end

  # Automatic detection of 'tz'
  missingTZ <- FALSE
  if (is.null(tz)) {
    missingTZ <- TRUE
    tz        <- ""
  } # IF end
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE), 
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )

  # If the index of 'p' is character, it is converted into a Date object
  if ( class(time(p))[1] %in% c("factor", "character") )
    #ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(p), format=date.fmt, tz=tz),
    ifelse(subdaily.date.fmt, time(p) <- as.POSIXct(time(p), format=date.fmt),
                              time(p) <- as.Date(time(p), format=date.fmt) )

  # If 'from' was given as Date, but 'x' is sub-daily
  if (!missing(from)) {
    if (from > to) stop("Invalid argument: 'from > to' !")

    if (from > end(p)) stop("Invalid argument: 'from > end(p)' !")

    if ( subdaily.date.fmt & !(grepl(":", from, fixed=TRUE) ) )
      from <- paste(from, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      from <- as.POSIXct(from, tz=tz)
  } # IF end

  # If 'to' was given as Date, but 'x' is sub-daily
  if (!missing(to)) {
    if (to < from ) stop("Invalid argument: 'to < from' !")

    if (to < start(p) ) stop("Invalid argument: 'to < start(p)' !")

    if ( subdaily.date.fmt & !(grepl(":", to, fixed=TRUE) ) )
      to <- paste(to, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      to <- as.POSIXct(to, tz=tz)
  } # IF end
        
  # checking that date.fmt and the sampling frequency of 'x' are compatible 
  if ( subdaily.ts ) {
    if (!subdaily.date.fmt) 
      stop("Invalid argument: 'date.fmt' (", date.fmt, 
           ") is not compatible with a sub-daily time series !!")
  } else {
           if (subdaily.date.fmt) {
             #time(x) <- as.POSIXct(time(x), tz=tz)
             time(x) <- as.POSIXct(time(x))
             warning("'date.fmt' (", date.fmt, ") is sub-daily, while 'p' is a '", 
                     p.freq, "' ts => 'time(p)=as.POSIXct(time(p), tz)'")
           } # IF end    
          } # ELSE end

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(from, format=date.fmt, tz=tz))
    #dt <-  try(as.POSIXct(from, format=date.fmt))
  } else dt <- try(as.Date(from, format=date.fmt))
  if("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'from' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      from <- as.POSIXct(from, format=date.fmt, tz=tz)
      #from <- as.POSIXct(from, format=date.fmt)
    } else from <- as.Date(from, format=date.fmt)

  if (subdaily.ts) {
    dt <-  try(as.POSIXct(to, format=date.fmt, tz=tz))
  } else dt <- try(as.Date(to, format=date.fmt))
  if("try-error" %in% class(dt) || is.na(dt)) {
    stop("Invalid argument: format of 'to' is not compatible with 'date.fmt' !")
  } else if (subdaily.ts) {
      to <- as.POSIXct(to, format=date.fmt, tz=tz)
    } else to <- as.Date(to, format=date.fmt)
  ####################################################################################

  # Selecting only those data within the time period between 'from' and 'to'
  p <- window(p, start=from, end=to)
  q <- window(q, start=from, end=to)

  # If required, filling in any missing value(s)
  if (na.fill != "remove") {
    p.na.index <- which(is.na(p))
    q.na.index <- which(is.na(q))

    if (length(p.na.index) > 0) {
      if (na.fill == "linear") {
        p <- zoo::na.approx(p)
      } else p <- zoo::na.spline(p)
    } # IF end
    if (length(q.na.index) > 0) {
      if (na.fill == "linear") {
        q <- zoo::na.approx(q)
      } else q <- zoo::na.spline(q)
    } # IF end
  } # IF end

  # saving graphical parameters
  oldpars <- par(no.readonly=TRUE)

  if (ptype=="original") {
     .plot_pq_ts.zoo(p, q, 
                     #ptype=ptype, na.fill=na.fill, from=from, end=end,
                     #date.fmt=date.fmt, tz=tz,
                     main=main, xlab=xlab, ylab=ylab, cols=c(p.col, q.col),
                     leg.title=leg.title, leg.text=leg.text,
                     q.pch=q.pch, q.cex=q.cex) 
  } else .plot_pq_monthly.zoo(p, q, 
                              #ptype=ptype, na.fill=na.fill, from=from, end=end,
                              #date.fmt=date.fmt, tz=tz,
                              main=main, xlab=xlab, ylab=ylab, cols=c(p.col, q.col),

                              #leg.title=leg.title, leg.text=leg.text,
                              #q.pch=q.pch, q.cex=q.cex,

                              start.month=start.month, plot.q.probs=plot.q.probs,
                              q.probs=q.probs, q.probs.col=q.probs.col,
                              q.probs.alpha=q.probs.alpha,
                         
                              plot.p.probs=plot.p.probs,
                              p.probs=p.probs, p.alpha=p.alpha,
                            
                              labels=labels, labels.cex=labels.cex,
                              labels.q.dx=labels.q.dx,
                              labels.q.dy=labels.q.dy,
                              labels.p.dy=labels.p.dy
                              )

   # restoring original graphical parameters
  par(oldpars)
} # 'plot_pq.zoo' END




.plot_pq_ts.zoo <- function(p, 
                            q, 

                            #na.fill=c("remove", "linear", "spline"), 
                            
                            #from=start(x), 
                            #to=end(x),
                            
                            #date.fmt, 
                            #tz,
                            
                            xlab="Time", 
                            ylab=c("P", "Q"), 
                            main="Precipitation and Streamflows",
                            leg.title="",
                            leg.text=c("P", "Q"),
                            cols=c("blue", "black"),
                            q.pch=16,
                            q.cex=0.3,
                            ...
                            ) {

  # saving graphical parameters
  #oldpars <- par(no.readonly=TRUE)

  #par(mar=0.1 + c(5, 4, 4, 8), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)
  par(mar=0.1 + c(7, 4, 4, 4), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)

  # Plotting Q as time series
  ylim    <- range(q, na.rm=TRUE)
  ylim[2] <- ylim[2]*1.5
  plot(q, type="n", xaxt="n", xlab=xlab, ylab=ylab[2], ylim=ylim, main=main)
  drawTimeAxis(q)
  points(q, type="o", col=cols[2], pch=q.pch, cex=q.cex, lty=1)
  grid()

  # Plotting P as barplot from top to bottom
  par(new=TRUE)
  ylim    <- rev(range(p, na.rm=TRUE))
  ylim[1] <- ylim[1]*3
  barplot(p, ylim = ylim, xaxt="n", yaxt = "n", col=cols[1], border=NA)
  axis(4, at = pretty(p), col=cols[1], col.axis=cols[1])
  mtext(side = 4, line = 2, text="P", col=cols[1], srt=45)
  #at <- pretty(p)
  #axis(4, at=at, label=FALSE, col=cols[1])
  #text(y=at-2.5, x=par("usr")[2]+30, labels = at, srt = -90, pos = 4, xpd = TRUE, col=cols[1], offset=0.5)

  # legend("topright", inset=c(-0.07, 0), legend = leg.text, bty="n",
  #        lty=1, pch=pchs, col=cols,
  #        #lty = 1:2, xjust = 1, yjust = 1,
  #        title=leg.title)
  p.pch     <- 15
  leg.text  <- c(leg.text[1], " ", leg.text[2])
  cols      <- c(cols[1], "white", cols[2])
  pchs      <- c(p.pch, 3, q.pch)

  legend("bottom", horiz=TRUE, , bty="n", inset=c(0, -0.45), 
         x.intersp=0.1, y.intersp=3, 
         lty=1, pch=pchs, col=cols, cex=1,
         title=leg.title, legend= leg.text)

  # restoring original graphical parameters
  #par(oldpars)

} # '.plot_pq_ts.zoo' END



###################################################################################
# .plot_pq_monthly.zoo: Function for drawing a figure with mean monthly           #
#                       precipitation (on the top panel) and mean monthly         #
#                       streamflows (on the bottom pannel)                        #
###################################################################################
# Originally this function was called 'monthlycurve', but afterwards it was       # 
# merged with the 'plot_pq' function                                              #
###################################################################################
# Author : Mauricio Zambrano-Bigiarini                                            #
###################################################################################
# Started: 26-Jul-2022                                                            #
# Updates: 22-Sep-2022 ; 11-Oct-2022 ; 25-Oct-2022                                #
#          22-Dec-2023 ; 27-Dec-2023                                              #
#          12-Jan-2024 ; 17-Jan-2024                                              #          
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

.plot_pq_monthly.zoo <- function(p,
                                 q, 
                            
                                 #na.rm=TRUE, 
                                 #na.fill=c("remove", "linear", "spline"), 
                            
                                 #from, 
                                 #to, 
                            
                                 #date.fmt, 
                                 #tz,
                            
                                 main="Monthly Precipitation and Streamflows", 
                                 xlab="Month",
                                 ylab=c("P, [mm]", "Q, [m3/s]"),
                                 cols=c("lightblue", "blue"),
                            
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
                                 labels.p.dy=-median(daily2monthly(p, FUN=sum, na.rm=TRUE), na.rm=TRUE)/10
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
   

  # saving graphical parameters
  oldpars <- par(no.readonly=TRUE)

  ###########################################
  ## In case 'q' is not average monthly values
  if ( (sfreq(q) != "monthly") ) {
    q.m    <- daily2monthly(q, FUN=mean, na.rm=TRUE)
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


  ###########################################
  ## In case 'q' is not average monthly values
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


  
  # the next line is required just in case a previous plot modified the graphical 'layout'
  par(mfrow=c(1,1)) 

  ##############################################################################
  # Definining the plotting area (1 column, 2 rows), where the lower row has 
  # a height 3 times larger than the upper window, AND 
  ##############################################################################
  par(mar=c(3, 4.1, 3, 1.5), xpd=TRUE) # default  c(5.1, 4.1, 4.1, 2.1)
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=5, heights=c(1,3))  
 
  ######################################################
  # 1st Figure: Monthly Precipitation in the upper panel
  ######################################################

  ylim <- range(pretty(p.m.q1), pretty(p.m.q2))
  x <- barplot(p.m.med, ylim=rev(ylim), xlab="", ylab=ylab[1], axes=TRUE, col=cols[1], names.arg=month.names, main=main)
  #axis(side=1, at=lx, labels=month.names, line=0.02, outer=TRUE, pos=1)
  if (labels) text(x, labels.p.dy, cex=labels.cex, adj=0.5, labels= round(p.m.med,1), col="black")

  # Adding error bars
  if (plot.p.probs) 
    graphics::arrows(x0 = x, y0 = p.m.q2, y1 = p.m.q1, angle=90, code=3, length=0.1)

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
  #plot(lx, q.m.med, xlim=xlim, ylim=ylim, col= cols[2], type="o", lwd=3, pch=15, cex=1.4, axes=TRUE, xaxt="n", xlab=xlab, ylab=ylab[2])

  if (plot.q.probs) 
    .plotbands(x=lx, lband=q.m.q1, uband=q.m.q2, col=lubands.col, border=NA)
  
  grid()
  lines(lx, q.m.med, xlim=xlim, ylim=ylim, col= cols[2], type = "o", lwd=3, pch=15, cex=1.4)
  axis(side=1, at=lx, labels=month.names)
  if (labels) text(lx+labels.q.dx, q.m.med+labels.q.dy, cex=labels.cex, adj=0.5, labels= round(q.m.med,1), col="black" )
 
  # restoring original graphical parameters
  par(oldpars)

} # 'plot_pq_monthly.zoo' END
