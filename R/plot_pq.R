# File plot_p_q.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2012-2018 Mauricio Zambrano-Bigiarini
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
# Updates: 26-Nov-2023                                                         #
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
                        from=start(x), 
                        to=end(x),
                        date.fmt, 
                        tz,
                        na.fill=c("remove", "linear", "spline"), 
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

  if (!is.zoo(p)) stop("Invalid argument: 'p' must be of class 'zoo' !")
  if (!is.zoo(q)) stop("Invalid argument: 'q' must be of class 'zoo' !")

  # Checking 'na.fill'
  na.fill <- match.arg(na.fill)

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)
        
  # Cheking if 'x is a sub-daily zoo object  
  if (x.freq %in% c("minute","hourly") ) {
    subdaily.ts <- TRUE
  } else subdaily.ts <- FALSE

  # Checking that 'p' and 'q' have the same time index
  if ( !all.equal(time(p), time(q)) )
    stop("Invalid argument(s): 'p' and 'q' must have the same time index !")

  ####################################################################################
  # Lines 79-162 are taken from izoo2rzoo.R to check 'from' and 'to'
  ####################################################################################
  x <- p

  # Automatic detection of 'date.fmt'
  if ( missing(date.fmt) ) {
    if ( subdaily.ts ) {
      date.fmt <- "%Y-%m-%d %H:%M:%S"
    } else date.fmt <- "%Y-%m-%d"
  } # IF end

  # Automatic detection of 'tz'
  missingTZ <- FALSE
  if (missing(tz)) {
    missingTZ <- TRUE
    tz        <- ""
  } # IF end
      
  ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
           grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
           grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE), 
           subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )

  # If the index of 'x' is character, it is converted into a Date object
  if ( class(time(x))[1] %in% c("factor", "character") )
    #ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt, tz=tz),
    ifelse(subdaily.date.fmt, time(x) <- as.POSIXct(time(x), format=date.fmt),
                              time(x) <- as.Date(time(x), format=date.fmt) )

  # If 'from' was given as Date, but 'x' is sub-daily
  if (!missing(from)) {
    if (from > to) stop("Invalid argument: 'from > to' !")

    if (from > end(x)) stop("Invalid argument: 'from > end(x)' !")

    if ( subdaily.date.fmt & !(grepl(":", from, fixed=TRUE) ) )
      from <- paste(from, "00:00:00")

    if ( subdaily.date.fmt & missingTZ )
      from <- as.POSIXct(from, tz=tz)
  } # IF end

  # If 'to' was given as Date, but 'x' is sub-daily
  if (!missing(to)) {
    if (to < from ) stop("Invalid argument: 'to < from' !")

    if (to < start(x) ) stop("Invalid argument: 'to < start(x)' !")

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
             warning("'date.fmt' (", date.fmt, ") is sub-daily, while 'x' is a '", 
                     x.freq, "' ts => 'time(x)=as.POSIXct(time(x), tz)'")
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
        p <- na.approx(p)
      } else p <- na.spline(p)
    } # IF end
    if (length(q.na.index) > 0) {
      if (na.fill == "linear") {
        q <- na.approx(q)
      } else q <- na.spline(q)
    } # IF end
  } # IF end

  # saving graphical parameters
  oldpars <- par(no.readonly=TRUE)

  #par(mar=0.1 + c(5, 4, 4, 8), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)
  par(mar=0.1 + c(7, 4, 4, 4), xpd=TRUE ) # bottom, left, top and right. Default: 0.1+ c(5, 4, 4, 2)

  # Plotting Q as time series
  ylim    <- range(q, na.rm=TRUE)
  ylim[2] <- ylim[2]*1.5
  plot(q, type="n", xaxt="n", xlab=xlab, ylab=ylab[2], ylim=ylim, main=main)
  drawTimeAxis(q)
  points(q, type="o", col=cols[2], pch=q.pchs, cex=q.cex, lty=1)
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

  # saving graphical parameters
  par(oldpars)

} # 'plot_pq.zoo' END