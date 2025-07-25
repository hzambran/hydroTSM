# File baseflow.R
# File plot_p_q.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
#                                 http://www.rforge.net/hydroTSM/ 
# Copyright 2012-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                               'baseflow'                                     #  
################################################################################       
# Purpose: Given a complete (without missing values) series of streamflow,     #
#          this function computes the baseflow using the filter proposed by    # 
#          Arnold & Allen in 1999                                              #
################################################################################
# References: Arnold, J. G., & Allen, P. M. (1999). Automated methods for      #
#             estimating baseflow and ground water recharge from streamflow    #
#             records. JAWRA Journal of the American Water Resources ,         #
#             Association 35(2), 411-424.                                      #
#             doi: 10.1111/j.1752-1688.1999.tb03599.x                          #
#                                                                              #
#             Ladson, A. R., Brown, R., Neal, B., & Nathan, R. (2013). A       #
#             standard approach to baseflow separation using the Lyne and      #
#             Hollick filter. Australasian Journal of Water Resources, 17(1),  #
#             25-34. doi:10.7158/13241583.2013.11465417                        #
################################################################################
# Author : Mauricio Zambrano-Bigiarini, Hector Garces-Figueroa                 #
# Started: 2013                                                                #
# Updates: 16-May-2018                                                         #
#          26-Nov-2023                                                         #
#          17-Jan-2024                                                         #
#          24-Jul-2025 (HGF) ; 25-Jul-2025                                     #
################################################################################

# 'x'       : zoo object with streamflow records. The suggested time 
#             frequency should be hourly or daily, but the algorithm will work  
#             with any time frequency
# 'beta'    : numeric representing the filter parameter. Default value is 0.925  
#             as recommended by Arnold & Allen (1999)
# 'from'       : Character indicating the starting date for creating the regularly 
#                spaced zoo object. The default value corresponds to the date of 
#                the first element of \code{x} \cr
#                It has to be in the format indicated by \code{date.fmt}. 
# 'to'         : Character indicating the ending date for creating the regularly 
#                spaced zoo object. The default value corresponds to the date of 
#                the last element of \code{x} \cr
#                It has to be in the format indicated by \code{date.fmt}.
# 'n.pass'     : Integer indicating the number times the filetr can be passed
#                over the streamflow data. Default is 3L.
# 'pass.start' : Character indicating the direction of the first filter pass.
#                Subsequent passes alternate direction (e.g., forward-backward)
#                Valid values are:
#             -) "forward" (default)
#             -) "backward"
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
# 'na.fill' : Character indicating how to fill any NA present in 'x'.
#             Valid values are:
#             -) "none"   => NAs are not removed, and therefore the algorithm is 
#                            not executed
#             -) "linear" => NAs are removed by linear interpolation, using 
#                            \code{\link[zoo]{na.approx}}
#             -) "spline" => NAs are removed by spline interpolation, using 
#                            \code{\link[zoo]{na.spline}}
# 'out.type': Character indicating the type of result that is given by this function.
#             Valid values are:
#             -) "last" => only the baseflow computed after the third pass 
#                          of the filter is returned
#             -) "all " => the 3 baseflows computed after each pass of the 
#                          filter are returned in a matrix or zoo object
# 'plot'    : logical. Indicates if the baseflow should be plotted or not. 
#             If plotted, the original 'x' values are plotted as well
# 'xcol'    : character, representing the color to be used for ploting the 
#             streamflow time series
#             Only used when \code{plot=TRUE}.
# 'bfcol'   : character of lenght 3, representing the color(s) to be used for 
#             ploting the baseflow time series. The first, second and third 
#             element are used to represent the baseflow after the third, 
#             second and first pass of the filter, respectively.
#             Only used when \code{plot=TRUE}.
# 'pch'     : numeric, representing the symbols used for ploting the streamflow 
#             time series (both, the original series and the baseflow).
#             Only used when \code{plot=TRUE}.
# 'cex'     : a numerical vector giving the amount by which plotting characters 
#             and symbols should be scaled relative to the default. 
#             This works as a multiple of par("cex"). 
#             See \code{\link[graphics]{plot.default}}.
#             Only used when \code{plot=TRUE}.
# '...'     : further arguments passed to or from other methods. Not used yet.

baseflow <- function(x, ...) UseMethod("baseflow")

baseflow.zoo <- function(x, 
                         beta=0.925, 
                         from=start(x), 
                         to=end(x),
                         n.pass = 3L,
                         pass.start = c("forward", "backward"),
                         date.fmt,
                         tz,
                         na.fill=c("none", "linear", "spline"), 
                         out.type=c("last", "all"), 
                         plot=TRUE, 
                         xcol="black",
                         bfcol=c("blue", "darkcyan", "darkorange3"),
                         pch=15,
                         cex=0.3,
                         ...) {

  # Checking 'out.type'
  out.type <- match.arg(out.type)

  # Checking 'na.fill'
  na.fill <- match.arg(na.fill)

  # sampling frequency of 'x'           
  x.freq <- sfreq(x)
  
  # Checking if 'x is a sub-daily zoo object  
  subdaily.ts <- FALSE
  if (x.freq %in% c("minute","hourly") ) 
    subdaily.ts <- TRUE

  if ( (x.freq == "hourly") and (n.pass < 9) )
    warning("For hourly data 'n.pass' should be equal to 9 !! (Ladson et al., 2013) ")

  # checking 'n.pass'
  if (!inherits(n.pass, "integer")) stop("'n.pass' must be an integer !")
  if (n.pass <= 0) stop("'n.pass' must be greater than 0 !")
  
  # checking 'pass.start'
  pass.choices <- c("forward", "backward")
  pass.start <- match.arg(pass.start, pass.choices)

  # checking 'bfcol'
  if (out.type == "all" && plot && length(bfcol) != n.pass) {
    stop ("length of 'bfcol' must match 'n.pass'")
  } 

  ####################################################################################
  # Lines 154-178 are taken from izoo2rzoo.R to check 'from' and 'to'
  ####################################################################################

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
  x <- window(x, start=from, end=to)

  # Checking that 'x' does not have any missing value
  na.index <- which(is.na(x))
  if (length(na.index) > 0) {
    if (na.fill == "none") {
      stop("Invalid argument: 'x' has some NA values. See 'na.fill' argument !!")
    } else if (na.fill == "linear") {
        x <- zoo::na.approx(x)
      } else x <- zoo::na.spline(x)
  } # IF end

  # Storing and  then removing the possible time attribute
  HasTime <- FALSE
  if ( is.zoo(x) ) {
    HasTime <- TRUE
    xtime   <- time(x)
    x       <- zoo::coredata(x)
  } # if END

  # Initializing some parameters of the algorithm
  n  <- length(x)
  f1 <- beta
  f2 <- (1+beta) / 2

  # Initializing quickflow values 
  quickflow    <- rep(NA, n)
  quickflow <- replicate(n.pass, quickflow, simplify = FALSE)
  quickflow[[1]][1] <- x[1] / 2

  # Initializing baseflow values after the first ('baseflow1'), 
  # second ('baseflow2') and third ('baseflow3') pass of the filter
  baseflow <- rep(NA, n)
  baseflow <- replicate(n.pass, baseflow, simplify = FALSE)
  pass.type <- rep(unique(c(pass.start, pass.choices)), length.out = n.pass)

  .filter_pass <- function(l.baseflow, l.quickflow, l.type,
                           l.f1 = f1, l.f2 = f2, l.n = n) {
    if (l.type == "forward") {
      aux.n <- 2:l.n
      op    <- match.fun("-")
    } else { # type == "backward"
      aux.n <- (l.n-2) : 1
      op    <- match.fun("+")
    }

    for (i in aux.n) {
      l.quickflow[i] <- l.f1 * l.quickflow[op(i, 1)] +
                        l.f2 * (l.baseflow[i] - l.baseflow[op(i, 1)])
      if (l.quickflow[i] < 0) l.quickflow[i] <- 0
    }

    baseflow.out <- l.baseflow - l.quickflow
    neg.index <- which(baseflow.out < 0)
    if (length(neg.index) > 0) baseflow.out[neg.index] <- 0
    larger.index <- which(baseflow.out > l.baseflow)
    if (length(larger.index) > 0) {
      baseflow.out[larger.index] <- l.baseflow[larger.index]
    }
    
    out <- list(bf = baseflow.out, qf = l.quickflow)
    return(out)
  }
  # First pass of the filter
  first.pass     <- .filter_pass(l.baseflow = x,
                                 l.quickflow = quickflow[[1]],
                                 l.type = pass.type[[1]])
  baseflow[[1]]  <- first.pass[["bf"]]
  quickflow[[1]] <- first.pass[["qf"]]
  
  if (n.pass >= 2) {
    for (i in 2:n.pass) { 
      baseflow[[i]][n-1] <- baseflow[[1]][n-1] 
      filtered.data  <- .filter_pass(l.baseflow = baseflow[[i - 1]],
                                    l.quickflow = quickflow[[i - 1]],
                                    l.type = pass.type[[i]])
      baseflow[[i]]  <- filtered.data[["bf"]]
      quickflow[[i]] <- filtered.data[["qf"]]
    }
  }

  # Giving back the time attribute if the orignal 'x' object had one
  
  baseflow <- do.call(cbind, baseflow)
  if ( HasTime ) {
    x         <- zoo(x, xtime)
    baseflow <- zoo::zoo(baseflow, xtime)
  } # if END

  # Creating the output object
  if (out.type=="last") {
    out <- baseflow[, n.pass]
  } else out <- baseflow


  # Plotting, if necessary
  if (plot) {
    ylab <- "Q"
    xlab <- "Time"
    legend.text <- c("Streamflow", "Baseflow")
    if (HasTime) {
      plot(x, ylab=ylab, type="n", xaxt="n", xlab=xlab)
      drawTimeAxis(x)
      grid()
      points(x, type="o", col=xcol, pch=15, lty=1, cex=cex)
      if (out.type=="all") {
        for (i in 1:(n.pass - 1)) {
            aux.col <- bfcol[n.pass + 1 - i]
            points(1:n, baseflow[, i], type="o", col=aux.col, pch=pch, lty=1,
                   cex=cex)
          }
        legend.text <- c("Streamflow", paste("Baseflow pass", n.pass:1))
        # legend.cols <- c(xcol, bfcol[1], bfcol[2], bfcol[3])
        legend.cols <- c(xcol, bfcol)
      } else {
          legend.cols <- c(xcol, bfcol[1])
        } # ELSE end
        points(baseflow[, n.pass], type="o", col=bfcol[1], pch=pch, lty=1,
               cex=cex)
    } else { # HasTime == FALSE
        plot(1:n, x, ylab=ylab, type="n", xlab=xlab)
        grid()
        points(1:n, x, type="o", col=xcol, pch=pch, lty=1, cex=cex)
        if (out.type=="all") {
          for (i in 1:(n.pass - 1)) {
            aux.col <- bfcol[n.pass + 1 - i]
            points(1:n, baseflow[, i], type="o", col=aux.col, pch=pch, lty=1,
                   cex=cex)
          }

          legend.text <- c("Streamflow", paste("Baseflow pass", n.pass:1))
          legend.cols <- c(xcol, bfcol)
        } else {
            legend.cols <- c(xcol, bfcol[1])
          } # ELSE end
          points(1:n, baseflow[, n.pass], type="o", col=bfcol[1], pch=pch,
                 lty=1, cex=cex)
      } # ELSE end

    legend("topright", legend = legend.text, bty="n",
           lty=c(1,1), pch=c(pch, pch), col=legend.cols,
           #lty = 1:2, xjust = 1, yjust = 1,
           title = "")
  } # IF end

  return(out)

} # 'baseflow' END

#setwd("/dataMZB/2018-UFRO/work/Investigacion/Colaboracion_de_estudiantes/Cristobal_Soto/data/ts")
#library(zoo)
#x <- read.csv(file="Caudales_Curacautin_en_Rari_Ruca.csv")
#dates <- as.Date(x[,1], format="%d-%m-%Y")
#q <- x[1:3597, 2]
#dates <- dates[1:3597]
#q.na.filled <- na.approx(q)
#bf <- baseflow(q.na.filled)
#bf <- zoo(bf, dates)


