# File si.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
#                                 https://CRAN.R-project.org/package=hydroTSM
#                                 https://www.rforge.net/hydroTSM/
# Copyright 2023-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                      'si' : seasonality index                                #  
################################################################################       
# Purpose: Given a zoo object with daily/monthly precipitation, it computes    #
#          the seasonality index, as:                                          #
#               si = (1/R) *sum(i=1, i=12, abs(xi - R/12) )                    #
#          where:                                                              #
#            -) xi: mean monthly precipitation for month i                     #
#            -) R: mean annual precipitation                                   #    
################################################################################
# Reference:                                                                   #
# Walsh, R., & Lawler, D. (1981). Rainfall seasonality: Description, spatial   #
# patterns and change through time (British Isles, Africa). Weather, 36(7),    #
# 201-208. doi:10.1002/j.1477-8696.1981.tb05400.x                              #
################################################################################
# This index can theoretically vary from 0 (when all months have the same      #
# rainfall) to 1.83 (when all the rainfall ocurrs in a single month)           #
# A qualitative classification of degrees of seasonality is the following:     #
# -----------------------------------------------------------------------------#
#  si values   |                    Rainfall regime                            #
# -----------------------------------------------------------------------------#
#     <= 0.19  | Very equable                                                  #
# 0.20 - 0.39  | Equable but with a definite wetter season                     #
# 0.40 - 0.59  | Rather seasonal with a short drier season                     #
# 0.60 - 0.79  | Seasonal                                                      #
# 0.80 - 0.99  | Markedly seasonal with a long drier season                    #
# 1.00 - 1.19  | Most rain in 3 months or less                                 #
#     >= 1.20  | Extreme, almost all rain in 1-2 months                        #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 28-Nov-2023                                                         #
# Updates: 15-Jan-2024                                                         #
################################################################################


si <- function(x, na.rm=TRUE, from=start(x), to=end(x), 
               date.fmt="%Y-%m-%d", start.month=1) {

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


  if (missing(x)) {
    stop("Missing argument: 'x' must be provided !")
  } else {
      # Checking that 'x' is a zoo object
      if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be 'zoo' !)")

      # Checking that 'x' is a subdaily or monthly object
      if ( sfreq(x) %in% c("quarterly", "annual")) 
        stop("Invalid argument: 'sfreq(x)' must be in c('minute', 'hourly', 'daily', 'weekly', 'monthly' !)")
    } # ELSE end

  ###########################################
  ## In case 'from' and 'to' are provided  ##
  dates.pcp  <- time(x)
     
  # Checking the validity of the 'from' argument
  if (!missing(from)) { 
     from <- as.Date(from, format=date.fmt)

     if (from < dates.pcp[1])
       stop("Invalid argument: 'from' is lower than the first date in 'x' !")

     x <- window(x, start=from)
   } # ELSE end

  # Checking the validity of the 'to' argument
  if (!missing(to)) { 
     to <- as.Date(to, format=date.fmt)

     if (to > dates.pcp[length(x)])
       stop("Invalid argument: 'to' is greater than the last date in 'x' !")

     if (to > dates.pcp[length(x)])

     x <- window(x, end=to)
   } # ELSE end


  # Detecting if 'x' is already mean monthly values (i.e., 12 values)
  pcp.is.mean.monthly <- FALSE

  if ( ( sfreq(x) == "monthly" ) & (length(x) == 12) ) {
    pcp.is.mean.monthly <- TRUE

    months   <- format(dates.pcp, "%b")
    x        <- as.numeric(x)
    names(x) <- months

    if (start.month != 1)
      x <- .shift(x=x, imonth=start.month)

    x.m.avg <- x 
  } # IF end

  ###########################################
  ## In case 'x' was not given as average monthly values
  if ( !pcp.is.mean.monthly ) {

    nyears <- yip(from=from, to=to, date.fmt="%Y-%m-%d", out.type="nmbr")

    # Computing mean monthly values of 'x'
    if ( (sfreq(x) != "monthly") | ( (sfreq(x) == "monthly") & ( length(x) > 12) ) )
      x.m.avg <- monthlyfunction(x, FUN=sum, na.rm=na.rm) / nyears

    # Shifting the monthly values when 'start.month != 1'
    if (start.month != 1)
      x.m.avg <- .shift(x=x.m.avg, imonth=start.month)
  } # IF end

  ###########################################
  # Computation of the mean annual rainfall
  R  <- sum(x.m.avg)  

  ###########################################
  # Computation of the seasonality index
  si <- (1/R) * sum( abs(x.m.avg - R/12) )

  return(si)

} # 'si' END
