# File climograph.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2016-2016 Mauricio Zambrano-Bigiarini
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
#          08-May-2017                                                         #
################################################################################
# 'pcp'      : variable of type 'zoo' with monthly, daily or subdaily          
#               precipitation data
# 'tmean'    : variable of type 'zoo' with monthly, daily or subdaily          
#               mean temperature data
# 'tmx'      : variable of type 'zoo' with monthly, daily or subdaily          
#               maximum temperature data. 
#               ONLY used (togheter with 'tmn') when 'tmean' is missing
# 'tmn'      : variable of type 'zoo' with monthly, daily or subdaily          
#               minimum temperature data. 
#               ONLY used (togheter with 'tmx') when 'tmean' is missing
# 'date.fmt' : format in which the dates are stored in 'from' and 'to'.
# 'na.rm'    : Logical. Should missing values be removed?
#              TRUE : the monthly values  are computed considering only those values in 'x' different from NA
#              FALSE: if there is AT LEAST one NA within a month, the FUN and monthly values are NA

climograph <- function(pcp, tmean, tmx, tmn, na.rm=TRUE, 
                       from, to, date.fmt="%Y-%m-%d", 
                       main="Climograph", 
                       pcp.label="Precipitation, [mm]", 
                       tmean.label="Temperature, [\U00B0 C]",
                       pcp.col="lightblue", 
                       tmean.col="red") {

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

   
  ###########################################
  ## In case 'from' and 'to' are provided  ##
  dates.pcp  <- time(x)
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

  #######################################
  # Drawing the climograph
  #######################################
  xlim <- c(0.5, 14.5)

  # Precipitation bars
  par(mar = c(7,5,3,5)) # c(bottom, left, top, right)
  x <- barplot(pcp.m.avg, col=pcp.col, xlim=xlim, ylab=pcp.label, las=1, main=main)
  grid()

  # Mean temperature as lines
  par(new = TRUE, xpd=TRUE)
  plot(x, tmean.m.avg, xlim=xlim, col= tmean.col, type = "o", lwd=3, pch=15, cex=1.4, axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis(side=4, at = pretty(range(tmean.m.avg)), las=1)
  text(par("usr")[2]+1.7, par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, srt=-90, adj = 0.5, labels= tmean.label,  xpd = TRUE)

  # Outter box and legend
  box()
  legend("bottom", legend = c("Precipitation", "Temperature"), bty="n",
         pch=c(15, 15), lty=c(NA, 1), cex=1.2, col=c(pcp.col, tmean.col), ncol=2, inset=c(0.5,-0.2),
             #lty = 1:2, xjust = 1, yjust = 1,
         title = "")
  
} # 'climograph' END
