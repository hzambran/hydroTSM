#############################################################################
# time2season : This function transforms a vector of dates into a vector of #
#               seasons (summer, winter, autumm, spring), considering that: #
#               winter = DJF: December, January, February                   #
#               spring = MAM: March, April, May                             #
#               summer = JJA: June, July, August                            #
#               autumm = SON: September, October, November                  #
#############################################################################
#                       March 18th, 2009                                    #
#############################################################################

# 'x'       : vector with the dates that have to be transformed. class(x) must be "Date"
# 'out.fmt' : format of the output seasons. Possible values are:
#             -) 'seasons' =>  "winter", "spring",  "summer", autumm"
#             -) 'months'  =>  "DJF", "MAM",  "JJA", SON"

# 'result': vector with the wheater season to which each date in 'x' belongs

time2season <- function(x, out.fmt="months") {

 # Checking that 'class(x)==Date'
 if (is.na(match(class(x), c("Date") ) ) )
     stop("Invalid argument: 'x' must be of class 'Date'")

 # Checking the class of out.fmt
 if (is.na(match(out.fmt, c("seasons", "months") ) ) )
     stop("Invalid argument: 'out.fmt' must be in c('seasons', 'months')")

 months <- format(x, "%m")

 winter <- which( months %in% c("12", "01", "02") )
 spring <- which( months %in% c("03", "04", "05") )
 summer <- which( months %in% c("06", "07", "08") )
 autumm <- which( months %in% c("09", "10", "11") )

 # Creation of the output, with the same length of the 'x' input
 seasons <- rep(NA, length(x))

 if (out.fmt == "seasons") {
    seasons[winter] <- "winter"
    seasons[spring] <- "spring"
    seasons[summer] <- "summer"
    seasons[autumm] <- "autumm"
 } else { # out.fmt == "months"
    seasons[winter] <- "DJF"
    seasons[spring] <- "MAM"
    seasons[summer] <- "JJA"
    seasons[autumm] <- "SON"
 } # IF end

 return(seasons)

} # 'time2season' END
