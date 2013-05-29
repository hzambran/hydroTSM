# File diy.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'diy' : Given a numeric value of a year, it generates:                       #
#         1) a vector with all the days (dates) within the year, OR            #
#	  2) the amount of days in the year                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates:                                                                     #
################################################################################
# year    : numeric, the year for which the sequence of days will be generated
# out.type: Character indicating the type of result that is given by this function.
#           Valid values are:
#		    -) type= "seq"  => a vectorial sequence with all the days within the given year
#		    -) type= "nmbr" => the number of days in the vectorial sequence with all the days within the given year
diy <- function(year, out.type="seq") {

   # Checking 'out.type'
   if (is.na(match(out.type, c("seq", "nmbr"))))
        stop("Invalid argument: 'out.type' must be of class 'seq' or 'nmbr'")

   # Generating a Daily-regular time series of Dates,
   # just for being counted as the maximum amount of possible daily data
   vec <- seq( from=as.Date( paste(year,"-01-01", sep="") ), to=as.Date( paste(year,"-12-31", sep="") ), by= "days" )

   if (out.type=="seq") return(vec)
   else if (out.type=="nmbr") return ( length(vec) )


} # 'diy' END
