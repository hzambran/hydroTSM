# File mip.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

####################################################################
# 'mip': Given any starting and ending dates, it generates:        #
#        1) a vector with all the months between the two dates, OR #
#	 2) the amount of months between the two dates                   #
####################################################################

# 'from'    : Starting date for computing the number of dyas. MUST have the date format specified by 'date.fmt'
# 'to'	    : Ending date for computing the number of dyas. MUST have the date format specified by 'date.fmt'
# 'date.fmt': Format of the dates (e.g., "%d-%m-%Y")
# out.type  : type of result that is given by this function
#		      -) type= "seq"  => a vectorial sequence with all the months within the given year
#		      -) type= "nmbr" => the number of days in the vectorial sequence with all the months within the given year
mip <- function(from, to, date.fmt="%Y-%m-%d", out.type="seq") {

     # Checking 'out.type'
     if (is.na(match(out.type, c("seq", "nmbr"))))
        stop("Invalid argument: 'out.type' must be of class 'seq' or 'nmbr'")

     # Generating an Annual-regular time series of Dates.
     vec.months <- seq( from=as.Date(from, format=date.fmt), to=as.Date(to, format=date.fmt), by="months" )

     if (out.type=="seq") return(vec.months)
     else if (out.type=="nmbr") return ( length(vec.months) )

} # 'mip' END
