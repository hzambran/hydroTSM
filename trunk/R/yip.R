#############################################################################
# 'yip': Given any starting and ending dates, it generates:                 #
#        1) a vector of dates with all the years between the two dates, OR  #
#		     2) the amount of years between the two dates                       #
#############################################################################
# 18-May-2010

# 'from'	: Starting date for computing the number of years. MUST have the date format specified by 'date.fmt'
# 'to'		: Ending date for computing the number of years. MUST have the date format specified by 'date.fmt'
# 'date.fmt': Format of the dates (e.g., "%d-%m-%Y")
# out.type  : type of result that is given by this function
#		      -) type= "seq"  => a vectorial sequence with all the months within the given year
#		      -) type= "nmbr" => the number of days in the vectorial sequence with all the months within the given year
yip <- function(from, to, date.fmt="%Y-%m-%d", out.type="seq") {


     # Checking 'out.type'
     if (is.na(match(out.type, c("seq", "nmbr"))))
        stop("Invalid argument: 'out.type' must be of class 'seq' or 'nmbr'")

     # Generating an Annual-regular time series of Dates.
     vec.years <- seq( from=as.Date(from, format=date.fmt), to=as.Date(to, format=date.fmt), by="years" )

     if (out.type=="seq") return(vec.years)
     else if (out.type=="nmbr") return ( length(vec.years) )

} # 'yip' END