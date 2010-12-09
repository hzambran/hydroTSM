##################################################################
# 'dip': Given any starting and ending dates, it generates:      #
#        1) a vector with all the days between the two dates, OR #
#	 2) the amount of days between the two dates             #
##################################################################

# 'from'    : Character indicating the starting date for computing the number of dyas.
#             It MUST have the date format specified by 'date.fmt'
# 'to'	    : Character indicating the ending date for computing the number of dyas.
#             It MUST have the date format specified by 'date.fmt'
# 'date.fmt': Character indicating the date format in which you provide 'from' and 'to'. (e.g., "%d-%m-%Y")
# 'out.type': Character indicating the type of result that is given by this function.
#             Valid values are:
#		      -) type= "seq"  => a vectorial sequence with all the days within the given time period
#		      -) type= "nmbr" => the number of days in the vectorial sequence with all the days within the given time period
dip <- function(from, to, date.fmt="%Y-%m-%d", out.type="seq") {

     # Checking 'out.type'
     if (is.na(match(out.type, c("seq", "nmbr"))))
        stop("Invalid argument: 'out.type' must be of class 'seq' or 'nmbr'")

     # Generating an Annual-regular time series of Dates.
     vec.days <- seq( from=as.Date(from, format=date.fmt), to=as.Date(to, format=date.fmt), by="days" )

     if (out.type=="seq") return(vec.days)
     else if (out.type=="nmbr") return ( length(vec.days) )

} # 'dip' END