# File vector2zoo.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2008-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# vector2zoo: Transforms a numeric vector 'x' with a corresponding             #
#             'Date' vector (any format) into a 'zoo' object                   #
################################################################################
# Author    : Mauricio Zambrano-Bigiarini                                      #
################################################################################
# Started   : 17-Dec-2008                                                      #
# Updates   : 01-Oct-2009                                                      #
#             06-Oct-2010                                                      #
#             05-May-2011                                                      #
#             15-Oct-2012 ; 16-Oct-2012                                        #     
#             29-May-2013                                                      #               
################################################################################
#  Transform a numeric vector and its corresponding dates into a 'zoo' object

# 'x':      : numeric vector
# 'dates'   : vector with a complete series of dates, in the same order of 'ts'
# 'date.fmt': format in which the dates are stored in 'dates'
vector2zoo <- function(x, dates, date.fmt="%Y-%m-%d") {

  if (is.na(match(class(dates)[1], c("Date", "POSIXct", "POSIXlt", "character", "factor"))))
      stop("Invalid argument: 'class(dates)' must be in c('Date', 'POSIXct', 'POSIXlt', 'character', 'factor')")
      
  if (length(x) != length(dates)) 
     stop("Invalid argument: length(x) != length(dates) (", length(x), "!=", length(dates), ")")

  # If 'dates' is a factor or character, it have to be converted into 'Date' class, 
  # using the date format  specified by 'date.fmt'
  if ( class(dates)[1] %in% c("factor", "character") ) {
     ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
            grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
            grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE),
            subdaily.date.fmt <- TRUE, subdaily.date.fmt <- FALSE )
     ifelse(subdaily.date.fmt, dates <- as.POSIXct(dates, format= date.fmt), 
                               dates <- as.Date(dates, format= date.fmt) )  
  } # IF end 

  return( zoo(x, dates) )

}  # 'vector2zoo' END
