################################################################################
# sfreq: Sampling frequency of a ts/zoo object                                 #
################################################################################
# This function generates a table indicating the number of days                #
# with information (<>NA's) within a data.frame                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: May 13th, 2009                                                      #
# Updates: March 2009, Nov 2010, April 2011                                    #
################################################################################
sfreq <- function(x, min.year=1800) {

  # Checking that 'class(x)'
  valid.class <- c("xts", "zoo", "ts")    
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0) 
     stop("Invalid argument: 'x' must be in c('xts', 'zoo', 'ts')" )

  if ( length(x) < 2) stop("Invalid argument: 'length(x)' must be larger than 2 for finding its sampling frequency" )

  t1 <- time(x[1])
  t2 <- time(x[2])

  if ( ( class(t1) == "character" ) & ( as.numeric(t1) > min.year ) ) {
    sfreq <- "annual"
  } else
      if ( ( t2 - t1 ) == 1)  {
        sfreq <- "daily"
      } else if ( ( t2 - t1 ) %in% c(28,29,30,31) )  {
        sfreq <- "monthly"
        } else if (  ( t2 - t1 ) %in% c(365, 366) )  {
          sfreq <- "annual"
        } else
          stop("Invalid argument: the sampling frequency of 'x' is not in c('daily', 'monthly', 'annual') " )

  return(sfreq)

} # 'sfreq' END
