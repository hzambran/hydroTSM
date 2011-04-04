#################################################
# sfreq: Sampling frequency of a ts/zoo object  #
#################################################
#              May 13th, 2009                   #
#################################################
# This function generates a table indicating the number of days
# with information (<>NA's) within a data.frame

# 'x'        : variable of type 'zoo' or 'ts', with AT LEAST 2 elements, AND
#              with a Daily, Monthly or Annual sampling frequency.
# 'min.year' : integer used for a correct identification of the sampling fequency
#              when 'x' is an annual zoo (e.g.: time(x) = "1961") => the minimum possible years starts
#              in 'min.year'

# Result     : Possible values are:
#              -) 'daily'   : indicating that the sampling freqeuncy in 'x' is daily
#              -) 'monthly' : indicating that the sampling freqeuncy in 'x' is monthly
#              -) 'annual'  : indicating that the sampling freqeuncy in 'x' is annual
sfreq <- function(x, min.year=1800) {

  # Checking that 'class(x)==Date'
  if (is.na(match(class(x), c("zoo", "ts") ) ) )
     stop("Invalid argument: 'x' must be in c('zoo', 'ts')" )

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
