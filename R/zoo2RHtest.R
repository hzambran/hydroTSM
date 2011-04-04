########################################################################
# zoo2RHtest: input file to the 'RHtest_dlyPrcp.r' script              #
########################################################################
#	                   Date: 26-Nov-2009                               #
########################################################################
# function for creating the input file to the 'RHtest_dlyPrcp.r' script,
# that test the homogeneity of climatological time series (http://ccma.seos.uvic.ca)

# 'x'        : time series that will be written. class(x) must be 'zoo'
# 'fname'    : filename of the output precipitation time series
# 'tstep.out': Character indicating the time step that have to be used for
#              writting 'x' into the output file
# 'dec'      : the string to use for decimal points in numeric or complex
#              columns: must be a single character.
# 'na'       : the string to use for missing values in the data

zoo2RHtest <- function(x, fname="pcp.txt", tstep.out="daily", dec=".", na="-999.0") {

  # Checking that 'class(x)' is 'ts' or 'zoo'
  if ( is.na( match(class(x), c("zoo") ) ) )
      stop("Invalid argument: 'class(x)' must be 'zoo'")

  # Checking 'tstep.out'
    if ( is.na( match(tstep.out, c("daily", "monthly", "annual") ) ) )
      stop("Invalid argument: 'tstep.out' must be in c('daily', monthly', 'annual')")

  pfreq <- sfreq(x)

  years  <- as.numeric(format(time(x), "%Y"))
  months <- as.numeric(format(time(x), "%m"))
  days   <- as.numeric(format(time(x), "%d"))
  values <- coredata(x)

  # Checking the user provide a valid value for 'x'
  if (is.na(match(sfreq(x), c("daily", "monthly", "annual")))) {
     stop(paste("Invalid argument: 'x' is not a daily, mothly or annual ts, it is a ", sfreq(x), " ts", sep="") ) }

  if ( tstep.out=="daily") {
    out <- data.frame(years=years, months=months, days=days, values=values)
  } else if ( tstep.out=="monthly") {
     out <- data.frame(years=years, months=months, days=0, values=values)
    } else if ( tstep.out=="monthly") {
      out <- data.frame(years=years, months=0, days=0, values=values)
      } # ELSE end

  write.table(out, file = fname, sep = " ",
              eol = "\n", na = na, dec = dec, row.names = FALSE,
              col.names = FALSE)


} # 'zoo2RHtest' end
