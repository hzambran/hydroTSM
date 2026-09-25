library(hydroTSM)

################################################################################
# nyears, nmonths, nweeks, ndays, nhours                                      #
################################################################################

# Checking a complete daily zoo object
daily.dates <- as.Date("2020-01-01") + 0:30
x.daily <- zoo::zoo(seq_along(daily.dates), daily.dates)
stopifnot(identical(nyears(x.daily, verbose=FALSE), 31 / 366))
stopifnot(identical(nmonths(x.daily, verbose=FALSE), 1))
stopifnot(identical(ndays(x.daily, verbose=FALSE), 31))
stopifnot(identical(nhours(x.daily, verbose=FALSE), 31 * 24))

# Checking a complete hourly zoo object
hourly.times <- as.POSIXct("2020-01-01 00:00:00", tz="UTC") + 0:23 * 3600
x.hourly <- zoo::zoo(seq_along(hourly.times), hourly.times)
stopifnot(identical(nhours(x.hourly, verbose=FALSE), 24))
stopifnot(identical(ndays(x.hourly, verbose=FALSE), 1))

# Checking calendar and sequential weekly groups across a year boundary
weekly.dates <- as.Date("2020-12-28") + 0:9
x.weekly <- zoo::zoo(seq_along(weekly.dates), weekly.dates)
stopifnot(identical(nweeks(x.weekly, verbose=FALSE), 3L))
stopifnot(identical(nweeks(x.weekly, week.grouping="sequential",
                           verbose=FALSE), 2L))

# Checking a multivariate zoo object
x.multivariate <- zoo::zoo(cbind(a=seq_along(daily.dates),
                                  b=2 * seq_along(daily.dates)), daily.dates)
stopifnot(identical(nmonths(x.multivariate, verbose=FALSE), 1))
stopifnot(identical(nweeks(x.multivariate, verbose=FALSE), 5L))

# Checking monthly and annual zoo objects
monthly.index <- zoo::as.yearmon(sprintf("2020-%02d", 1:12))
x.monthly <- zoo::zoo(seq_along(monthly.index), monthly.index)
stopifnot(identical(nmonths(x.monthly, verbose=FALSE), 12))
stopifnot(identical(nyears(x.monthly, verbose=FALSE), 1))

annual.index <- 2001:2010
x.annual <- zoo::zoo(seq_along(annual.index), annual.index)
stopifnot(identical(nyears(x.annual, verbose=FALSE), 10))
stopifnot(identical(nmonths(x.annual, verbose=FALSE), 120))

# Checking integer and decimal outputs for a partial month
partial.dates <- as.Date("2020-01-01") + 0:14
x.partial <- zoo::zoo(seq_along(partial.dates), partial.dates)
stopifnot(identical(ndays(x.partial, verbose=FALSE), 15))
stopifnot(identical(ndays(x.partial, only.integer.output=TRUE,
                          verbose=FALSE), 15L))

warning.message <- NA_character_
out.integer <- withCallingHandlers(
  nmonths(x.partial, only.integer.output=TRUE, verbose=FALSE),
  warning=function(w) {
    warning.message <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  } # 'warning' END
)
stopifnot(identical(out.integer, 0L))
stopifnot(grepl("last month is not complete", warning.message, fixed=TRUE))

# Checking the completeness message for a missing date
missing.dates <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-04"))
x.missing <- zoo::zoo(seq_along(missing.dates), missing.dates)
message.output <- capture.output(out <- ndays(x.missing, verbose=TRUE),
                                 type="message")
stopifnot(identical(out, 4))
stopifnot(any(grepl("Missing date/times are: 2020-01-03", message.output,
                    fixed=TRUE)))

# Checking invalid arguments and unsupported time indices
stopifnot(inherits(try(nyears(1:10), silent=TRUE), "try-error"))
stopifnot(inherits(try(ndays(x.daily[1], verbose=FALSE), silent=TRUE),
                   "try-error"))
stopifnot(inherits(try(nmonths(x.daily, only.integer.output=NA,
                              verbose=FALSE), silent=TRUE), "try-error"))
stopifnot(inherits(try(nweeks(x.monthly, verbose=FALSE), silent=TRUE),
                   "try-error"))
