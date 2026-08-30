library(hydroTSM)

################################################################################
# isComplete                                                                  #
################################################################################

daily.dates <- seq(as.Date("2020-01-01"), by="day", length.out=10)
daily <- zoo::zoo(seq_along(daily.dates), daily.dates)

daily.complete <- isComplete(daily, out.type="all", verbose=FALSE)
daily.incomplete <- isComplete(daily[-5], out.type="all", verbose=FALSE)

stopifnot(
  identical(daily.complete$isComplete, TRUE),
  identical(daily.complete$NumberMissingDT, 0L),
  inherits(daily.complete$missingDateTimes, "Date"),
  length(daily.complete$missingDateTimes) == 0L,
  identical(daily.incomplete$isComplete, FALSE),
  identical(daily.incomplete$NumberMissingDT, 1L),
  identical(daily.incomplete$missingDateTimes, daily.dates[5])
)

hourly.times <- seq(as.POSIXct("2020-01-01 00:00:00", tz="UTC"),
                    by="hour", length.out=10)
hourly <- zoo::zoo(seq_along(hourly.times), hourly.times)

hourly.complete <- isComplete(hourly, out.type="all", verbose=FALSE)
hourly.incomplete <- isComplete(hourly[-5], out.type="all", verbose=FALSE)

stopifnot(
  identical(hourly.complete$isComplete, TRUE),
  identical(hourly.complete$NumberMissingDT, 0L),
  inherits(hourly.complete$missingDateTimes, "POSIXct"),
  length(hourly.complete$missingDateTimes) == 0L,
  identical(hourly.incomplete$isComplete, FALSE),
  identical(hourly.incomplete$NumberMissingDT, 1L),
  isTRUE(all.equal(hourly.incomplete$missingDateTimes, hourly.times[5]))
)
