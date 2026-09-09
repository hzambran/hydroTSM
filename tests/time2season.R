library(hydroTSM)

################################################################################
# time2season                                                                  #
################################################################################

dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by="month")
months <- as.integer(format(dates, "%m"))
sondefm.months <- c(9, 10, 11, 12, 1, 2, 3)
expected <- ifelse(months %in% sondefm.months, "SONDEFM", NA)

sondefm <- time2season(dates, type="SONDEFM")
sondefm.seasons <- time2season(dates, type="SONDEFM", out.fmt="seasons")

stopifnot(
  identical(sondefm, expected),
  identical(sondefm.seasons, expected)
)

datetime <- as.POSIXct(dates, tz="UTC")
sondefm.datetime <- time2season(datetime, type="SONDEFM")

stopifnot(
  identical(sondefm.datetime, expected)
)
