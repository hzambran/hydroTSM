library(hydroTSM)

################################################################################
# time2season                                                                  #
################################################################################

dates <- seq(as.Date("2000-01-01"), as.Date("2001-12-01"), by="month")
months <- as.integer(format(dates, "%m"))
years <- as.integer(format(dates, "%Y"))
sondjfm.months <- c(9, 10, 11, 12, 1, 2, 3)
expected <- ifelse((months %in% sondjfm.months) &
                   !((years == min(years)) & (months < 9)),
                   "SONDJFM", NA)

sondjfm <- time2season(dates, type="SONDJFM")
sondjfm.seasons <- time2season(dates, type="SONDJFM", out.fmt="seasons")

stopifnot(
  identical(sondjfm, expected),
  identical(sondjfm.seasons, expected)
)

datetime <- as.POSIXct(dates, tz="UTC")
sondjfm.datetime <- time2season(datetime, type="SONDJFM")
old.type <- try(time2season(dates, type="SONDEFM"), silent=TRUE)

stopifnot(
  identical(sondjfm.datetime, expected),
  inherits(old.type, "try-error")
)
