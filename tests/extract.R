library(hydroTSM)

################################################################################
# extract                                                                      #
################################################################################

dates <- seq(as.Date("2000-01-01"), as.Date("2001-12-31"), by="day")
x <- zoo::zoo(seq_along(dates), dates)

wet.months <- c(9, 10, 11, 12, 1, 2, 3)
wet <- extract(x, trgt=wet.months)
wet.special <- extract(x, trgt="SONDEFM")
expected.index <- which(as.integer(format(dates, "%m")) %in% wet.months)

stopifnot(
  inherits(wet, "zoo"),
  identical(zoo::index(wet), dates[expected.index]),
  identical(as.numeric(wet), as.numeric(x[expected.index])),
  identical(wet.special, wet),
  identical(sort(unique(as.integer(format(zoo::index(wet), "%m")))),
            sort(as.integer(wet.months)))
)

years <- extract(x, trgt=c(2000, 2001))

stopifnot(
  identical(zoo::index(years), dates),
  identical(as.numeric(years), as.numeric(x))
)

mixed.trgt <- try(extract(x, trgt=c(1, 2000)), silent=TRUE)
invalid.month <- try(extract(x, trgt=0), silent=TRUE)

stopifnot(
  inherits(mixed.trgt, "try-error"),
  inherits(invalid.month, "try-error")
)
