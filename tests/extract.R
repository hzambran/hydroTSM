library(hydroTSM)

################################################################################
# extract                                                                      #
################################################################################

dates <- seq(as.Date("2000-01-01"), as.Date("2001-12-31"), by="day")
x <- zoo::zoo(seq_along(dates), dates)

wet.months <- c(9, 10, 11, 12, 1, 2, 3)
wet <- extract(x, trgt=wet.months)
wet.special <- extract(x, trgt="SONDJFM")
date.months <- as.integer(format(dates, "%m"))
date.years <- as.integer(format(dates, "%Y"))
expected.index <- which((date.months %in% wet.months) &
                        !((date.years == min(date.years)) & (date.months < 9)))
same.year.months <- c(1, 2, 3, 9, 10, 11, 12)
same.year <- extract(x, trgt=same.year.months)
same.year.index <- which(date.months %in% same.year.months)

stopifnot(
  inherits(wet, "zoo"),
  identical(zoo::index(wet), dates[expected.index]),
  identical(as.numeric(wet), as.numeric(x[expected.index])),
  identical(wet.special, wet),
  identical(zoo::index(same.year), dates[same.year.index]),
  identical(as.numeric(same.year), as.numeric(x[same.year.index])),
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
old.special <- try(extract(x, trgt="SONDEFM"), silent=TRUE)

stopifnot(
  inherits(mixed.trgt, "try-error"),
  inherits(invalid.month, "try-error"),
  inherits(old.special, "try-error")
)
