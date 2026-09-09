library(hydroTSM)

################################################################################
# submonthly2monthly                                                           #
################################################################################

hourly.dates <- as.POSIXct("2023-01-31 00:00:00", tz="UTC") + 3600*(0:47)
hourly <- zoo::zoo(1:48, hourly.dates)
hourly.monthly <- submonthly2monthly(hourly, FUN=sum, na.rm=TRUE)

daily.dates <- as.Date("2023-01-01") + 0:40
daily <- zoo::zoo(seq_along(daily.dates), daily.dates)
daily.monthly <- submonthly2monthly(daily, FUN=sum, na.rm=TRUE)
daily.reference <- daily2monthly(daily, FUN=sum, na.rm=TRUE)

weekly.dates <- as.Date("2023-01-01") + 7*(0:8)
weekly <- zoo::zoo(seq_along(weekly.dates), weekly.dates)
weekly.monthly <- submonthly2monthly(weekly, FUN=sum, na.rm=TRUE)

pseudo.weekly.dates <- as.Date(c("2023-01-01", "2023-01-08", "2023-01-15",
                                 "2023-01-23", "2023-02-01", "2023-02-10"))
pseudo.weekly <- zoo::zoo(seq_along(pseudo.weekly.dates), pseudo.weekly.dates)
pseudo.weekly.monthly <- submonthly2monthly(pseudo.weekly, FUN=sum, na.rm=TRUE)

pseudo.weekly.na <- zoo::zoo(c(1, NA, 3, 4, 5, 6), pseudo.weekly.dates)
pseudo.weekly.na.monthly <- submonthly2monthly(pseudo.weekly.na, FUN=sum,
                                               na.rm=TRUE, na.rm.max=0.2)

stopifnot(
  inherits(time(hourly.monthly), "Date"),
  identical(time(hourly.monthly), as.Date(c("2023-01-01", "2023-02-01"))),
  identical(as.numeric(hourly.monthly), c(300, 876)),
  inherits(time(daily.monthly), "Date"),
  identical(time(daily.monthly), time(daily.reference)),
  identical(as.numeric(daily.monthly), as.numeric(daily.reference)),
  inherits(time(weekly.monthly), "Date"),
  identical(time(weekly.monthly), as.Date(c("2023-01-01", "2023-02-01"))),
  identical(as.numeric(weekly.monthly), c(15, 30)),
  inherits(time(pseudo.weekly.monthly), "Date"),
  identical(time(pseudo.weekly.monthly), as.Date(c("2023-01-01", "2023-02-01"))),
  identical(as.numeric(pseudo.weekly.monthly), c(10, 11)),
  is.na(as.numeric(pseudo.weekly.na.monthly)[1]),
  identical(as.numeric(pseudo.weekly.na.monthly)[2], 11)
)

df <- data.frame(date=pseudo.weekly.dates, A=seq_along(pseudo.weekly.dates))
df.numeric <- submonthly2monthly(df, FUN=sum, out.fmt="numeric")
df.zoo <- submonthly2monthly(df, FUN=sum, out.fmt="zoo")
df.db <- submonthly2monthly(df, FUN=sum, out.type="db", verbose=FALSE)

stopifnot(
  identical(rownames(df.numeric), c("Jan-2023", "Feb-2023")),
  identical(as.numeric(df.numeric[, "A"]), c(10, 11)),
  inherits(time(df.zoo), "Date"),
  identical(time(df.zoo), as.Date(c("2023-01-01", "2023-02-01"))),
  identical(as.numeric(df.zoo), c(10, 11)),
  identical(as.character(df.db$Year), c("2023", "2023")),
  identical(as.character(df.db$Month), c("Jan", "Feb")),
  identical(as.numeric(df.db$Value), c(10, 11))
)

monthly <- zoo::zoo(1:3, as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")))
monthly.err <- try(submonthly2monthly(monthly, FUN=sum), silent=TRUE)

stopifnot(inherits(monthly.err, "try-error"))
