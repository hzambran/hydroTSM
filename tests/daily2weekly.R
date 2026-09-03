library(hydroTSM)

################################################################################
# daily2weekly                                                                 #
################################################################################

dates <- as.Date("2023-01-01") + 0:13
x <- zoo::zoo(1:14, dates)

w.default <- daily2weekly(x, FUN=sum)
w.dates <- daily2weekly(x, FUN=sum, week.date.format="%Y-%m-%d")
w.sequential <- daily2weekly(x, FUN=sum, week.grouping="sequential",
                             week.date.format="%Y-%m-%d")

stopifnot(
  identical(as.character(time(w.default)), c("2023-00", "2023-01", "2023-02")),
  identical(as.numeric(w.default), c(1, 35, 69)),
  inherits(time(w.dates), "Date"),
  identical(time(w.dates), as.Date(c("2023-01-01", "2023-01-02", "2023-01-09"))),
  identical(as.numeric(w.dates), as.numeric(w.default)),
  inherits(time(w.sequential), "Date"),
  identical(time(w.sequential), as.Date(c("2023-01-01", "2023-01-08"))),
  identical(as.numeric(w.sequential), c(28, 77))
)

df <- data.frame(date=dates, A=1:14)

df.numeric <- daily2weekly(df, FUN=sum, out.fmt="numeric")
df.zoo <- daily2weekly(df, FUN=sum, out.fmt="zoo",
                       week.date.format="%Y-%m-%d")
df.zoo.sequential <- daily2weekly(df, FUN=sum, out.fmt="zoo",
                                  week.date.format="%Y-%m-%d",
                                  week.grouping="sequential")
df.db <- daily2weekly(df, FUN=sum, out.type="db", verbose=FALSE)
df.db.dates <- daily2weekly(df, FUN=sum, out.type="db", verbose=FALSE,
                            week.date.format="%Y-%m-%d")
df.db.sequential <- daily2weekly(df, FUN=sum, out.type="db", verbose=FALSE,
                                 week.date.format="%Y-%m-%d",
                                 week.grouping="sequential")

stopifnot(
  identical(rownames(df.numeric), c("2023-00", "2023-01", "2023-02")),
  identical(as.numeric(df.numeric[, "A"]), c(1, 35, 69)),
  inherits(time(df.zoo), "Date"),
  identical(time(df.zoo), as.Date(c("2023-01-01", "2023-01-02", "2023-01-09"))),
  identical(as.numeric(df.zoo), c(1, 35, 69)),
  inherits(time(df.zoo.sequential), "Date"),
  identical(time(df.zoo.sequential), as.Date(c("2023-01-01", "2023-01-08"))),
  identical(as.numeric(df.zoo.sequential), c(28, 77)),
  identical(as.character(df.db$Year), c("2023", "2023", "2023")),
  identical(as.character(df.db$Week), c("00", "01", "02")),
  identical(as.numeric(df.db$Value), c(1, 35, 69)),
  identical(as.character(df.db.dates$Week),
            c("2023-01-01", "2023-01-02", "2023-01-09")),
  identical(as.numeric(df.db.dates$Value), c(1, 35, 69)),
  identical(as.character(df.db.sequential$Week),
            c("2023-01-01", "2023-01-08")),
  identical(as.numeric(df.db.sequential$Value), c(28, 77))
)

mx <- matrix(1:14, ncol=1, dimnames=list(NULL, "A"))
mx.numeric <- daily2weekly(mx, FUN=sum, out.fmt="numeric",
                           dates=dates,
                           week.date.format="%Y-%m-%d")

stopifnot(
  identical(rownames(mx.numeric),
            c("2023-01-01", "2023-01-02", "2023-01-09")),
  identical(as.numeric(mx.numeric[, "A"]), c(1, 35, 69))
)

invalid.format <- try(
  daily2weekly(x, FUN=sum, week.date.format="%Y-%U"),
  silent=TRUE
)
invalid.grouping <- try(
  daily2weekly(x, FUN=sum, week.grouping="iso"),
  silent=TRUE
)

Date.Ini <- "1981-01-01"
Date.Fin <- "2025-12-31"
long.dates <- hydroTSM::dip(Date.Ini, Date.Fin)
long.z <- zoo::zoo(seq_along(long.dates), long.dates)
long.calendar <- daily2weekly(long.z, FUN=mean)
long.sequential <- daily2weekly(long.z, FUN=mean, week.grouping="sequential",
                                week.date.format="%Y-%m-%d")
expected.weeks <- seq(as.Date(Date.Ini), as.Date(Date.Fin), by="weeks")

stopifnot(
  inherits(invalid.format, "try-error"),
  inherits(invalid.grouping, "try-error"),
  identical(length(long.calendar), 2387L),
  identical(length(long.sequential), length(expected.weeks)),
  isTRUE(all.equal(time(long.sequential), expected.weeks))
)
