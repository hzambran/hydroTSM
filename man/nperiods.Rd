%%File nperiods.Rd
%% Part of the hydroTSM R package
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{nperiods}
\Rdversion{1.1}
\alias{nperiods}
\alias{nyears}
\alias{nyears.zoo}
\alias{nmonths}
\alias{nmonths.zoo}
\alias{nweeks}
\alias{nweeks.zoo}
\alias{ndays}
\alias{ndays.zoo}
\alias{nhours}
\alias{nhours.zoo}

\title{
Number of Years, Months, Weeks, Days, or Hours in a zoo Object
}
\description{
Generic functions for computing the temporal span covered by a
\code{\link[zoo]{zoo}} object, expressed as years, months, weeks, days, or
hours.
}
\usage{
nyears(x, ...)
nmonths(x, ...)
nweeks(x, ...)
ndays(x, ...)
nhours(x, ...)

\method{nyears}{zoo}(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
\method{nmonths}{zoo}(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
\method{nweeks}{zoo}(x,
                     week.grouping="calendar",
                     verbose=TRUE, ...)
\method{ndays}{zoo}(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
\method{nhours}{zoo}(x, only.integer.output=FALSE, tz, verbose=TRUE, ...)
}
\arguments{
  \item{x}{
A zoo object with a regular time index.
}
  \item{only.integer.output}{
logical; if \code{FALSE} (default), decimal values are returned when the last
year, month, day, or hour is not complete. If \code{TRUE}, only the number of
whole years, months, days, or hours is returned, and a warning is issued when
the last period is incomplete.
}
  \item{week.grouping}{
character indicating how weeks are counted by \code{nweeks}. Valid values are:
\cr
-) \kbd{calendar}: distinct calendar year-week groups defined by
\kbd{\%Y-\%W}. Weeks crossing a calendar-year boundary are split. \cr
-) \kbd{sequential}: consecutive 7-day groups starting at the first date in
\code{x}. This argument is fully compatible with \code{\link{daily2weekly}}.
}
  \item{tz}{
character, indicating the time zone in which the time index of \code{x} is
located. If missing, the time zone stored in the index is used when available.
See \code{\link{isComplete}}.
}
  \item{verbose}{
logical; if \code{TRUE}, informative messages are shown in the screen.
The functions call \code{\link{isComplete}} and, when \code{x} has missing
date/time values, they are reported by \code{isComplete}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
The returned value is computed from the first date/time to the inferred end of
the last observation. The end of the last observation is inferred from the
regular time step of \code{x}. For example, a daily object from
\code{2020-01-01} to \code{2020-01-31} returns \code{1} month and \code{31}
days.

Before computing the temporal span, the functions call
\code{isComplete(x, out.type="all")}. Therefore, incomplete objects are still
accepted when their time index has a regular frequency, but missing date/time
values are reported when \code{verbose=TRUE}.

For \code{nweeks}, the result is the number of weekly groups that
\code{daily2weekly} would produce with the same \code{week.grouping}.
}
\value{
A numeric value with the temporal span of \code{x} expressed in the requested
unit. \code{nweeks} returns the integer number of weekly groups. For the other
functions, an integer value is returned when \code{only.integer.output=TRUE}.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\seealso{
\code{\link{isComplete}}, \code{\link{daily2weekly}}, \code{\link[zoo]{zoo}}
}
\examples{
dates <- as.Date("2020-01-01") + 0:30
x <- zoo::zoo(seq_along(dates), dates)

nyears(x, verbose=FALSE)
nmonths(x, verbose=FALSE)
nweeks(x, verbose=FALSE)
ndays(x, verbose=FALSE)
nhours(x, verbose=FALSE)

y <- x[1:15]
ndays(y, verbose=FALSE)
ndays(y, only.integer.output=TRUE, verbose=FALSE)
}
