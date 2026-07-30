%%File change_tz.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{change_tz}
\Rdversion{1.1}
\alias{change_tz}
\alias{change_tz.zoo}

\title{
Change the time zone of a sub-daily zoo object
}
\description{
Changes the time zone used by the time index of a sub-daily \code{zoo} object
whose index inherits from \code{POSIXct} or \code{POSIXlt}.
}
\usage{
change_tz(x, ...)

\method{change_tz}{zoo}(x, new.tz, old.tz=NULL, ...)
}
\arguments{
  \item{x}{
\code{zoo} object with a sub-daily \code{POSIXct} or \code{POSIXlt} time
index.
}
  \item{new.tz}{
character. Time zone to assign to the output time index.
}
  \item{old.tz}{
character or \code{NULL}. Time zone currently used by the time index. When
\code{NULL}, it is inferred from \code{attr(time(x), "tzone")}. When provided,
it must match the detected time zone when the time index already has one.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
Both \code{old.tz} and \code{new.tz} are checked against
\code{\link[base]{OlsonNames}}.

\code{change_tz()} preserves the represented instants and changes their time
zone for display and downstream calendar grouping. For example, midnight UTC is
shown as the previous evening in \code{"America/Santiago"} when the applicable
UTC offset is negative. It does not reinterpret clock labels in a new time zone.

The values and dimensions of \code{x} are preserved; only the time index is
changed.
}
\value{
A \code{zoo} object with the same values as \code{x}, but with its time index
converted to \code{new.tz}.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\seealso{
\code{\link[zoo]{zoo}}, \code{\link[base]{OlsonNames}},
\code{\link[base]{as.POSIXct}}, \code{\link{subhourly2hourly}},
\code{\link{subhourly2nhourly}}, \code{\link{subdaily2daily}}
}
\examples{
dates <- seq(as.POSIXct("2001-01-01 00:00:00", tz="UTC"),
             by="hour", length.out=3)
x <- zoo(1:3, dates)

y <- change_tz(x, new.tz="America/Santiago")
time(y)

## The represented instants and the observed values are preserved.
identical(as.numeric(time(x)), as.numeric(time(y)))
identical(coredata(x), coredata(y))
}
\keyword{manip}
\keyword{ts}
