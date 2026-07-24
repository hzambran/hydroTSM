%%File subhourly2nhourly.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{subhourly2nhourly}
\Rdversion{1.1}
\alias{subhourly2nhourly}
\alias{subhourly2nhourly.default}
\alias{subhourly2nhourly.zoo}
\alias{subhourly2nhourly.data.frame}
\alias{subhourly2nhourly.matrix}

\title{
Sub-hourly or hourly -> n-hourly
}
\description{
Generic function for aggregating a sub-hourly or hourly time series into a user-defined n-hourly one.
}
\usage{
subhourly2nhourly(x, ...)

\method{subhourly2nhourly}{default}(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt="\%H:\%M:\%S", tz, ...)

\method{subhourly2nhourly}{zoo}(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt="\%H:\%M:\%S", tz, ...)

\method{subhourly2nhourly}{data.frame}(x, FUN, n.hours, na.rm=TRUE,
           na.rm.max=0, start="00:00:00", start.fmt="\%H:\%M:\%S", tz,
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo",
           verbose=TRUE, ...)

\method{subhourly2nhourly}{matrix}(x, FUN, n.hours, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt="\%H:\%M:\%S", tz,
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo",
           verbose=TRUE, ...)
}
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with sub-hourly or hourly time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represents the time series measured in each gauging station, and the column names of \code{x} should correspond to the ID of each station.
}
  \item{FUN}{
Function that have to be applied for aggregating from sub-hourly or hourly into n-hourly time step (e.g., for precipitation \code{FUN=sum} and for temperature and streamflows ts \code{FUN=mean}). \cr

\code{FUN} MUST accept the \code{na.rm} argument, because \code{na.rm} is passed to \code{FUN}.
}
  \item{n.hours}{
positive integer indicating the length, in hours, of each output aggregation period.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the n-hourly values are computed only for periods with a percentage of missing values less than or equal to \code{na.rm.max} \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within an n-hour period, the corresponding n-hourly value in the output object will be \code{NA}.
}
  \item{na.rm.max}{
Numeric in [0, 1]. It is used to define the maximum percentage of missing values allowed in each n-hour period to keep the aggregated value in the output object of this function. In other words, if the percentage of missing values in a given n-hour period is larger than \code{na.rm.max}, the corresponding n-hourly value will be \code{NA}.
}
  \item{start}{
character, indicating the starting time used for anchoring the n-hour aggregation periods. It MUST be provided in the format specified by \code{start.fmt}. \cr
}
  \item{start.fmt}{
character indicating the format in which the time is provided in \code{start}, By default \code{date.fmt=\%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.POSIXct}}.
}
  \item{tz}{
character, with the specification of the time zone used in both \code{x} and \code{start}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr
If \code{tz} is provided, it forces \code{time(x)} to be in the time zone specified by \code{tz}, without modifying the values (hours, minutes, seconds, etc).
}
  \item{dates}{
numeric, factor, character, POSIXct or POSIXt object indicating how to obtain the dates and times for each column of \code{x} (e.g., gauging station) \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the date and times \cr
If \code{dates} is a factor or character, it is converted into POSIXct class, using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of POSIXct or POSIXt class, the code verifies that the number of elements on it be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{dates}, By default \code{date.fmt=\%Y-\%m-\%d \%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"}, \code{class(dates)=="character"} or \code{class(dates)=="numeric"}.
}
  \item{out.fmt}{
OPTIONAL. Only used when \code{x} is a matrix or data.frame object /cr
character, for selecting if the result will be a matrix/data.frame or a zoo object. Valid values are: \kbd{numeric}, \kbd{zoo} (default)
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
}
  \item{\dots}{
arguments additional to \code{na.rm} passed to \code{FUN}.
}
}
\details{
This function assumes that \code{x} has an hourly or sub-hourly temporal frequency. The output time index corresponds to the beginning of each n-hour aggregation period.
}
\value{
  a zoo object with n-hourly time series
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
\seealso{
\code{\link{subhourly2hourly}}, \code{\link{subhourly2nminutes}},
\code{\link{subdaily2daily}}, \code{\link{subdaily2monthly}},
\code{\link{subdaily2annual}}, \code{\link{subdaily2seasonal}},
\code{\link{as.POSIXct}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}},
\code{\link{izoo2rzoo}}
}
\examples{
## Creating a 30-min time sequence and counting its length
dt  <- seq(from=as.POSIXct("2021-06-30 00:30", tz="UTC"),
           to=as.POSIXct("2021-06-30 23:30", tz="UTC"), by="30 min")
ndt <- length(dt)

## Creating a dummy 30-min zoo object, with 1 as the only value in each time period
x <- zoo(rep(1, ndt), dt)

## Aggregation from 30-minute single ts into 3-hourly ts
h1 <- subhourly2nhourly(x, FUN=sum, n.hours=3, na.rm=TRUE)

## Aggregation of 3 ts with 30-minute time frequency (i.e., a zoo matrix)
## into a 6-hourly zoo object.
X  <- cbind(x, x, x)
h2 <- subhourly2nhourly(X, FUN=sum, n.hours=6, na.rm=TRUE)

## Hourly input into 6-hourly output
h  <- subhourly2hourly(x, FUN=sum)
h3 <- subhourly2nhourly(h, FUN=sum, n.hours=6, na.rm=TRUE)
}
\keyword{manip}
