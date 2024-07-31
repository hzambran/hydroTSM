%%File subhourly2nminutes.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2022-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{subhourly2nminutes}
\Rdversion{1.1}
\alias{subhourly2nminutes}
\alias{subhourly2nminutes.default}
\alias{subhourly2nminutes.zoo}
\alias{subhourly2nminutes.data.frame}
\alias{subhourly2nminutes.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sub-hourly -> n-minutes
}
\description{
Generic function for aggregating a sub-hourly time series into a "n-minutes' one.
}
\usage{
subhourly2nminutes(x, ...)

\method{subhourly2nminutes}{default}(x, nminutes, FUN, na.rm=TRUE, 
                                     from=start(x), to=end(x), ...)

\method{subhourly2nminutes}{zoo}(x, nminutes, FUN, na.rm=TRUE, 
                                 from=start(x), to=end(x), tz, ...)

\method{subhourly2nminutes}{data.frame}(x, nminutes, FUN, na.rm=TRUE, 
        from=start(x), to=end(x), dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", 
        out.fmt="zoo", verbose= TRUE, ...)

\method{subhourly2nminutes}{matrix}(x, nminutes, FUN, na.rm=TRUE, 
        from=start(x), to=end(x), dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", 
        out.fmt="zoo", verbose= TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with sub-hourly time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} represent the ID of each station.
}
  \item{nminutes}{
numeric, defining the amount of minutes to be used for aggregating \code{x}.
\code{nminutes} must be larger than the amount of minutes between each \code{x} value (computed as \code{time(x)[2]-time(x)[1]}).
}
  \item{FUN}{
Function that have to be applied for aggregating from sub-hourly into n-minutes time step. (e.g., for precipitation \code{FUN=sum} and for temperature and streamflow ts, \code{FUN=mean}). \cr
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the hourly values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA sub-hourly value within a day, the corresponding hourly value(s) will be NA as well
}
  \item{from}{
POSIX object indicating the starting time used to carry out the temporal aggregation.  \cr
When \code{from > start(x)} then \code{x} is cut in time to the starting DateTime defined by \code{from}. \cr
When \code{from < start(x)} then \code{x} is extended backward with NAs to the starting DateTime defined by \code{from}.
}
  \item{to}{
POSIX object indicating the ending time used to carry out the temporal aggregation.  \cr
When \code{to < end(x)} then \code{x} is cut in time to the ending DateTime defined by \code{to}. \cr
When \code{to > end(x)} then \code{x} is extended forward with NAs to the ending DateTime defined by \code{from}.
}
  \item{tz}{
character, with the specification of the time zone used for \code{x}, \code{from}, and \code{to}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr

If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr

If \code{tz} is provided, it forces \code{time(x)} to be in the tome zone specified by \code{tz}, without modifying the the values (hours, minutes, seconds, etc). \cr

A list of valid time zones can be obtained by calling the base function \code{OlsonNames()}. \cr

This argument can be used when working with sub-daily zoo objects to force using  time zones other than the local time zone for \code{from} and \code{to}. It should be used with caution, being well aware of the time zone of the data. See examples.
}
  \item{dates}{
numeric, factor, POSIXct or POSIXt object indicating how to obtain the dates and times for each column of \code{x} (e.g., gauging station) \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the date and times \cr
If \code{dates} is a factor, it is converted into POSIXct class, using the date format specified by \code{date.fmt}  \cr
If \code{dates} is already of POSIXct or POSIXt class, the code verifies that the number of elements on it be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{dates}, By default \code{date.fmt=\%Y-\%m-\%d \%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.fmt}{
OPTIONAL. Only used when \code{x} is a matrix or data.frame object /cr
character, for selecting if the result will be a matrix/data.frame or a zoo object. Valid values are: \kbd{numeric}, \kbd{zoo} (default)
}
  \item{verbose}{
logical; if TRUE, progress messages are printed 
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  a zoo object with hourly time series
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{subhourly2hourly}}, \code{\link{subdaily2daily}}, \code{\link{subdaily2monthly}}, \code{\link{subdaily2annual}}, \code{\link{subdaily2seasonal}}, \code{\link{as.POSIXct}}, \code{\link{dm2seasonal}}, \code{\link{monthlyfunction}}, \code{\link{seasonalfunction}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}
}
\examples{
## Creating a 5-min time sequence and counting its length
dt  <- seq( from=as.POSIXct("2021-06-30 00:00"), to=as.POSIXct("2021-06-30 23:55"), by="5 min" )
ndt <- length(dt)

## Creating a dummy 5-min zoo object, with 1 as the only value in each time period
x <- zoo( rep(1, ndt), dt)


## Aggregation from 5-minute single ts into 10-minute ts
h1 <- subhourly2nminutes(x, nminutes= 10, FUN=sum, na.rm=TRUE)

## Aggregation of 3 ts with 5-minute time frequency (i.e., a zoo matrix) 
## into a 30-minute zoo object.
X  <- cbind(x, x, x)
h2 <- subhourly2nminutes(X, nminutes= 30, FUN=sum, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
