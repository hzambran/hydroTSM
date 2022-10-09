%%File subhourly2hourly.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{subhourly2hourly}
\Rdversion{1.1}
\alias{subhourly2hourly}
\alias{subhourly2hourly.default}
\alias{subhourly2hourly.zoo}
\alias{subhourly2hourly.data.frame}
\alias{subhourly2hourly.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sub-daily -> Daily
}
\description{
Generic function for transforming a Sub-DAILY time series into a DAILY one
}
\usage{
subhourly2hourly(x, ...)

\method{subhourly2hourly}{default}(x, FUN, na.rm = TRUE, start="00:00:00", 
                          start.fmt= "\%H:\%M:\%S", tz="UTC", ...)

\method{subhourly2hourly}{zoo}(x, FUN, na.rm = TRUE, start="00:00:00", 
                         start.fmt= "\%H:\%M:\%S", tz="UTC", ...)

\method{subhourly2hourly}{data.frame}(x, FUN, na.rm = TRUE, start="00:00:00", 
           start.fmt= "\%H:\%M:\%S", tz="UTC",
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)

\method{subhourly2hourly}{matrix}(x, FUN, na.rm = TRUE, start="00:00:00", 
           start.fmt= "\%H:\%M:\%S", tz="UTC",
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with sub-hourly time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} represent the ID of each station.
}
  \item{FUN}{
Function that have to be applied for transforming from sub-hourly to hourly time step. (e.g., for precipitation \code{FUN=sum} and for temperature and streamflow ts, \code{FUN=mean}). \cr
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the hourly values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA sub-hourly value within a day, the corresponing hourly value(s) will be NA as well
}
  \item{start}{
character, indicating the starting time used for aggregating sub-hourly time series into hourly ones. It MUST be provided in the format specified by \code{start.fmt}. \cr
This value is used to define the time when a new day begins. \cr
-) All the values of \code{x} with a time attribute before \code{start} are considered as belonging to the day before the one indicated in the time attribute of those values. \cr
-) All the values of \code{x} with a time attribute equal to \code{start} are considered to be equal to \code{"00:00:00"} in the output zoo object. \cr
-) All the values of \code{x} with a time attribute after \code{start} are considered as belonging to the same day as the one indicated in the time attribute of those values. \cr

It is useful when the daily values start at a time different from \code{"00:00:00"}. Use with caution. See examples.
}
  \item{start.fmt}{
character indicating the format in which the time is provided in \code{start}, By default \code{date.fmt=\%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.POSIXct}}.
}
  \item{tz}{
character, indicating the time zone specification to be used for both \code{start} and \code{dates}. System-specific, but \kbd{"GMT"} is UTC (Universal Time Coordinated), the dault, and \kbd{""} is the current time zone.
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
\code{\link{subdaily2daily}}, \code{\link{subdaily2monthly}}, \code{\link{subdaily2annual}}, \code{\link{subdaily2seasonal}}, \code{\link{as.POSIXct}}, \code{\link{dm2seasonal}}, \code{\link{monthlyfunction}}, \code{\link{seasonalfunction}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}
}
\examples{
## Creating a 15-min time sequence and counting its length
dt  <- seq( from=as.POSIXct("2021-06-30 00:15"), to=as.POSIXct("2021-06-30 23:45"), by="15 min" )
ndt <- length(dt)

## Creating a dummy 15-min zoo object, with 1 as the only value in each time period
x <- zoo( rep(1, ndt), dt)


## sub-hourly to hourly
h1 <- subhourly2hourly(x, FUN=sum, na.rm=TRUE)

## sub-hourly to hourly, considering that the new day starts at 08:00:00
h2 <- subhourly2hourly(x, FUN=sum, na.rm=TRUE, start="08:00:00")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
