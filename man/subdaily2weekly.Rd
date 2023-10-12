%%File daily2weekly.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{subdaily2weekly}
\Rdversion{1.1}
\alias{subdaily2weekly}
\alias{subdaily2weekly.default}
\alias{subdaily2weekly.zoo}
\alias{subdaily2weekly.data.frame}
\alias{subdaily2weekly.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subdaily -> Weekly
}
\description{
Generic function for transforming a DAILY (or sub-daily) regular time series into a WEEKLY one
}
\usage{
subdaily2weekly(x, ...)

\method{subdaily2weekly}{default}(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{subdaily2weekly}{zoo}(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{subdaily2weekly}{data.frame}(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz,
           dates=1, date.fmt = "\%Y-\%m-\%d \%H:\%M:\%S",  
           out.fmt="zoo", verbose=TRUE, ...)

\method{subdaily2weekly}{matrix}(x, FUN, na.rm=TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz,
           dates=1, date.fmt = "\%Y-\%m-\%d \%H:\%M:\%S",  
           out.fmt="zoo", verbose=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with (sub)daily time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represents the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that have to be applied for transforming from daily to weekly time step (e.g., for precipitation \code{FUN=sum} and for temperature and streamflow ts \code{FUN=mean}). \cr

\code{FUN} MUST accept the \code{na.rm} argument, because \code{na.rm} is passed to \code{FUN}.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the weekly values are computed only for weeks with a percentage of missing values less than \code{na.rm.max} \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the corresponing weekly values in the output object will be \code{NA}.
}
  \item{na.rm.max}{
Numeric in [0, 1]. It is used to define the maximum percentage of missing values allowed in each month to keep the weekly aggregated value in the output object of this function. In other words, if the percentage of missing values in a given month is larger or equal than \code{na.rm.max} the corresponding weekly value will be \code{NA}.
}
  \item{start}{
character, indicating the starting time used for aggregating sub-daily time series into daily ones. It MUST be provided in the format specified by \code{start.fmt}. \cr
This value is used to define the time when a new day begins (e.g., for some rain gauge stations). \cr
-) All the values of \code{x} with a time attribute before \code{start} are considered as belonging to the day before the one indicated in the time attribute of those values. \cr
-) All the values of \code{x} with a time attribute equal to \code{start} are considered to be equal to \code{"00:00:00"} in the output zoo object. \cr
-) All the values of \code{x} with a time attribute after \code{start} are considered as belonging to the same day as the one indicated in the time attribute of those values. \cr

It is useful when the daily values start at a time different from \code{"00:00:00"}. Use with caution. See examples.
}
  \item{start.fmt}{
character indicating the format in which the time is provided in \code{start}, By default \code{date.fmt=\%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.POSIXct}}.
}
  \item{tz}{
character, with the specification of the time zone used in both \code{x} and \code{start}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr
This argument can be used to force using the local time zone or any other time zone instead of UTC as time zone.
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates for each gauging station \cr
If \code{dates} is a number (default), it indicates the index of the column in \var{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format specified by \code{date.fmt}  \cr
If \code{dates} is already of Date class, the code verifies that the number of days on it be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the DateTime objects are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d \%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.POSIXct}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.fmt}{
OPTIONAL. Only used when \code{x} is a matrix or data.frame object /cr
character, for selecting if the result will be a matrix/data.frame or a zoo object. Valid values are: \kbd{numeric}, \kbd{zoo}.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed 
}
  \item{\dots}{
arguments additional to \code{na.rm} passed to \code{FUN}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
a zoo object with weekly time frequency
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
\code{\link{cmv}}, \code{\link{subhourly2hourly}}, \code{\link{daily2monthly}}, \code{\link{daily2annual}}, \code{\link{subdaily2daily}}, \code{\link{weeklyfunction}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}, \code{\link{as.Date}}
}
\examples{
######################
## Ex1: Computation of weekly values, removing any missing value in 'x'

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three weeks (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Agreggating from Daily to Weekly, removing any missing value in 'x'
m <- daily2weekly(x, FUN=sum, na.rm=TRUE)

######################
## Ex2: Computation of Weekly values only when the percentage of NAs in each
#       month is lower than a user-defined percentage (10% in this example).

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three weeks (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Daily to Weekly, only for weeks with less than 10% of missing values
m2 <- daily2weekly(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1)

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="month")

######################
## Ex3: Loading the HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to weekly ts
subdaily2weekly(x, FUN=mean, na.rm=TRUE)
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
