%%File subdaily2daily.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2008-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{subdaily2daily}
\Rdversion{1.1}
\alias{subdaily2daily}
\alias{subdaily2daily.default}
\alias{subdaily2daily.zoo}
\alias{subdaily2daily.data.frame}
\alias{subdaily2daily.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sub-daily -> Daily
}
\description{
Generic function for transforming a Sub-DAILY time series into a DAILY one
}
\usage{
subdaily2daily(x, ...)

\method{subdaily2daily}{default}(x, FUN, na.rm = TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{subdaily2daily}{zoo}(x, FUN, na.rm = TRUE, na.rm.max=0,
                       start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{subdaily2daily}{data.frame}(x, FUN, na.rm = TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz,
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)

\method{subdaily2daily}{matrix}(x, FUN, na.rm = TRUE, na.rm.max=0,
           start="00:00:00", start.fmt= "\%H:\%M:\%S", tz,
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with sub-daily time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represents the time series measured in each gauging station, and the column names of \code{x} should correspond to the ID of each station (starting by a letter).
}
 \item{FUN}{
Function that have to be applied for aggregating from sub-daily into daily time step (e.g., for precipitation \code{FUN=sum} and for temperature and streamflows ts \code{FUN=mean}). \cr

\code{FUN} MUST accept the \code{na.rm} argument, because \code{na.rm} is passed to \code{FUN}.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the daily values are computed only for days with a percentage of missing values less than \code{na.rm.max} \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a day, the corresponing daily value in the output object will be \code{NA}.
}
  \item{na.rm.max}{
Numeric in [0, 1]. It is used to define the maximum percentage of missing values allowed in each day to keep the daily aggregated value in the output object of this function. In other words, if the percentage of missing values in a given day is larger or equal than \code{na.rm.max} the corresponding daily value will be \code{NA}.
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
arguments additional to \code{na.rm} passed to \code{FUN}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  a zoo object with daily time series
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
\code{\link{subhourly2hourly}}, \code{\link{subdaily2monthly}}, 
\code{\link{subdaily2annual}}, \code{\link{subdaily2seasonal}}, 
\code{\link{as.POSIXct}}, \code{\link{dm2seasonal}}, \code{\link{monthlyfunction}}, 
\code{\link{seasonalfunction}}, \code{\link{hydroplot}}, 
\code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}
}
\examples{
## Ex1: Computation of daily values, removing any missing value in 'x'

## Loading the time series of hourly streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Plotting the hourly streamflow values
plot(x)

# Subsetting 'x' to its first three days (01/Jan/1980 - 03/Jan/1980)
x <- window(x, end="1980-01-03 23:59:00")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Agreggating from Sub-Daily to Daily, removing any missing value in 'x'
( d1 <- subdaily2daily(x, FUN=mean, na.rm=TRUE) )


## Ex2: Computation of daily values, removing any missing value in 'x' and
##      considering that the new day starts at 08:00:00 local time
( d2 <- subdaily2daily(x, FUN=mean, na.rm=TRUE, start="08:00:00") )

## Ex3: Computation of daily values, removing any missing value in 'x' and
##      considering that the new day starts at 08:00:00, and forcing 
#       UTC both for 'x' and 'start'
( d3 <- subdaily2daily(x, FUN=mean, na.rm=TRUE, start="08:00:00", tz="UTC") )

######################
## Ex4: Compuation of daily values only when the percentage of NAs in each
#       day is lower than a user-defined percentage (10% in this example).
( d4 <- subdaily2daily(x, FUN=mean, na.rm=TRUE, na.rm.max=0.1) )
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
