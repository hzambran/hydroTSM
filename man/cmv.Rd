%%File cmv.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{cmv}
\Rdversion{1.1}
\alias{cmv}
\alias{cmv.default}
\alias{cmv.zoo}
\alias{cmv.data.frame}
\alias{cmv.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Counting Missing Values
}
\description{
Generic function for counting the percentage/amount of missing values in a zoo object, using a user-defined temporal scale.
}
\usage{
cmv(x, ...)

\method{cmv}{default}(x, tscale=c("hourly", "daily", "weekly", 
                      "monthly", "quarterly", "seasonal", "annual"),
                      out.type=c("percentage", "amount"), dec=3, 
                      start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{cmv}{zoo}(x, tscale=c("hourly", "daily", "weekly", 
                  "monthly", "quarterly", "seasonal", "annual"),
                  out.type=c("percentage", "amount"), dec=3, 
                  start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, ...)

\method{cmv}{data.frame}(x, tscale=c("hourly", "daily", "weekly", 
                         "monthly", "quarterly", "seasonal", "annual"),
                         out.type=c("percentage", "amount"), dec=3, 
                         start="00:00:00", start.fmt= "\%H:\%M:\%S", tz, 
                         dates=1, date.fmt="\%Y-\%m-\%d", ...)

\method{cmv}{matrix}(x, tscale=c("hourly", "daily", "weekly", 
                     "monthly", "quarterly", "seasonal", "annual"),
                     out.type=c("percentage", "amount"), dec=3, 
                     start="00:00:00", start.fmt= "\%H:\%M:\%S", tz,
                     dates=1, date.fmt="\%Y-\%m-\%d", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with the time series to be analised. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represents the time series measured in a gauging statio, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{tscale}{
character with the temporal scale to be used for analysing the mssing data. Valid values are: \cr

If \kbd{hourly}: the percentage/amount of missing values will be given for each hour and ,therefore, the expected time frequency of \code{x} must be sub-hourly. \cr
If \kbd{daily}: the percentage/amount of missing values will be given for each day and, therefore, the expected time frequency of \code{x}  must be sub-daily (i.e., hourly or sub-hourly). \cr
If \kbd{weekly}: the percentage/amount of missing values will be given for each week (starting on Monday) and, therefore, the expected time frequency of \code{x} must be sub-weekly (i.e., daily, (sub)hourly). \cr
If \kbd{monthly}: the percentage/amount of missing values will be given for each month and, therefore, the expected time frequency of \code{x} must be sub-monthly (i.e., daily, hourly or sub-hourly). \cr
If \kbd{quarterly}: the percentage/amount of missing values will be given for each quarter and, therefore, the expected time frequency of \code{x} must be sub-quarterly (i.e., monthly, daily, hourly or sub-hourly). \cr
If \kbd{seasonal}: the percentage/amount of missing values will be given for each weather season (see ?time2season) and, therefore, the expected time frequency of \code{x} must be sub-seasonal (i.e., monthly, daily, hourly or sub-hourly). \cr
If \kbd{annual}: the percentage/amount of missing values will be given for each year and, therefore, the expected time frequency of \code{x} must be sub-annual (i.e., seasonal, monthly, daily, hourly or sub-hourly). \cr
}

\item{dec}{
integer indicating the amount of decimal places included in the output. \cr
It is only used when \code{out.type=='percentage'}.
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
numeric, factor, POSIXct or POSIXt object indicating how to obtain the dates and times for each column of \code{x} (e.g., gauging station). \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the date and times. \cr
If \code{dates} is a factor, it is converted into POSIXct class, using the date format specified by \code{date.fmt}  \cr
If \code{dates} is already of POSIXct or POSIXt class, this function verifies that the number of elements on it be equal to the number of elements in \code{x}.
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{dates}, By default \code{date.fmt=\%Y-\%m-\%d \%H:\%M:\%S}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
character indicating how should be returned the missing values for each temporal scale. Valid values are: \cr
-) \kbd{percentage}: the missing values are returned as an real value, representing the percentage of missing values in each temporal scale. \cr
-) \kbd{amount}: the missing values are returned as an integer value, representing the absolute amount of missing values in each temporal scale.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
The amount of missing values in each temporal scale is computed just by counting the amount of NAs in each hour / day / week / month / quarter / season / year, while the amount of missing values in each temporal scale is computed by dividing the revious number by the total number of data elements in each hour / day / week / month / quarter / season / year. \cr

This function was developed to allow the selective removal of values when agregting from a high temporal resolution into a lower temporal resolution (e.g., from hourly to daily or from hourly to monthly), using any of the temporal aggregation functions available int his package (e.g., \code{hourly2daily}, \code{daily2monthly})
}
\value{
a zoo object with the percentage/amount of missing values for each temporal scale selected by the user.
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
\code{\link{dwi}}, \code{\link{subhourly2hourly}}, \code{\link{subdaily2daily}}, \code{\link{daily2monthly}}, \code{\link{daily2annual}}, \code{\link{monthlyfunction}}, \code{\link{izoo2rzoo}}
}
\examples{
######################
## Ex1: Loading the DAILY precipitation data at SanMartino (25567 daily values)
data(SanMartinoPPts)
x <- SanMartinoPPts

## Transforming into NA the 10% of values in 'x'
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

# Getting the amount of NAs in 'x' for each week (starting on Monday)
cmv(x, tscale="weekly")

# Getting the amount of NAs in 'x' for each month
cmv(x, tscale="monthly")

# Getting the amount of NAs in 'x' for each quarter
cmv(x, tscale="quarterly")

# Getting the amount of NAs in 'x' for each weather season
cmv(x, tscale="seasonal")

# Getting the amount of NAs in 'x' for each year
cmv(x, tscale="annual")
######################
## Ex2: Loading the time series of HOURLY streamflows for the station 
## Karamea at Gorge (52579 hourly values)
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Transforming into NA the 30% of values in 'x'
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

# Getting the amount of NAs in 'x' for each day
cmv(x, tscale="daily")

# Getting the amount of NAs in 'x' for each weather season
cmv(x, tscale="seasonal")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
