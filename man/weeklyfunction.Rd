%%File weeklyfunction.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{weeklyfunction}
\Rdversion{1.1}
\alias{weeklyfunction}
\alias{weeklyfunction.zoo}
\alias{weeklyfunction.default}
\alias{weeklyfunction.matrix}
\alias{weeklyfunction.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weekly Function
}
\description{
Generic function for obtaining 52 weekly values of a zoo object, by applying any R function to ALL the values in the object belonging to each one of the 52 calendar weeks (starting on Monday).
}
\usage{
weeklyfunction(x, ...)

\method{weeklyfunction}{default}(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                                 start.fmt= "\%H:\%M:\%S", tz, ...)

\method{weeklyfunction}{zoo}(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                             start.fmt= "\%H:\%M:\%S", tz, ...)

\method{weeklyfunction}{data.frame}(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                        start.fmt= "\%H:\%M:\%S", tz, dates=1, date.fmt="\%Y-\%m-\%d",
                        out.type="data.frame", verbose=TRUE,...)
             
\method{weeklyfunction}{matrix}(x, FUN, na.rm=TRUE, na.rm.max=0, start="00:00:00", 
                        start.fmt= "\%H:\%M:\%S", tz, dates=1, date.fmt="\%Y-\%m-\%d",
                        out.type="data.frame", verbose=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts, data.frame or matrix object, with daily or monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that will be applied to ALL the values in \code{x} belonging to each one of the 12 months of the year (e.g., FUN can be some of \code{mean}, \code{sum}, \code{max}, \code{min}, \code{sd}).
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly values and FUN are computed considering only those values in \code{x} different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the corresponding monthly value will be NA 
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
It is only used when \code{x} is not a zoo object. \cr
numeric, factor, Date indicating how to obtain the dates. \cr
If \code{dates} is a number (default), it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into 'Date' class, using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
It is only used when \code{x} is not a zoo object. \cr
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
It is only used when \code{x} is a matrix or data.frame. \cr
Character defining the desired type of output. Valid values are: \cr
-) \kbd{data.frame}: a data.frame, with 12 columns representing the months, and as many rows as gauging stations are included in \code{x} \cr
-) \kbd{db}        : a data.frame, with 4 columns will be produced. Useful for a posterior boxplot \cr
                 The first column ('StationID') will store the ID of the station,  \cr
                 The second column ('Year') will store the year, \cr
                 The third column ('Month') will store month,  \cr
                 The fourth column ('Value') will contain the monthly value corresponding to the three previous columns.
}
  \item{verbose}{
Logical; if TRUE, progress messages are printed 
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
When \code{x} is a zoo object, a numeric vector with 12 elements representing the computed monthly value for each month. \cr
When \code{x} is a data.frame which columns represent measurements at different gauging stations, the resulting object is a data.frame with 12 columns and as many rows as gauging stations are in \code{x}, each row storing the computed 12 monthly value for each gauging station.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
\note{
Due to the fact that \code{FUN} is applied over all the elements in \code{x} belonging to a given calendar month, its result will depend on the sampling frequency of \code{x} and the type of function provided by \code{FUN} (\bold{special attention have to be put when \code{FUN=sum}})
}

\seealso{
\code{\link{annualfunction}}, \code{\link{seasonalfunction}}, \code{\link{dm2seasonal}}, \code{\link{daily2monthly}}, \code{\link{daily2annual}}
}
\examples{
## Ex1: Computation of mean WEEKLY values from DAILY ts, removing any missing value in 'x'

# Loading DAILY streamflows (3 years) at the station 
# Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Mean WEEKLY streamflows at station 'x'
weeklyfunction(x, FUN=mean, na.rm=TRUE)

######################
## Ex2: Computation of mean WEEKLY values from HOURLY ts, removing any missing value in 'x'

# Loading HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Mean WEEKLY streamflows at station 'x'. Each day starts at 00:00:00
weeklyfunction(x, FUN=mean, na.rm=TRUE)

######################
## Ex3: Computation of mean WEEKLY values from HOURLY ts, removing any missing value in 'x'
##      and starting each day at 08:00:00

# Loading HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

## Mean WEEKLY streamflows at station 'x'. Each day starts at 00:00:00
weeklyfunction(x, FUN=mean, na.rm=TRUE, start="00:00:00")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
