%% File hip.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hip}
\Rdversion{1.1}
\alias{hip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hours in Period
}
\description{
Given any starting and ending date/time objects, it generates:  \cr
1) a vector of class c("POSIXct" "POSIXt") with all the hours between the two date/time objects (both of them included), OR \cr
2) the amount of hours between the two date/time objects 
}
\usage{
hip(from, to, date.fmt="\%Y-\%m-\%d \%H", out.type = "seq", tz="UTC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{
Character or POSIXct object indicating the starting date/time for creating the sequence. It has to be in the format indicated by \code{date.fmt}.
}
  \item{to}{
Character indicating the ending date/time for creating the sequence. It has to be in the format indicated by \code{date.fmt}.
}
  \item{date.fmt}{
character indicating the format in which the date/time objects are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d \%H:\%M}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
1) \kbd{seq} : a vector of class Date with all the days between the two dates, OR \cr
2) \kbd{nmbr}: a single numeric value with the amount of days between the two dates.
}
  \item{tz}{
specification of the desired time zone yo be used. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} (the default value) is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
This argument can be used when working with subdaily zoo objects to force using the local time zone instead of GMT as time zone.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Depending on the value of \code{out.type}, it returns: \cr
1) a vector of class \code{c("POSIXct" "POSIXt")} with all the hours between \code{from} and \code{to} (both of them included), OR \cr
2) the amount of hours between the two date/time objects
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
\code{\link{dip}}, \code{\link{mip}}, \code{\link{yip}}, \code{\link{diy}}, \code{\link[xts]{timeBasedSeq}}
}
\examples{
## Sequence of hours between "1961-01-01 00:00" and "1961-01-10 00:00", giving the
## starting and ending date/time objects with hours and skipping the minutes (default)
hip("1961-01-01 00", "1961-12-31 00")

## Sequence of hours between "1961-01-01 00:00" and "1961-01-10 00:00", giving the
## starting and ending date/time objects only with hours and minutes(skipping the minutes)
hip("1961-01-01 00:00", "1961-12-31 00:00", date.fmt="\%Y-\%m-\%d \%H:\%M")

## Number of hours between the 10:00 AM of "1961-Jan-02" and the 11:00 AM of "1961-Jan-01", 
## using "\%d/\%m/\%Y" as date/time format.
hip("01/01/1961 10", "02/01/1961 11", date.fmt= "\%d/\%m/\%Y \%H", out.type = "nmbr")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
