%%File sname2ts.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{sname2ts}
\Rdversion{1.1}
\alias{sname2ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Station Name -> Time Series
}
\description{
This function takes a data.frame whose columns contains the time series of several gauging stations, along with a character representing the name of one gauging station, and extracts the time series corresponding to that station.
}
\usage{
sname2ts(x, sname, dates, date.fmt = "\%Y-\%m-\%d", var.type, 
         tstep.out = "daily", FUN, na.rm = TRUE, from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data.frame containing the complete times series of all the stations. \cr
It may also contain 1 column with the dates of the measurements, or they can be provided in a different way (see \code{dates} below).
}
  \item{sname}{
Character representing the name of a station, which have to correspond to one column name in \code{x}
}
  \item{dates}{
numeric, factor, Date object indicating how to obtain the dates corresponding to the \code{sname} station. \cr
-) If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
-) If \code{dates} is a factor, it is converted into Date class, using the date format specified by \code{date.fmt} \cr
-) If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of element in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{var.type}{
character representing the type of variable being plotted. Used for determining the function used for computing the monthly or/and annual values when \code{FUN} is missing. Valid values are: \cr
-) \kbd{Precipitation} => \code{FUN=sum}  \cr
-) \kbd{Temperature}   => \code{FUN= mean} \cr
-) \kbd{Flow}          => \code{FUN= mean}
}
  \item{tstep.out}{
character that defines the time step of the desired output time series. Valid values are: \cr
-) \kbd{daily}  : daily time series   \cr
-) \kbd{monthly}: monthly time series \cr
-) \kbd{annual} : annual time series 
}
  \item{FUN}{
ONLY required when \code{var.type} is missing and \code{tstep} is one of \code{monthly} or \code{annual}. \cr
Function that have to be applied for transforming from daily to monthly or annual time step (e.g., for precipitation \code{FUN=sum} and for temperature and flow ts, \code{FUN=mean}) 
}
  \item{na.rm}{
a logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{from}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the starting date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
  \item{to}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the ending date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
zoo object
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sname2plot}}
}
\examples{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Annual values of temperature at the station "T9105", stored in 'EbroPPtsMonthly'.
sname2ts(EbroPPtsMonthly, sname="P9001", dates=1, FUN=sum, tstep.out="annual")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
