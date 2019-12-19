%%File subdaily2daily.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
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

\method{subdaily2daily}{default}(x, FUN, na.rm = TRUE, start="00:00:00", ...)

\method{subdaily2daily}{zoo}(x, FUN, na.rm = TRUE, start="00:00:00", ...)

\method{subdaily2daily}{data.frame}(x, FUN, na.rm = TRUE, start="00:00:00",
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)

\method{subdaily2daily}{matrix}(x, FUN, na.rm = TRUE, start="00:00:00",
           dates=1, date.fmt="\%Y-\%m-\%d \%H:\%M:\%S", out.fmt="zoo", 
           verbose= TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
xts, data.frame or matrix object, with sub-daily time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} should correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that have to be applied for transforming from sub-daily to daily time step. (e.g., for precipitation \code{FUN=sum} and for temperature and streamflow ts, \code{FUN=mean}). \cr
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the daily values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA sub-daily value within a day, the corresponing daily value(s) will be NA as well
}
  \item{start}{
character, indicating the starting time used for aggregating subdaily time series into daily ones. It MUST be provided in the \code{"\%H:\%M:\%S"} format.
}
  \item{dates}{
numeric, factor, POSIXct or POSIXt object indicating how to obtain the dates and times for each gauging station \cr
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
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
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
\code{\link{subdaily2monthly}}, \code{\link{subdaily2annual}}, \code{\link{subdaily2seasonal}}, \code{\link{as.POSIXct}}, \code{\link{dm2seasonal}}, \code{\link{monthlyfunction}}, \code{\link{seasonalfunction}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}
}
\examples{
## Loading the time series of hourly streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Plotting the hourly streamflow values
plot(x)

## sub-daily to Daily
d <- subdaily2daily(x, FUN=sum, na.rm=TRUE)

# Plotting the daily streamflow values
plot(d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
