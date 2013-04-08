%%File daily2monthly.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{(sub)daily2monthly}
\Rdversion{1.1}
\alias{daily2monthly}
\alias{daily2monthly.default}
\alias{daily2monthly.zoo}
\alias{daily2monthly.data.frame}
\alias{daily2monthly.matrix}
\alias{subdaily2monthly}
%\alias{subdaily2monthly.default}
%\alias{subdaily2monthly.zoo}
%\alias{subdaily2monthly.data.frame}
%\alias{subdaily2monthly.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(Sub)Daily -> Monthly
}
\description{
Generic function for transforming a DAILY (sub-daily or weekly) regular time series into a MONTHLY one
}
\usage{
daily2monthly(x, ...)
subdaily2monthly(x, ...)

\method{daily2monthly}{default}(x, FUN, na.rm = TRUE, ...)

\method{daily2monthly}{zoo}(x, FUN, na.rm = TRUE, ...)

\method{daily2monthly}{data.frame}(x, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
        out.type = "data.frame", out.fmt="numeric", verbose = TRUE, ...)

\method{daily2monthly}{matrix}(x, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
        out.type = "data.frame", out.fmt="numeric", verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts, data.frame or matrix object, with daily/monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that have to be applied for transforming from daily to annual time step. (e.g., for precipitation \code{FUN=sum} and for temperature and streamflow ts, \code{FUN=mean}). \cr
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly and annual values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a year, the monthly values are NA 
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates for each gauging station \cr
If \code{dates} is a number, it indicates the index of the column in \var{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format specified by \code{date.fmt}  \cr
If \code{dates} is already of Date class, the code verifies that the number of days on it be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character that defines the desired type of output. Valid values are:  \cr
-) \kbd{data.frame}: a data.frame, with as many columns as stations are included in \code{x}, and row names indicating the month and year for each value. \cr
-) \kbd{db}        : a data.frame, with 4 columns will be produced. \cr
                 The first column (StationID) stores the ID of the station, \cr
                 The second column (Year) stores the year \cr
                 The third column (Month) stores the Month  \cr
                 The fourth column (Value) stores the numerical values corresponding to the values specified in the three previous columns. \cr
}
  \item{out.fmt}{
OPTIONAL. Only used when \code{x} is a matrix or data.frame object /cr
character, for selecting if the result will be a matrix/data.frame or a zoo object. Valid values are: \kbd{numeric}, \kbd{zoo}.
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
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{daily2annual}}, \code{\link{subdaily2daily}}, \code{\link{monthlyfunction}}, \code{\link{hydroplot}}, \code{\link{vector2zoo}}, \code{\link{izoo2rzoo}}, \code{\link{as.Date}}
}
\examples{
######################
## Ex1: Loading the SanMartino DAILY precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

## Daily to monthly
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

######################
## Ex2: Loading the time series of HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to monthly ts
subdaily2monthly(x, FUN=mean, na.rm=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
