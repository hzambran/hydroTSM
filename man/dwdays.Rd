%% File dwdays.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{dwdays}
\Rdversion{1.1}
\alias{dwdays.data.frame}
\alias{dwdays.matrix}
\alias{dwdays.default}
\alias{dwdays}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Amount of dry/wet days in a time series
}
\description{
Given a daily time series (usually precipitation), this function computes the average amount of wet/dry days in each month.
}
\usage{
dwdays(x, ...)

\method{dwdays}{default}(x, thr=0, type="wet", na.rm=TRUE, ... )

\method{dwdays}{data.frame}(x, thr=0, type="wet", na.rm=TRUE, 
        dates, date.fmt="\%Y-\%m-\%d", verbose=TRUE,...)

\method{dwdays}{matrix}(x, thr=0, type="wet", na.rm=TRUE, 
        dates, date.fmt="\%Y-\%m-\%d", verbose=TRUE,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, usually with daily time series of precipitation. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{thr}{
numeric. Value of daily precipitation used as threshold for classifying a day as dry/wet or not. Days with a precipitation value larger to \code{thr} are classified as \var{wet days}, whereas precipitation values lower to \code{thr} are classified as \var{dry days}. 
}
  \item{type}{
character, indicating if the daily values have to be classified as dry or wet days. It works linked to the values specified in \code{thr}. Valid values are: \kbd{wet}, \kbd{dry}.
}
  \item{na.rm}{
Logical. Should missing values be removed before counting?
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format  specified by \code{date.fmt}  \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of element in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
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

%%\seealso{
%%\code{\link{vector2zoo}}, \code{\link{as.Date}}, \code{\link{daily2annual}}
%%}

\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

## Average amount of wet days in each month (for this example, this means days 
## with precipitation larger than 0.1mm) 
dwdays(x, thr=0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
