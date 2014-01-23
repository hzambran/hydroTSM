%% File dwi.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{dwi}
\Rdversion{1.1}
\alias{dwi}
\alias{dwi.default}
\alias{dwi.zoo}
\alias{dwi.data.frame}
\alias{dwi.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Days with Information
}
\description{
This function generates a table indicating the number of days with information (<>NA) within a zoo object, aggregated by year, month or month per year.
}
\usage{
dwi(x, ...)

\method{dwi}{default}(x, out.unit = "years", from = start(x), to = end(x), 
     date.fmt = "\%Y-\%m-\%d", tstep="days", ...)
     
\method{dwi}{zoo}(x, out.unit = "years", from = start(x), to = end(x), 
     date.fmt = "\%Y-\%m-\%d", tstep="days", ...)

\method{dwi}{data.frame}(x, out.unit = "years", from, to, date.fmt = "\%Y-\%m-\%d", tstep="days", 
     dates = 1, verbose = TRUE, ...)
     
\method{dwi}{matrix}(x, out.unit = "years", from, to, date.fmt = "\%Y-\%m-\%d", tstep="days", 
     dates = 1, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with daily/monthly/annual time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{out.unit}{
aggregation time for the computation of the amount of days with information. Valid values are: \cr
-) \kbd{months}: monthly;  \cr
-) \kbd{years} : annual;   \cr
-) \kbd{mpy}   : month per year (not available for data.frames)
}
  \item{from}{
Character indicating the starting date for the computations. It has to be in the format indicated by \code{date.fmt}.\cr
When \code{x} is a data.frame and this value is not provided, the date corresponding to the first row of \code{x} is used
}
  \item{to}{
Character indicating the ending date for the computations. It has to be in the format indicated by \code{date.fmt}. \cr
When \code{x} is a data.frame and this value is not provided, the date corresponding to the last row of \code{x} is used
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}. 
}
  \item{tstep}{
Time step used for storing the values in \code{x}. Valid values are: \kbd{days}, \kbd{months}, \kbd{years}.
Since the version 0.3-0 of hydroTSM, this argument is not required any more, because it is not used any longer.
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates for each column of \code{x}\cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format  specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of element in \code{x}
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
\code{\link{matrixplot}}
}
\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

## Days with information per year
dwi(x)

## Days with information per month per year.
dwi(x, out.unit="mpy")

###########
\dontrun{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Months with information per year in the 9 first stations of 'EbroPPtsMonthly'
a <- dwi(EbroPPtsMonthly[,1:10], out.unit="years", dates=1)

## Before plotting the results in 'a', and just for obtaining a more interesting
## plot, 70 random numbers (between 1 and 11) are introduced in 'a'
a[sample(length(a), size = 70)] <- rep(1:11, length=70)

## Plotting the amount of months with information per year in each station
matrixplot(a, var.type="Days", main="Number of months with info per year")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
