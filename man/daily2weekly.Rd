%%File daily2weekly.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{(sub)daily2weekly}
\Rdversion{1.1}
\alias{daily2weekly}
\alias{daily2weekly.default}
\alias{daily2weekly.zoo}
\alias{daily2weekly.data.frame}
\alias{daily2weekly.matrix}
\alias{subdaily2weekly}
\alias{subdaily2weekly.default}
\alias{subdaily2weekly.zoo}
\alias{subdaily2weekly.data.frame}
\alias{subdaily2weekly.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(sub)Daily -> Weekly
}
\description{
Generic function for transforming a DAILY (or sub-daily) regular time series into a WEEKLY one
}
\usage{
daily2weekly(x, ...)
subdaily2weekly(x, ...)

\method{daily2weekly}{default}(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

\method{daily2weekly}{zoo}(x, FUN, na.rm=TRUE, na.rm.max=0, ...)

\method{daily2weekly}{data.frame}(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
        date.fmt = "\%Y-\%m-\%d", out.type = "data.frame", out.fmt="numeric", 
        verbose=TRUE, ...)

\method{daily2weekly}{matrix}(x, FUN, na.rm=TRUE, na.rm.max=0, dates=1, 
        date.fmt = "\%Y-\%m-\%d", out.type = "data.frame", out.fmt="numeric", 
        verbose=TRUE, ...)
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
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates for each gauging station \cr
If \code{dates} is a number (default), it indicates the index of the column in \var{x} that stores the dates \cr
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
