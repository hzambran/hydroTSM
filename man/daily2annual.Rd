%% File daily2annual.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%% Copyright 2008-2025 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{(sub)daily2annual}
\Rdversion{1.1}
\alias{daily2annual}
\alias{daily2annual.default}
\alias{daily2annual.zoo}
\alias{daily2annual.data.frame}
\alias{daily2annual.matrix}
\alias{monthly2annual}
\alias{monthly2annual.default}
\alias{monthly2annual.zoo}
\alias{monthly2annual.data.frame}
\alias{monthly2annual.matrix}
\alias{subdaily2annual}
\alias{subdaily2annual.default}
\alias{subdaily2annual.zoo}
\alias{subdaily2annual.data.frame}
\alias{subdaily2annual.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(sub)Daily/Monthly -> Annual 
}
\description{
Generic function for transforming a (sub)DAILY/MONTHLY (weekly and quarterly) regular time series into an ANNUAL one.
}
\usage{
daily2annual(x, ...)
subdaily2annual(x, ...)
monthly2annual(x, ...)

\method{daily2annual}{default}(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("\%Y", "\%Y-\%m-\%d"), start.month=1, ...)

\method{daily2annual}{zoo}(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("\%Y", "\%Y-\%m-\%d"), start.month=1, ...) 

\method{daily2annual}{data.frame}(x, FUN, na.rm=TRUE, na.rm.max=0, 
        out.fmt=c("\%Y", "\%Y-\%m-\%d"), start.month=1, dates=1, 
        date.fmt = "\%Y-\%m-\%d", out.type = "data.frame", verbose = TRUE, ...)

\method{daily2annual}{matrix}(x, FUN, na.rm = TRUE, na.rm.max=0, 
        out.fmt=c("\%Y", "\%Y-\%m-\%d"), start.month=1, 
        dates=1, date.fmt = "\%Y-\%m-\%d", out.type = "data.frame", 
        verbose = TRUE,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with (sub)daily/monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame or matrix object, and in that case, each column of \code{x} represents the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that have to be applied for aggregating from (sub)daily/monthly into annual time step (e.g., for precipitation \code{FUN=sum} and for temperature and streamflows ts \code{FUN=mean}). \cr

\code{FUN} MUST accept the \code{na.rm} argument, because \code{na.rm} is passed to \code{FUN}. \cr

When \code{FUN=max} or \code{FUN=min} the date(time) where the maximum/minimum value actually occurs is returned in the output object, otherwise, a generic 1st of january for each year is returned.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the annual values are computed only for years with a percentage of missing values less than \code{na.rm.max} \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a year, the corresponing annual values in the output object will be \code{NA}.
}
  \item{na.rm.max}{
Numeric in [0, 1]. It is used to define the maximum percentage of missing values allowed in each year to keep the yearly aggregated value in the output object of this function. In other words, if the percentage of missing values in a given year is larger than \code{na.rm.max} the corresponding annual value will be \code{NA}.
}
  \item{out.fmt}{
Character indicating the date format for the output zoo object. See \code{format} in \code{\link[base]{as.Date}}. Possible values are: \cr
-) \kbd{\%Y}        : only the year will be used for the time. Default option. (e.g., "1961" "1962"...) \cr
-) \kbd{\%Y-\%m-\%d}: a complete date format will be used for the time. (e.g., "1961-01-01" "1962-01-01"...). See Details.
}
  \item{start.month}{
 numeric in [1,..,12], representing the starting month (1:Jan, ..., 12:Dec) to be used in the computation of annual values. By default \code{start.month=1}.
 }
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates for corresponding to each gauging station \cr
If \code{dates} is a number (default), it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days on it be equal to the number of element in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character that defines the desired type of output. Valid values are: \cr
-) \kbd{data.frame}: a data.frame, with as many columns as stations are included in \code{x}, and row names indicating the Year \cr
-) \kbd{db}        : a data.frame, with 3 columns will be produced. \cr
                     The first column (StationID) will store the ID of the station \cr
                     The second column (Year) will store the year, \cr
                     The third column (Value) will contain the annual value corresponding to the two previous columns.
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
When \code{FUN!=max} and \code{FUN!=min} the output is a zoo object with annual time frequency, where the time attribute has the format defined in \code{out.fmt}. \cr
When \code{FUN!=max} and \code{FUN!=min} and \code{out.fmt="\%Y-\%m-\%d"} the time attribute of the output zoo object will use the 1st of January of each year to create a full Date object from the corresponding year of each element of the output object (e.g., fi the year is \kbd{2022}, the time attribute will be \kbd{2022-01-01}). The only exception occurrs when \code{FUN=max} or \code{FUN=min}, where the time attribute of each element will correspond to the actual date where the annual maximum/minimum occurs (which is very useful for identifying the date of the annual maximum or the annual minimum of a time series). \cr

When \code{FUN=max} or \code{FUN=min} and \code{x} is a single time series, the output is a zoo object with annual time frequency, where the time attribute has the same class than \code{time(x)}, and the date(time) value corresponds to the date(time) where the maximum/minimum value actually occurs. \cr
When \code{FUN=max} or \code{FUN=min} and \code{x} has two or more time series, the output is a list object where each element has an annual time frequency. The time attribute of each list element has the same class than \code{time(x)}, and the date(time) value of each list element corresponds to the date(time) where the maximum/minimum value actually occurs. \cr
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
\code{\link{subhourly2hourly}}, \code{\link{daily2monthly}}, \code{\link{monthly2annual}}, \code{\link{hydroplot}}, \code{\link{annualfunction}}, \code{\link{vector2zoo}}, \code{\link{as.Date}}
}
\examples{
######################
## Ex1: Computation of annual values, removing any missing value in 'x'

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three months (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Agreggating from Daily to Annual, removing any missing value in 'x'
( a <- daily2annual(x, FUN=sum, na.rm=TRUE) )

######################
## Ex2: Compuation of annual values only when the percentage of NAs in each
#       year is lower than a user-defined percentage (10% in this example).

# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three months (Jan/1921 - Mar/1921)
x <- window(x, end="1921-03-31")

## Transforming into NA the 10% of values in 'x'
set.seed(10) # for reproducible results
n           <- length(x)
n.nas       <- round(0.1*n, 0)
na.index    <- sample(1:n, n.nas)
x[na.index] <- NA

## Daily to annual, only for months with less than 10% of missing values
( a2 <- daily2annual(x, FUN=sum, na.rm=TRUE, na.rm.max=0.1) )

# Verifying that the second and third month of 'x' had 10% or more of missing values
cmv(x, tscale="annual")


######################
## Ex3: Getting the annual maximum value, including the date where this annual 
##      maximum actually occurs
daily2annual(x, FUN=max)


######################
## Ex4: Monthly to Annual (same result as )
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)
monthly2annual(m, FUN=sum, na.rm=TRUE)


######################
## Ex5: Loading the time series of HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Sub-daily to monthly ts
subdaily2annual(x, FUN=mean, na.rm=TRUE)

############
## Ex6: Loading the monthly time series of precipitation within the Ebro River basin
data(EbroPPtsMonthly)

# computing the annual values for the first 10 gauging stations in 'EbroPPtsMonthly'
a <- monthly2annual(EbroPPtsMonthly[,1:11], FUN=sum, dates=1)

# same as before, but with a nicer format of years
a <- monthly2annual(EbroPPtsMonthly[,1:11], FUN=sum, dates=1, out.fmt="\%Y")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
