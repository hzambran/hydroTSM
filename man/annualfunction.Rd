%% File annualfunction.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{annualfunction}
\Rdversion{1.1}
\alias{annualfunction}
\alias{annualfunction.default}
\alias{annualfunction.zoo}
\alias{annualfunction.data.frame}
\alias{annualfunction.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annual Function
}
\description{
Generic function for obtaining a SINGLE annual value of a zoo object, by applying any R function to ALL the values in \code{x} belonging to the same year, and then applying the same function to ALL the previously computed annual values (e.g., for computing the average annual precipitation or the mean annual streamflow of a long-term time series).
}
\usage{
annualfunction(x, FUN, na.rm = TRUE, ...)

\method{annualfunction}{default}(x, FUN, na.rm = TRUE, ...)

\method{annualfunction}{zoo}(x, FUN, na.rm = TRUE, ...)

\method{annualfunction}{data.frame}(x, FUN, na.rm = TRUE, dates=1, date.fmt = "\%Y-\%m-\%d", 
        verbose = TRUE, ...)
        
\method{annualfunction}{matrix}(x, FUN, na.rm = TRUE, dates=1, date.fmt = "\%Y-\%m-\%d", 
        verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts, data.frame or matrix object, with daily/monthly/annual time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that will be used to compute the final annual value (e.g., \code{FUN} may be some of \code{mean}, \code{sum}, \code{max}, \code{min}, \code{sd})  .
}
  \item{na.rm}{
Logical. Should missing values be removed?. \cr
-) \kbd{TRUE} : the annual values are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a year, the resulting annual value will be NA 
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates corresponding to each gauging station. \cr
If \code{dates} is a number (default), it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it have to be converted into Date class, using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{verbose}{
Logical; if TRUE, progress messages are printed. 
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
When \code{x} is a time series, a single annual value is returned. \cr
For a data frame, a named vector with the appropriate method being applied column by column.
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
\code{FUN} is first applied to all the values of \code{x} belonging to the same year and then it is applied to all the previously computed annual values to get the final result.
Its result will depend on the sampling frequency of \code{x} and the type of function provided by \code{FUN} (\bold{special attention have to be put when \code{FUN=sum}})
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{monthlyfunction}}, \code{\link{daily2annual}}, \code{\link{monthly2annual}}, \code{\link{yip}}
}
\examples{
## Loading the SanMartino daily precipitation data (1921-1990)
data(SanMartinoPPts)
x <- SanMartinoPPts

# Amount of years in 'x' (needed for computing the average)
nyears <- length( seq(from=time(x[1]), to=time(x[length(x)]), by="years") )

## Average annual precipitation for the 70 years period. 
# It is necessary to divide by the amount of years to obtain the average annual value, 
# otherwise it will give the total precipitation for all the 70 years.
annualfunction(x, FUN=sum, na.rm=TRUE) / nyears


#####################
### verification ####
# Daily to annual
a <- daily2annual(x, FUN=sum, na.rm=TRUE)

# Mean annual value
mean(a)

##############################
##############################
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Dates of 'x'
dates <- as.Date(x[,1])


## Computation of the average annual precipitation
\dontrun{

## Transforming 'x' into a zoo object
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

## Average annual precipitation, for the first 5 stations in 'x'
annualfunction(z[ ,1:5], FUN=sum)/nyears
 
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
