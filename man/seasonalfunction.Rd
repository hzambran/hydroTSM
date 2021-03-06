%% File seasonalfunction.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{seasonalfunction}
\Rdversion{1.1}
\alias{seasonalfunction}
\alias{seasonalfunction.default}
\alias{seasonalfunction.zoo}
\alias{seasonalfunction.data.frame}
\alias{seasonalfunction.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonal Function
}
\description{
Generic function for applying any R function to a zoo object, in order to obtain 4 representative seasonal values.
}
\usage{
seasonalfunction(x, ...)

\method{seasonalfunction}{default}(x, FUN, na.rm = TRUE, type="default", ...)

\method{seasonalfunction}{zoo}(x, FUN, na.rm = TRUE, type="default", ...)

\method{seasonalfunction}{data.frame}(x, FUN, na.rm = TRUE, type="default",
                          dates=1, date.fmt = "\%Y-\%m-\%d", 
                          out.type = "data.frame", verbose = TRUE, ...)
                          
\method{seasonalfunction}{matrix}(x, FUN, na.rm = TRUE, type="default",
                          dates=1, date.fmt = "\%Y-\%m-\%d", 
                          out.type = "data.frame", verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, data.frame or matrix object, with daily or monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that will be applied to ALL the values in \code{x} belonging to each one of the 4 weather seasons (e.g., \code{FUN} can be some of \code{mean}, \code{max}, \code{min}, \code{sd}). \cr
}
  \item{na.rm}{
Logical. Should missing values be removed before the computations? \cr
-) \kbd{TRUE} : the monthly values  are computed considering only those values in \code{x} different from NA  (\bold{very important when \code{FUN=sum}}) \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the FUN and monthly values are NA    
}
  \item{type}{
character, indicating which weather seasons will be used for computing the output. Possible values are: \cr
-) \kbd{default}          =>  "winter"= Dec, Jan, Feb;      "spring"= Mar, Apr, May;  "summer"=Jun, Jul, Aug;       "autumn"= Sep, Oct, Nov  \cr
-) \kbd{FrenchPolynesia}  =>  "winter"= Dec, Jan, Feb, Mar; "spring"= Apr, May;       "summer"=Jun, Jul, Aug, Sep;  "autumn"= Oct, Nov 
}
  \item{dates}{
numeric, factor, Date indicating how to obtain the dates. \cr
If \code{dates} is a number (default), it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, by using the date format specified by \code{date.fmt}  \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of element in \code{x}
}
  \item{date.fmt}{
Character indicating the format in which the dates are stored in \code{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character defining the desired type of output. Valid values are: \cr
-) \kbd{data.frame}: a data.frame, with 4 columns representing the weather seasons, and as many rows as stations are included in \code{x} \cr
-) \kbd{db}        : a data.frame, with 4 colums will be produced. Useful for a posterior boxplot \cr
                 The first column (StationID) will store the ID of the station,  \cr
                 The second column (Year) will store the year,              \cr
                 The third column (Season) will store the season,  \cr
                 The fourth column (Value) will contain the seasonal value corresponding to that year and that station.
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
\note{
\code{FUN} is applied to all the values of \code{x} belonging to each one of the four weather seasons, so the results of this function depends on the frequency sampling of \code{x} and the type of function given by \code{FUN} \cr
}
\section{Warning}{
The \var{FUN} value for the winter season (DJF) is computed considering the consecutive months of December, January and February. Therefore, if \code{x} starts in January and ends in December of any year, the winter value of the first year is computed considering only the January and February value of that year, whereas the December value of the first year is used to compute the winter value of the next year.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dm2seasonal}}, \code{\link{time2season}}, \code{\link{monthlyfunction}}, \code{\link{annualfunction}}, \code{\link{extract}} 
}
\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

# Amount of years
nyears <- yip(from=start(x), to=end(x), out.type="nmbr")

## Mean annual precipitation.
# It is necessary to divide by the amount of years to obtain the mean annual value, 
# otherwise it will give the total precipitation for all the 70 years
seasonalfunction(x, FUN=sum, na.rm=TRUE) / nyears

#####################
### verification ####
# Mean winter (DJF) value
sum( extractzoo(x, trgt="DJF") ) / nyears

# Mean spring (MAM) value
sum( extractzoo(x, trgt="MAM") ) / nyears

# Mean summer (JJA) value
sum( extractzoo(x, trgt="JJA") ) / nyears

# Mean autumn (SON) value
sum( extractzoo(x, trgt="SON") ) / nyears

\dontshow{
############
############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Winter (DJF) mean values of precipitation for the first 3 stations 
## in 'EbroPPtsMonthly' (its first column stores the dates)
seasonalfunction(x[,1:4], FUN=mean, dates=1)

## The same previous example, but using a zoo object
dates <- as.Date(x[,1]) # dates of the zoo object
z     <- zoo(x[ ,2:ncol(x)], dates) # zoo creation
seasonalfunction(z, FUN=mean)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
