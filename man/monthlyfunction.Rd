\name{monthlyfunction}
\Rdversion{1.1}
\alias{monthlyfunction}
\alias{monthlyfunction.zoo}
\alias{monthlyfunction.default}
\alias{monthlyfunction.matrix}
\alias{monthlyfunction.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Monthly Function
}
\description{
Generic function for obtaining 12 monthly values of a zoo object, by applying any R function to ALL the values in the object belonging to each one of the 12 calendar months (Jan...Dec).
}
\usage{
monthlyfunction(x, ...)

\method{monthlyfunction}{default}(x, FUN, na.rm = TRUE, ...)

\method{monthlyfunction}{zoo}(x, FUN, na.rm=TRUE,...)

\method{monthlyfunction}{data.frame}(x, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
             out.type = "data.frame", verbose = TRUE, ...)
             
\method{monthlyfunction}{matrix}(x, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
             out.type = "data.frame", verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts, data.frame or matrix object, with daily or monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{FUN}{
Function that will be applied to ALL the values in \code{x} belonging to each one of the 12 months of the year (e.g., FUN can be some of \code{mean}, \code{sum}, \code{max}, \code{min}, \code{sd}).
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly values and FUN are computed considering only those values in \code{x} different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the corresponding monthly value will be NA 
}
  \item{dates}{
It is only used when \code{x} is not a zoo object. \cr
numeric, factor, Date indicating how to obtain the dates. \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into 'Date' class, using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
It is only used when \code{x} is not a zoo object. \cr
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
It is only used when \code{x} is a matrix or data.frame. \cr
Character defining the desired type of output. Valid values are: \cr
-) \kbd{data.frame}: a data.frame, with 12 columns representing the months, and as many rows as gauging stations are included in \code{x} \cr
-) \kbd{db}        : a data.frame, with 4 columns will be produced. Useful for a posterior boxplot \cr
                 The first column ('StationID') will store the ID of the station,  \cr
                 The second column ('Year') will store the year, \cr
                 The third column ('Month') will store month,  \cr
                 The fourth column ('Value') will contain the monthly value corresponding to the three previous columns.
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
\value{
When \code{x} is a zoo object, a numeric vector with 12 elements representing the computed monthly value for each month. \cr
When \code{x} is a data.frame which columns represent measurements at different gauging stations, the resulting object is a data.frame with 12 columns and as many rows as gauging stations are in \code{x}, each row storing the computed 12 monthly value for each gauging station.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
\note{
Due to the fact that \code{FUN} is applied over all the elements in \code{x} belonging to a given calendar month, its result will depend on the sampling frequency of \code{x} and the type of function provided by \code{FUN} (\bold{special attention have to be put when \code{FUN=sum}})
}

\seealso{
\code{\link{annualfunction}}, \code{\link{seasonalfunction}}, \code{\link{dm2seasonal}}, \code{\link{daily2monthly}}, \code{\link{daily2annual}}
}
\examples{
## Loading daily streamflows (3 years) at the station 
## Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Mean monthly streamflows at station 'x'
monthlyfunction(x, FUN=mean, na.rm=TRUE)


############################
## Boxplot of monthly values

## Daily to Monthly
m <- daily2monthly(x, FUN=mean, na.rm=TRUE)

## Median of the monthly values at the station
monthlyfunction(m, FUN=median, na.rm=TRUE)

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "\%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values
boxplot( coredata(m) ~ months, col="lightblue", main="Monthly streamflows, [m3/s]")


##############################
##############################
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Dates of 'x'
dates <- as.Date(x[,1])

## Monthly precipitation of all the stations in 'x'
\dontrun{

## Sum of the monthly values in each station of 'x'
z <- zoo( x[, 2:ncol(x)], dates)

# Amount of years in 'x' (needed for computing the average)
nyears <- yip(from=start(z), to=end(z), out.type="nmbr" )

m <- monthlyfunction(z, FUN=sum)


## Another way of computing the sum of the monthly values in each station of 'x'
## This way is usefult for posteriori boxplots
m2 <- monthlyfunction(x, FUN=sum, dates=1, out.type="db")

## Average monthly precipitation in each station of 'x'
m2$Value <- m2$Value / nyears 

## Creating monthly factors
m2$Month <- factor(m2$Month, levels=month.abb)

## boxplot of the monthly values in all stations
boxplot(Value ~ Month, m2, col="lightyellow", main="Monthly Precipitation, [mm/month]")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
