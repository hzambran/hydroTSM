\name{dm2seasonal}
\Rdversion{1.1}
\alias{dm2seasonal}
\alias{dm2seasonal.default}
\alias{dm2seasonal.zoo}
\alias{dm2seasonal.data.frame}
\alias{dm2seasonal.matrix}

\title{
Daily/Monthly -> Seasonal Values
}
\description{
Generic function for computing a seasonal value for every year of a daily/monthly time series.
}
\usage{
dm2seasonal(x, ...)

\method{dm2seasonal}{default}(x, season, FUN, na.rm = TRUE, out.fmt="\%Y", ...)

\method{dm2seasonal}{zoo}(x, season, FUN, na.rm = TRUE, out.fmt="\%Y", ...)

\method{dm2seasonal}{data.frame}(x, season, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
        out.type = "data.frame", out.fmt="\%Y", ...)
        
\method{dm2seasonal}{matrix}(x, season, FUN, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
        out.type = "data.frame", out.fmt="\%Y", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts, data.frame or matrix object, with daily or monthly time series. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter).
}
  \item{season}{
character, indicating the weather season to be used for selecting the data. Valid values are: \cr
-) \kbd{DJF} : December, January, February   \cr
-) \kbd{MAM} : March, April, May             \cr
-) \kbd{JJA} : June, July, August            \cr
-) \kbd{SON} : September, October, November  \cr
-) \kbd{DJFM}: December, January, February, March  \cr                   
-) \kbd{AM}  : April, May                    \cr   
-) \kbd{JJAS}: June, July, August, September \cr            
-) \kbd{ON}  : October, November
}
  \item{FUN}{
Function that will be applied to ALL the values of \code{x} belonging to the given weather season (e.g., \code{FUN} can be some of \code{mean}, \code{max}, \code{min}, \code{sd}). \cr
\bold{The FUN value for the winter season (DJF or DJFM) is computed considering the consecutive months of December, January and February/March}. See 'Note' section.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the seasonal values  are computed considering only those values different from NA (\bold{very important when \code{FUN=sum}}) \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a weather season, the corresponding seasonal values are NA
}
  \item{out.fmt}{
Character indicating the date format for the output time series. See \code{format} in \code{\link[base]{as.Date}}. Possible values are: \cr
-) \kbd{\%Y}        : only the year will be used for the time. Default option. (e.g., "1961" "1962"...) \cr
-) \kbd{\%Y-\%m-\%d}: a complete date format will be used for the time. (e.g., "1961-01-01" "1962-01-01"...)
}
  \item{dates}{
numeric, factor or Date object indicating how to obtain the dates. \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, by using the date format specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days on it be equal to the number of elements in \code{x}
}
  \item{date.fmt}{
Character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character that defines the desired type of output. Valid values are: \cr
-) \kbd{data.frame}: a data.frame, with as many columns as stations are included in \code{x}, the year corresponding to each seasonal value are used as row names. \cr
-) \kbd{db}        : a data.frame, with 4 columns will be produced. \cr
                 The first column (StationID) stores the ID of the station 
                 The second column (Year) stores the year,  \cr
                 The third column (Season) stores the season,\cr
                 The fourth column (Value) contains the seasonal value corresponding to the values specified in the previous three columns
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A numeric vector with the seasonal values for all the years in which \code{x} is defined.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
\note{
\code{FUN} is applied to all the values of \code{x} belonging to the selected season, so the results of this function depends on the frequency sampling of \code{x} and the type of function given by \code{FUN} \cr
}
\section{Warning}{
For any year, the \code{FUN} value for the winter season (DJF), is computed considering only January and February, and the value of December is used for computing the winter value of the next year.
}
\seealso{
\code{\link{seasonalfunction}}, \code{\link{time2season}}, \code{\link{extractzoo}}, \code{\link{daily2monthly}}, \code{\link{daily2annual}}, \code{\link{monthly2annual}}
}
\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- SanMartinoPPts

## Winter (DJF) mean values of precipitation for each year of 'x'
dm2seasonal(x, FUN=mean, season="DJF")

\dontshow{
############
############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)
x <- EbroPPtsMonthly

## Winter (DJF) mean values of precipitation for the first 3 stations 
## in 'EbroPPtsMonthly' (its first column stores the dates)
dm2seasonal(x[,1:4], FUN=mean, season="DJF", dates=1)

## The same previous example, but using a zoo object
dates <- as.Date(x[,1]) # dates of the zoo object
z     <- zoo(x[ ,2:ncol(x)], dates) # zoo creation
dm2seasonal(z, FUN=mean, season="DJF")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
