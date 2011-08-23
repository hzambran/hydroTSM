\name{extractzoo}
\Rdversion{1.1}
\alias{extractzoo}
\alias{extractzoo.default}
\alias{extractzoo.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract from Zoo
}
\description{
Extracts from a zoo object all the values belonging to a given month, year or weather season.
}
\usage{
extractzoo(x, ...)

\method{extractzoo}{default}(x, trgt, ...)
     
\method{extractzoo}{zoo}(x, trgt, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo object
}
  \item{trgt}{
numeric or character indicating the elements to extract from \code{x}. Valid values are: \cr
1) integer from 1 to 12: \code{trgt} is considered as a month (1=JAN, 2=FEB,...., 12=DEC), and all the values in \code{x} belonging to the month specified by \code{trgt} will be extracted. \cr
2) integer > 12: \code{trgt} is considered as a year, and all the values in \code{x} belonging to the year specified by \code{trgt} will be extracted \cr
3) character: \code{trgt} is considered as a weather season, and all the values in \code{x} belonging to the season specified by \code{trgt} will be extracted. Valid values are: \cr
   -) \kbd{DJF} : December, January, February   \cr                   
   -) \kbd{MAM} : March, April, May             \cr                     
   -) \kbd{JJA} : June, July, August            \cr                      
   -) \kbd{SON} : September, October, November  \cr
   -) \kbd{DJFM}: December, January, February, March  \cr                   
   -) \kbd{AM}  : April, May                    \cr  
   -) \kbd{JJAS}: June, July, August, September \cr            
   -) \kbd{ON}  : October, November
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
a zoo object with the extracted values.
}
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
\code{\link{time2season}}, \code{\link{seasonalfunction}}, \code{\link{daily2annual}}, \code{\link{daily2monthly}}
}
\examples{
### Loading temperature data ##
data(SanMartinoPPts)
x <- SanMartinoPPts

## Extracting all the values belonging to February (FEB=2)
extractzoo(x, trgt=2)

## Extracting all the values belonging to the year 1970
extractzoo(x, trgt=1970)

## Extracting all the values belonging to the autumn
extractzoo(x, trgt="SON")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
