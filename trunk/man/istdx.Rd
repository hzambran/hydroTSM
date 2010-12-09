\name{istdx}
\Rdversion{1.1}
\alias{istdx}
\alias{istdx.default}
\alias{istdx.matrix}
\alias{istdx.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse Standarization
}
\description{
This function back transforms a standarized vector/matrix \code{z} into their original values, i.e., re-scales all the values in the [0,1] interval to the original range of values  \code{z = re-scale(x) = x*[ xmax - xmin ] + xmin}.       
}
\usage{
istdx(x, ...)
\method{istdx}{default}(x, xmin, xrange, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
standarized vector or matrix to be re-scaled, all the values have to be in the range [0,1]
}
  \item{xmin}{
numeric with the minimum value(s) in the original \code{x} \cr
-) if \code{x} is a vector, \code{xmin} has to be a real \cr
-) if \code{x} is a matrix/data.frame, \code{xmin} has to be a vector, with the minimum values for each column of the original \code{x}. In this case, the vector of minimums can be obtained as: \code{xmin <-  apply(x, 2, min, na.rm=TRUE)}
}
  \item{xrange}{
numeric with the range of value(s) in the original \code{x} \cr
-) if \code{x} is a vector, \code{xrange} has to be a real \cr
-) if \code{x} is a matrix/data.frame, \code{xrange} has to be a vector, with the range of values for each column of the original \code{x}.
   In this case, the vector of ranges can be obtained as: \cr
   \code{xrange <-  apply(x, 2,range, na.rm=TRUE)} \cr
   \code{xrange <-  apply(xrange, 2, diff, na.rm=TRUE)} 
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
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{stdx}}, \code{\link{scale}}
}
\examples{
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Computing xmin and the range of 'x'
xmin <- min(x, na.rm=TRUE)
r <- diff(range(x, na.rm=TRUE))

## Standarized variable
s <- stdx(x)

## Inverse of the standarized variable
si <- istdx(s, xmin, xrange=r)

## 'si' and 'x' should be the same
summary(x-si)

###########
### Standarizing a subset of the stations 9 to 12 in 'EbroPPtsMonthly'

## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

pp <- EbroPPtsMonthly[1:70,10:13]
xmin   <- apply(pp, 2, min, na.rm=TRUE)
xrange <- apply(pp, 2, range, na.rm=TRUE)
xrange <- apply(xrange, 2, diff, na.rm=TRUE)

## Standarized variable
s <- stdx(as.matrix(pp))

## Inverse of the standarized variable
si <- istdx(s, xmin, xrange)

## 'si' and 'pp' should be the same
summary(pp - si)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
