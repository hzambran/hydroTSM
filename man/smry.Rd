\name{smry}
\Rdversion{1.1}
\alias{smry}
\alias{smry.default}
\alias{smry.zoo}
\alias{smry.Date}
\alias{smry.matrix}
\alias{smry.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary
}
\description{
Extended summary function for numeric objects, with 13 summary statistics. 
}
\usage{
smry(x, ...)

\method{smry}{default}(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

\method{smry}{zoo}(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

\method{smry}{Date}(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

\method{smry}{matrix}(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)

\method{smry}{data.frame}(x, na.rm=TRUE, digits = max(3, getOption("digits")-3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numeric object, vector, matrix or data.frame, for which a summary is desired.
}
  \item{na.rm}{
a logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{digits}{
numeric, with the amount of decimal places to be included in the result
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Computed summary statistics are: \cr
\item{Min}{Minimum}
\item{1stQ}{First quartile (lower-hinge)}
\item{Mean}{Mean value}
\item{Median}{Median}
\item{3rdQ}{Third quartile ( upper-hinge}
\item{Max}{Maximum of the input values.}
\item{IQR}{Interquartile Range. \code{IQR(x) = quantile(x,3/4) - quantile(x,1/4)}}
\item{sd}{Standard deviation. It uses 'n-1' as denominator.}
\item{cv}{Coefficient of variation ( \code{cv= sd / |mean|} )}
\item{skewness}{Skewness (using \pkg{e1071} package)   }
\item{kurtosis}{Kurtosis (using \pkg{e1071} package)   }
\item{n}{Total number of elements}
\item{NA's}{Amount of missing values}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}

\author{
Mauricio Zambrano-Bigiarini \email{mauricio.zambrano@ing.unitn.it}
}
\note{
Skewness and Kurtosis are computed with the e1071 package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{summary}}, \code{\link{fivenum}}, \code{\link{IQR}}, \code{\link{sd}}, \link[e1071]{skewness}, \link[e1071]{kurtosis} 
}
\examples{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Summary of monthly precipitation values for the first 7 stations in 'EbroPPtsMonthly'
smry(EbroPPtsMonthly[,2:8])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
