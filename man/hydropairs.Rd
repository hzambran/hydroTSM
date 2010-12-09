\name{hydropairs}
\Rdversion{1.1}
\alias{hydropairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visual Correlation Matrix
}
\description{
Visualization of a correlation matrix. On top the (absolute) value of the correlation plus the result of the \code{\link[stats]{cor.test}} as stars. On bottom, the bivariate scatterplots, with a fitted line. On the diagonal, an histogram of each variable.
}
\usage{
hydropairs(x, dec = 3, use = "pairwise.complete.obs", method = "pearson",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data.frame or matrix object with measurements at several locations. Each column of \code{x} represent values measured at different locations.
}
  \item{dec}{
decimal places to be used for showing the correlation values
}
  \item{use}{
See \code{\link[stats]{cor}}. An optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".
}
  \item{method}{
See \code{\link[stats]{cor}}. A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated
}
  \item{\dots}{
further arguments passed to or from other methods, in particular it is used in the \code{\link[graphics]{pairs}} function.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{On top}{ the (absolute) value of the correlation plus the result of the cor.test as points}
\item{On bottom}{the bivariate scatterplots, with a fitted line}
\item{On diagonal}{histograms (from \code{\link[graphics]{pairs}})}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
\note{
Original idea taken from: \url{http://addictedtor.free.fr/graphiques/graphcode.php?graph=137}. \cr

Histogram panel was taken from the R help of the original \code{\link[graphics]{pairs}} function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cor}}, \code{\link{pairs}}
}
\examples{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Visualizing the correlation among the monthly precipitation values 
## of the first 3 gauging stations in 'EbroPPtsMonthly'. 
## The first column of 'EbroPPtsMonthly' has the dates.
hydropairs(EbroPPtsMonthly[,2:4])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{graphs}
