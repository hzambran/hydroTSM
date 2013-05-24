%%File stdx.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{stdx}
\Rdversion{1.1}
\alias{stdx}
\alias{stdx.default}
\alias{stdx.matrix}
\alias{stdx.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standarization
}
\description{
Standarizes a vector or matrix, i.e., scales all the values in a way that the transformed values will be within the range [0, 1]. 
}
\usage{
stdx(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector, matrix or data.frame to be scaled
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\details{

\deqn{z = \frac{x - x_{min}}{x_{max}-x_{min}}}{%
z = scale(x) = [ x - xmin ] / [ xmax - xmin ] }
}
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{scale}}, \code{\link{istdx}}
}
\examples{
############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

# Standarizing only some values of 'EbroPPtsMonthly'
stdx(as.matrix(EbroPPtsMonthly[1:70,10:13]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
