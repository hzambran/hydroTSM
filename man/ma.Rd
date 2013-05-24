%% File ma.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{ma}
\Rdversion{1.1}
\alias{ma.zoo}
\alias{ma.default}
\alias{ma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Average
}
\description{
Generic function for computing a moving (sliding) average of ts.
}
\usage{
ma(x, ...)

\method{ma}{default}(x, win.len, FUN = mean, ...)

\method{ma}{zoo}(x, win.len, FUN = mean, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
ts or zoo object.
}
  \item{win.len}{
number of terms that will be considered in the mean. It have to be odd
}
  \item{FUN}{
Function that have to be applied for computing the moving average. Usually, \code{FUN} MUST be \code{mean}
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
a vector with the moving average termns. The length of the resulting vector is the same of \code{x}, but the first and last \code{(win.len-1)/2} elements will be NA.

%%  ~Describe the value returned
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)
x <- OcaEnOnaQts

## Daily to Monthly ts
m <- daily2monthly(x, FUN=mean, na.rm=FALSE)

# Plotting the monthly values
plot(m, xlab="Time")

## Plotting the annual moving average in station 'x'
lines(ma(m, win.len=12), col="blue")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
