%% File sfreq.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{sfreq}
\Rdversion{1.1}
\alias{sfreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sampling Frequency 
}
\description{
This function identifies the sampling frequency of a zoo object. So far, it only works with daily/monthly/annual ts.
}
\usage{
sfreq(x, min.year = 1800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
variable of type zoo, xts or ts, with AT LEAST 2 elements, AND with a daily, monthly or annual sampling frequency.
}
  \item{min.year}{
integer used for a correct identification of the sampling frequency when \code{x} is an annual time series.
}
}
%%\details{
%%
%%}
\value{
Character. Possible values are: \cr
-) \kbd{daily}   : indicating that the sampling frequency in \code{x} is daily   \cr
-) \kbd{monthly} : indicating that the sampling frequency in \code{x} is monthly \cr
-) \kbd{annual}  : indicating that the sampling frequency in \code{x} is annual 
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{frequency}}
}
\examples{
## Loading temperature data ##
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)
d <- OcaEnOnaQts

## Daily to Monthly
m <- daily2monthly(d, FUN=mean, na.rm=TRUE)

## Daily to Annual
a <- daily2annual(d, FUN=mean, na.rm=TRUE)

# Daily zoo object
sfreq(d)

# Monthly zoo object
sfreq(m)

# Annual zoo object
sfreq(a)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
