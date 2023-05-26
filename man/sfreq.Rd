%% File sfreq.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{sfreq}
\Rdversion{1.1}
\alias{sfreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sampling Frequency 
}
\description{
This function identifies the sampling frequency of a zoo object. It is wrapper to the \code{\link[xts]{periodicity}} function of the \pkg{xts} package.
}
\usage{
sfreq(x, min.year = 1800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
variable of type zoo, xts or ts, with AT LEAST 2 elements, AND with a (sub)hourly, hourly, daily, weekly, monthly, quarterly, or annual (yearly) sampling frequency.
}
  \item{min.year}{
integer used for a correct identification of the sampling frequency when \code{x} is an annual (yearly) time series.
}
}
\details{
See further details in the \code{\link[xts]{periodicity}} function of the \pkg{xts} package.
}
\value{
Character. Possible values are: \cr
-) \kbd{minute}    : indicating that the sampling frequency in \code{x} is sub-hourly \cr
-) \kbd{hourly}    : indicating that the sampling frequency in \code{x} is hourly     \cr
-) \kbd{daily}     : indicating that the sampling frequency in \code{x} is daily      \cr
-) \kbd{weekly}    : indicating that the sampling frequency in \code{x} is weekly     \cr
-) \kbd{monthly}   : indicating that the sampling frequency in \code{x} is monthly    \cr
-) \kbd{quarterly} : indicating that the sampling frequency in \code{x} is quarterly  \cr
-) \kbd{annual}    : indicating that the sampling frequency in \code{x} is annual 
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{frequency}}, \link[xts]{periodicity}
}
\examples{
## Ex1: sub-hourly data
## Creating a dummy 15-min zoo object, with 1 as the only value in each time period
dt  <- seq( from=as.POSIXct("2021-06-30 00:15"), to=as.POSIXct("2021-06-30 23:45"), by="15 min" )
ndt <- length(dt)
sh <- zoo( rep(1, ndt), dt)
sfreq(sh)


## Ex2: hourly data 
## Loading the time series of HOURLY streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts
sfreq(x)


## Ex3: Daily data
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain)
data(OcaEnOnaQts)
d <- OcaEnOnaQts
sfreq(d)


## Ex4: Monthly data
m <- daily2monthly(d, FUN=mean, na.rm=TRUE)
sfreq(m)


## Ex5: Annual data
a <- daily2annual(d, FUN=mean, na.rm=TRUE)
sfreq(a)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
