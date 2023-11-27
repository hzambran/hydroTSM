%%File plot_pq.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 http://www.rforge.net/hydroTSM/ ; 
%%                                 https://cran.r-project.org/package=hydroTSM
%% Copyright 2023-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{plot_pq}
\Rdversion{1.1}
\alias{plot_pq}
\alias{plot_pq.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Plot precipitation and streamflow time series in the same figure.
}
\description{
It takes an irregular zoo object (with non-existing values for some dates) and converts it into a regularly spaced zoo object within the time period defined by \code{from} and \code{to}, by filling the missing dates with \sQuote{NA}
}
\usage{
plot_pq(p, ...)

\method{plot_pq}{zoo}(p, q, from=start(x), to=end(x), date.fmt, tz,
                na.fill=c("remove", "linear", "spline"), 
                xlab="Time", ylab=c("P", "Q"), 
                main="Precipitation and Streamflows",
                leg.title="",  leg.text=c("P", "Q"),
                cols=c("blue", "black"), q.pch=16, q.cex=0.3)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
zoo object with precipitation time series, with any time frequency.
}
  \item{q}{
zoo object with streamflow time series, with any time frequency.
}
  \item{from}{
Character indicating the starting date for creating the regularly spaced zoo object. The default value corresponds to the date of the first element of \code{x} \cr
It has to be in the format indicated by \code{date.fmt}. 
}
  \item{to}{
Character indicating the ending date for creating the regularly spaced zoo object. The default value corresponds to the date of the last element of \code{x} \cr
It has to be in the format indicated by \code{date.fmt}.
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \sQuote{Details} section in \code{\link[base]{strptime}}.
By default, \code{date.fmt} is missing, and it is automatically set to \kbd{\%Y-\%m-\%d} when \code{time(x)} is \code{Date} object, and set to \kbd{\%Y-\%m-\%d \%H:\%M:\%S} when \code{x} is a sub-daily zoo object.
}
  \item{tz}{
character, with the specification of the time zone used for \code{from}, \code{to}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr

If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr

This argument can be used when working with sub-daily zoo objects to force using  time zones other than the local time zone for \code{from} and \code{to}. It should be used with caution, being well aware of the time zone of the data. See examples.
}
  \item{na.fill}{Character indicating how to fill any NA present in \code{x}. Valid values are: \cr
-) \kbd{remove}      => NAs are not plotted                                                         \cr
-) \kbd{linear}      => NAs are removed by linear interpolation, using \code{\link[zoo]{na.approx}} \cr 
-) \kbd{spline}      => NAs are removed by spline interpolation, using \code{\link[zoo]{na.spline}}
}
  \item{xlab}{
a title for the \code{x} axis. See \code{\link[graphics]{title}}.
}
  \item{ylab}{
a two-element title for the \code{y} axis: see \code{\link[graphics]{title}}. \cr
The first element is used for the right \code{y} axis (i.e., for precipitation). The second element is used for the left \code{y} axis (i.e., for streamflows).
}
  \item{main}{
The main title (on top) of the figure.
}
  \item{leg.title}{
a character string or length-one expression giving a title to be placed at the top of the legend. \code{\link[graphics]{legend}}.
}
  \item{leg.text}{
a two-element character to appear in the legend placed at the bottom of the figure. \cr
The first element is used for precipitation and the second element is used for streamflows.
}
  \item{cols}{
character, representing the colors to be used for ploting the precipitation and streamflow time series.  \cr
The first element is used for precipitation and the second element is used for streamflows.
}
  \item{q.pch}{
numeric, representing the symbols used for ploting the streamflow time series.
}
  \item{q.cex}{
a numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{par("cex")}. See \code{\link[graphics]{plot.default}}
}
  \item{\dots}{
further arguments passed to or from other methods. Not used yet.
}
}
\details{
Given a time series of precipitation and streamflow, this function plots the two time series in the same figure, streamflows as a normal time series and precipitation as bars comming from the upper part of the plotting window. 
}
\value{
A figure with the two time series in the same graphical area, streamflows as a normal time series and precipitation as bars comming from the upper part of the plotting window. 
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

\seealso{
\code{\link{monthlycurve}}, \code{\link{hydroplot}}, \code{\link{climograph}}, \code{\link{fdc}}, \code{\link{fdcu}}
}

\examples{
######################
## Ex1: Plotting precipitation and streamflows for the full time period of both
##      time series.
##      First, we load the daily P and Q time series for the Cauquenes en 
##      El Arrayan catchment. P, [mm] is the first column and Q, [mm] is the 
##      fifth column.

data(Cauquenes7336001)
p <- Cauquenes7336001[, 1]
q <- Cauquenes7336001[, 5]

## Plotting P and Q for the full time period of both time series
plot_pq(p=p, q=q)


## Ex2: Plotting precipitation and streamflows only for a specific time period,
##      from April to December 2000.
plot_pq(p, q, from="2000-04-01", to="2000-12-31")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%%\keyword{ ~kwd2 }
