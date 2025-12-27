%%File plot_pq.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 http://www.rforge.net/hydroTSM/ ; 
%%                                 https://cran.r-project.org/package=hydroTSM
%% Copyright 2023-2024 Mauricio Zambrano-Bigiarini
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
Given a time series of precipitation and streamflow, this function plots the two time series in the same figure, streamflows as a normal time series and preciitation as bars comming from the upper part of the plotting window. 
}
\usage{
plot_pq(p, ...)

\method{plot_pq}{zoo}(p, q, ptype=c("original", "monthly"),
                na.fill=c("remove", "linear", "spline"), 
                from=start(p), to=end(p), date.fmt=NULL, tz=NULL,
                main=ifelse(ptype=="original", "Precipitation and Streamflows", 
                            "Monthly Precipitation and Streamflows"),
                xlab=ifelse(ptype=="original", "Time", "Month"), 
                ylab=c("P, [mm]", "Q, [m3/s]"), 
                p.col=ifelse(ptype=="original", "blue", "lightblue"),
                q.col=ifelse(ptype=="original", "black", "blue"), 
                leg.title="", leg.text=c("P", "Q"),
                q.pch=16, q.cex=0.3,
                            
                start.month=1, 
                plot.p.probs=TRUE, p.probs=c(0.25, 0.75), 
                p.alpha=0.8,
                plot.q.probs=TRUE, q.probs=c(0.25, 0.75), 
                q.probs.col="lightskyblue1", q.probs.alpha=0.8, 
                labels=TRUE, labels.cex=0.8,
                labels.p.dy=-median(daily2monthly(p, FUN=sum, na.rm=TRUE), 
                             na.rm=TRUE)*1.1,
                labels.q.dx=c(rep(-0.2,6), rep(0.2,6)),
                labels.q.dy=rep(median(q, na.rm=TRUE)*1.3, 12),
                
                ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
zoo object with precipitation time series, with any time frequency.
}
  \item{q}{
zoo object with streamflow time series, with any time frequency.
}
  \item{ptype}{
Character indicating the type of plot to be produced. Valid values are: \cr
-) \kbd{original} => a time series plot with precipitation in the upper panel (as bars from the time axis) and streamflows in the lower pannel (as dotted lines). \cr
-) \kbd{monthly}  => a plot with mean monthly values of pecipitation in the upper panel (as bars from top to bottom) and streamflows in the lower panel (as bars from bottom to up). Quantiles of precipitation and streamflows are also plotted depending on the values defined in \code{p.probs} and \code{q.probs}, respectively.
}
  \item{na.fill}{
Character indicating how to fill any NA present in \code{x}. Valid values are: \cr
-) \kbd{remove} => NAs are not plotted
-) \kbd{linear} => NAs are removed by linear interpolation, using \code{\link[zoo]{na.approx}} \cr 
-) \kbd{spline} => NAs are removed by spline interpolation, using \code{\link[zoo]{na.spline}}
}
  \item{from}{
Character indicating the starting date for subsetting \code{p} and \code{q}. The default value corresponds to the date of the first element of \code{p} \cr
It has to be in the format indicated by \code{date.fmt}. 
}
  \item{to}{
Character indicating the ending date for subsetting \code{p} and \code{q}. The default value corresponds to the date of the last element of \code{p} \cr
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
  \item{main}{
The main title (on top) of the figure.
}
  \item{xlab}{
a title for the \code{x} axis. See \code{\link[graphics]{title}}.
}
  \item{ylab}{
a two-element title for the \code{y} axis: see \code{\link[graphics]{title}}. \cr
The first element is used for the right \code{y} axis (i.e., for precipitation). The second element is used for the left \code{y} axis (i.e., for streamflows).
}
  \item{p.col}{
character, representing the colors to be used for ploting the precipitation time series.  
}
  \item{q.col}{
character, representing the colors to be used for ploting the streamflow time series. 
}
  \item{leg.title}{
a character string or length-one expression giving a title to be placed at the top of the legend. \code{\link[graphics]{legend}}.
}
  \item{leg.text}{
a two-element character to appear in the legend placed at the bottom of the figure. \cr
The first element is used for precipitation and the second element is used for streamflows.
}
  \item{q.pch}{
numeric, representing the symbols used for ploting the streamflow time series.
}
  \item{q.cex}{
a numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{par("cex")}. See \code{\link[graphics]{plot.default}}
}
  \item{start.month}{
numeric in [1:12] indicating the starting month for the monthlycurve. Numeric values in [1, 12] represents months in [January, December]. By default \code{start.month=1}.
}
  \item{plot.p.probs}{
logical used to decide whether to show lower an upper uncertainty values for each one of the 12 monthly precipitation values. By default \code{plot.p.probs=TRUE}. \cr 
When \code{plot.p.probs=TRUE} the \code{p.probs} argument is used to define the values of the lower an upper uncertainty values.
} 
  \item{p.probs}{
numeric of length 2. It defines the quantile values used to compute the lower an upper uncertainty values for each one of the 12 monthly precipitation values. This uncertainty values are drawn as vertical lines over the bars used to plot the 12 monthly precipitation values. \cr 
By default \code{p.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly precipitation values. 
If \code{p} is a (sub)daily zoo object, it is first aggregated into monthly values using \code{mean}, and then the \code{p.probs} quantiles are computed over all the monthly values belonging to a calendar month.
} 
  \item{p.alpha}{
numeric of length 1, with the factor used to modify the opacity of \code{p.col}. Typically in [0,1], with 0 indicating a completely transparent colour and 1 indicating no transparency.
} 
  \item{plot.q.probs}{
logical used to decide whether to show uncertainty bands around each one of the 12 monthly average or median streamflow values. By default \code{plot.q.probs=TRUE}. \cr 
When \code{plot.q.probs=TRUE} the \code{q.probs} argument is used to define the values of the lower an upper uncertainty bands.
} 
  \item{q.probs}{
numeric of length 2. It is used to define quantile values used to compute the lower an upper uncertainty bands around each one of the 12 monthly average or median streamflow. \cr 
If \code{pcp} is provided, then \code{ubands.probs} are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly average or median precipitation values. \cr
By default \code{ubands.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly average or median values. 
If \code{x} is provided and is a (sub)daily zoo object, it is first aggregated into monthly values using \code{FUN}, and then the \code{ubands.probs} quantiles are computed over all the monthly values belonging to a calendar month.
}
  \item{q.probs.col}{
character with the color used to plot the uncertainty bands around the average or median streamflow values. \cr
} 
  \item{q.probs.alpha}{
numeric of length 1, with the factor used to modify the opacity of \code{q.probs.col}. Typically in [0,1], with 0 indicating a completely transparent colour and 1 indicating no transparency.
} 
  \item{labels}{
logical. Should monthly streamflow values to be shown above the lines?. By default \code{labels=TRUE}.
} 
  \item{labels.cex}{
numeric giving the amount by which plotting characters used to show the numeric values of monthly streamflow values are scaled relative to the default. 
}
  \item{labels.p.dy}{
numeric of length 12 giving the amount of vertical coordinate positions that have to be used to vertically shift the labels of monthly precipitation values. \cr
It is only used when \code{labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{labels.q.dx}{
numeric of length 12 giving the amount of horizontal coordinate positions that have to be used to horizontally shift the labels of monthly streamflow values. \cr
It is only used when \code{labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{labels.q.dy}{
numeric of length 12 giving the amount of vertical coordinate positions that have to be used to vertically shift the labels of monthly streamflow values. \cr
It is only used when \code{labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
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
 \code{\link{hydroplot}}, \code{\link{climograph}}, \code{\link{fdc}}, \code{\link{fdcu}}, \code{\link{monthlyfunction}}
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

# Subsetting to only the 1981-1990 temporal period
Cauquenes7336001 <- window(Cauquenes7336001, start="1981-01-01", end="1990-12-31")

## Plotting P and Q for the full time period of both time series
plot_pq(p=p, q=q)

######################
## Ex2: Plotting precipitation and streamflows only for a specific time period,
##      from April to December 2000.
plot_pq(p, q, from="2000-04-01", to="2000-12-31")

######################
## Ex3: Plotting monthly values of precipitation and streamflows for the 
##      full time period of both time series.
plot_pq(p, q, ptype="monthly")

######################
## Ex4: Plotting monthly values of precipitation and streamflows for the 
##      full time period of both time series, but using a hydrologic year
##      starting on April
plot_pq(p, q, ptype="monthly", start.month=4)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
%%\keyword{ ~kwd2 }
