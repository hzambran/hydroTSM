\name{hydroplot}
\Rdversion{1.1}
\alias{sname2plot}
\alias{hydroplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hydrological time series plotting and extraction.
}
\description{
\code{hydroplot}: When \code{x} is a zoo object it plots (a maximum of) 9 graphs (lines plot, boxplots and/or histograms) of the daily, monthly and/or annual time series. \cr

\code{sname2plot}: When \code{x} is a data frame whose columns contain the time series of several gauging stations, it takes the name of one gauging station and plots the graphs described before.
}
\usage{
hydroplot(x, FUN, na.rm=TRUE, ptype="ts+boxplot+hist", pfreq="dma",                      
          var.type, var.unit="units", main=NULL, xlab="Time", ylab,
          win.len1=365*1, win.len2=365*3, tick.tstep="auto", lab.tstep="auto", 
          lab.fmt=NULL, cex=0.3, cex.main=1.3, cex.lab=1.3, cex.axis=1.3, 
          col=c("blue", "lightblue", "lightblue"), ...)

sname2plot(x, sname, FUN, na.rm=TRUE, ptype="ts+boxplot+hist", pfreq="dma",                      
           var.type, var.unit="units",main=NULL, xlab="Time", ylab=NULL,
           win.len1=365*1, win.len2=365*3, tick.tstep="auto", lab.tstep="auto",
           lab.fmt=NULL, cex=0.3, cex.main=1.3, cex.lab=1.3, cex.axis=1.3,
           col=c("blue", "lightblue", "lightblue"), 
           dates, date.fmt = "\%Y-\%m-\%d", from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo, xts or data.frame object, with columns storing the time series of one or more gauging stations.
}
  \item{sname}{
ONLY required when \code{x} is a data frame. \cr
Character representing the name of a station, which have to correspond to one column name in \code{x}
}
  \item{FUN}{
ONLY required when \code{var.type} is missing AND \code{pfreq != "o"}. \cr
Function that have to be applied for transforming from daily to monthly or annual time step (e.g., For precipitation \code{FUN=sum} and for temperature and flow ts, \code{FUN=mean}) 
}
  \item{na.rm}{
Logical. Should missing values be removed before the computations?
}
  \item{ptype}{
Character indicating the type of plot that will be plotted. Valid values are: \cr
-) \kbd{ts}              => only time series                  \cr
-) \kbd{ts+boxplot}      => only time series + boxplot        \cr       
-) \kbd{ts+hist}         => only time series + histogram      \cr
-) \kbd{ts+boxplot+hist} => time series + boxplot + histogram 
}
  \item{pfreq}{
Character indicating how many plots are desired by the user. Valid values are: \cr                    
-) \kbd{dma}: Daily, Monthly and Annual values are plotted \cr
-) \kbd{dm} : Daily and Monthly values are plotted  \cr
-) \kbd{ma} : Monthly and Annual values are plotted \cr
-) \kbd{o}  : only the original zoo object is plotted, and \code{ptype} is changed to \kbd{ts}
}
  \item{var.type}{
ONLY required when \code{FUN} is missing. \cr
character representing the type of variable being plotted. Used for determining the function used for computing the monthly and annual values when \code{FUN} is missing. Valid values are: \cr
-) \kbd{Precipitation} => \code{FUN=sum}  \cr
-) \kbd{Temperature}   => \code{FUN=mean} \cr
-) \kbd{Flow}          => \code{FUN=mean} \cr
}
  \item{var.unit}{
Character representing the measurement unit of the variable being plotted. ONLY used for labelling the axes (e.g., "mm" for precipitation, "C" for temperature, and "m3/s" for flow.)
}
  \item{main}{
Character representing the main title of the plot. If the user do not provide a title, this is created automatically as: \code{main= paste(var.type, "at", sname, sep=" ")}, 
}
  \item{xlab}{
A title for the x axis. See \code{\link[graphics]{plot}}. 
}
  \item{ylab}{
A title for the y axis. See \code{\link[graphics]{plot}}. 
}
  \item{win.len1}{
number of days for being used in the computation of the first moving average. A value equal to zero indicates that this moving average is not going to be computed.
}
  \item{win.len2}{
number of days for being used in the computation of the second moving average. A value equal to zero indicates that this moving average is not going to be computed.
}
  \item{tick.tstep}{
Character indicating the time step that have to be used for putting the ticks on the time axis. Valid values are: \cr
-) \kbd{days},    \cr
-) \kbd{months},  \cr
-) \kbd{years} 
}
  \item{lab.tstep}{
Character indicating the time step that have to be used for putting the labels on the time axis. Valid values are: \cr
-) \kbd{days},   \cr
-) \kbd{months}, \cr
-) \kbd{years} 
}
  \item{lab.fmt}{
Character indicating with the format to be used for the label of the axis. See \code{format} in \code{\link[base]{as.Date}}. If not specified, it will try \kbd{"\%Y-\%m-\%d"} when \code{lab.tstep=="days"}, \kbd{"\%b"} when \code{lab.tstep=="month"}, and \kbd{"\%Y"} when \code{lab.tstep=="year"}. 
}
  \item{cex}{
A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default. (See \code{\link[graphics]{par}}).
}
  \item{cex.main}{
The magnification to be used for main titles relative to the current setting of \code{cex} (See \code{\link[graphics]{par}}).
}
  \item{cex.lab}{
The magnification to be used for x and y labels relative to the current setting of \code{cex} (See \code{\link[graphics]{par}}).
}
  \item{cex.axis}{
The magnification to be used for axis annotation relative to the current setting of \code{cex} (See \code{\link[graphics]{par}}).
}
  \item{col}{
A character vector with 3 elements, representing the colors to be used for plotting the lines of the ts, the boxplots, and the histograms, respectively. \cr
When \code{pfreq="o"}, only one character element is needed.
See \code{\link[graphics]{plot.default}}).
}
  \item{dates}{
ONLY required when \code{x} is a data frame. It is a numeric, factor or Date object indicating how to obtain the dates corresponding to the \code{sname} station. \cr
If \code{dates} is a number, it indicates the index of the column in \code{x} that stores the dates \cr
If \code{dates} is a factor, it is converted into Date class, using the date format  specified by \code{date.fmt} \cr
If \code{dates} is already of Date class, the code verifies that the number of days in \code{dates} be equal to the number of element in \code{x}
}
  \item{date.fmt}{
Character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{from}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the starting date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
  \item{to}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the ending date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
  \item{\dots}{
further arguments passed to the \code{plot.zoo} and \code{Axis} functions or from other methods.
}
}
\details{
Plots of the daily/monthly/annual time series given as input. \cr
Depending on the value of \code{pfreq}, daily, monthly and/or annual time series plots, boxplots and histograms are produced. \cr
Depending on the value of \code{ptype}, time series plots, boxplots and/or histograms are produced.
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
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sname2ts}}
}
\examples{
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)

# 3 ts, 3 boxplots and 3 histograms
hydroplot(OcaEnOnaQts, FUN=mean, ylab= "Q", var.unit = "m3/s")

# only the original time series
hydroplot(OcaEnOnaQts, pfreq="o")

#############
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

## Plotting the monthly and annual values of precipitation at station "P9001", 
## stored in 'EbroPPtsMonthly'.
sname2plot(EbroPPtsMonthly, sname="P9001", var.type="Precipitation", dates=1, pfreq="ma")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
\keyword{manip}
