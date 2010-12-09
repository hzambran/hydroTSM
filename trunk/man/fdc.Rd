\name{fdc}
\Rdversion{1.1}
\alias{fdc.data.frame}
\alias{fdc.default}
\alias{fdc.matrix}
\alias{fdc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flow Duration Curve
}
\description{
Computes and plots the Flow Duration Curve (\acronym{FDC}) corresponding to a given time series of streamflow discharges. 
}
\usage{
fdc(x, ...)

\method{fdc}{default}(x,lQ.thr=0.7,hQ.thr=0.2, plot=TRUE, log="y", main="Flow Duration Curve", 
    xlab="\% Time flow equalled or exceeded", ylab="Q, [m3/s]", ylim, 
    col="black", pch=1, lwd=1, lty=1, cex=0.4, cex.axis=1.2, cex.lab=1.2,
    leg.txt=NULL, verbose= TRUE, thr.shw=TRUE, new=TRUE, ...)

\method{fdc}{matrix}(x, lQ.thr=0.7, hQ.thr=0.2, plot=TRUE, log="y", main= "Flow Duration Curve",                         
    xlab="\% Time flow equalled or exceeded", ylab="Q, [m3/s]", ylim, 
    col=palette("default")[1:ncol(x)], pch=1:ncol(x), lwd=rep(1, ncol(x)), 
    lty=1:ncol(x), cex=0.4, cex.axis=1.2, cex.lab=1.2, leg.txt= colnames(x), 
    verbose=TRUE, thr.shw=TRUE, new=TRUE, ...)

\method{fdc}{data.frame}(x, lQ.thr=0.7, hQ.thr=0.2, plot=TRUE, log="y", main= "Flow Duration Curve",                         
     xlab="\% Time flow equalled or exceeded", ylab="Q, [m3/s]", ylim, 
     col=palette("default")[1:ncol(x)], pch=1:ncol(x), lwd=rep(1, ncol(x)), 
     lty=1:ncol(x), cex=0.4, cex.axis=1.2, cex.lab=1.2, leg.txt= colnames(x), 
     verbose=TRUE, thr.shw=TRUE, new=TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric, zoo, data.frame or matrix object with the observed streamflows for which the flow duration curve have to be computed. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter). When \code{x} is a matrix or data.frame, the flow duration curve is computed for each column.
}
\item{lQ.thr}{
numeric, low-flow separation threshold. If this value is different from NA, a vertical line is drawn in this value, and all the values to the left of it are deemed low flows.
}
\item{hQ.thr}{
numeric, high-flow separation threshold. If this value is different from NA, a vertical line is drawn in this value, and all the values to the right of it are deemed high flows
}
  \item{plot}{
logical. Indicates if the flow duration curve should be plotted or not. Default value is TRUE.
}
  \item{log}{
character, indicates which axis has to be plotted with a logarithmic scale. Default value is \kbd{y} 
}
  \item{main}{
See \code{\link[graphics]{plot}}. An overall title for the plot: see \code{\link[graphics]{title}}.
}
  \item{xlab}{
A title for the x axis. See \code{\link[graphics]{plot}}. 
}
  \item{ylab}{
A title for the y axis. See \code{\link[graphics]{plot}}. 
}
  \item{ylim}{
The y limits of the plot. See \code{\link[graphics]{plot.default}}. 
}
  \item{col}{
The colors to be used for lines and points. Multiple colors can be specified so that each point can be given its own color. If there are fewer colors than points they are recycled in the standard fashion. Lines will all be plotted in the first colour specified. See \code{\link[graphics]{plot.default}}. 
}
  \item{pch}{
A vector of plotting characters or symbols: see \code{\link[graphics]{points}}. See \code{\link[graphics]{plot.default}}. 
}
  \item{lwd}{
The line width, see \code{\link[graphics]{par}}. See \code{\link[graphics]{plot.default}}. 
}
  \item{lty}{
The line type, see \code{\link[graphics]{par}}. See \code{\link[graphics]{plot.default}}. 
}
  \item{cex}{
See \code{\link[graphics]{plot.default}}. A numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default.  \cr
This works as a multiple of \env{par("cex")}. 'NULL' and 'NA' are equivalent to '1.0'.  Note that this does not affect annotation
}
  \item{cex.axis}{
magnification of axis annotation relative to 'cex'.
}
  \item{cex.lab}{
Magnification to be used for x and y labels relative to the current setting of 'cex'. See '?par'.
}
  \item{leg.txt}{
vector with the names that have to be used for each column of \code{x}.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed (when \code{x} is a matrix or data.frame). 
}
  \item{thr.shw}{
logical, indicating if the streamflow values corresponding to the user-defined thresholds \code{lQ.thr} and \code{hQ.thr} have to be shown in the plot.
}
  \item{new}{
logical, if TRUE (default), a new plotting window is created. 
}
  \item{\dots}{
further arguments passed to or from other methods (to the plotting functions)
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
numeric, matrix or data.frame whose columns contains the \% of time each one of the streamflow magnitudes given as input was equalled or exceeded. The resulting values have to be multiplied by 100 to get a percentage. \cr

When \code{plot} is TRUE (default), the resulting flow duration curve is plotted in a new window.
}
\references{
\cite{Vogel, R., and N. M. Fennessey (1994), Flow duration curves I: A new interpretation and confidence intervals, ASCE, Journal of Water Resources Planning and Management, 120(4)}. \cr

\cite{Vogel, R., and N. Fennessey (1995), Flow duration curves II: A review of applications in water resources planning, Water Resources Bulletin, 31(6), 1029-1039, doi:10.1111/j.1752-1688.1995.tb03419.x}. \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}.

}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fdcu}}
}
\examples{
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)

## Daily Flow Duration Curve
fdc(OcaEnOnaQts)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
