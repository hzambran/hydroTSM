\name{fdcu}
\Rdversion{1.1}
\alias{fdcu.data.frame}
\alias{fdcu.default}
\alias{fdcu.matrix}
\alias{fdcu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flow Duration Curve with uncertainty bounds.
}
\description{
Computes and plots the Flow Duration Curve (\acronym{FDC}) for the streamflows given by \code{x} and for two uncertainty bounds, with the possibility of plotting an additional FDC representing simulated streamflows for \code{x}, in order to compare them.
}
\usage{
fdcu(x, lband, uband, ...)

\method{fdcu}{default}(x, lband, uband, sim, lQ.thr=0.7, hQ.thr=0.2, plot=TRUE, log="y", 
     main="Flow Duration Curve", xlab="\% Time flow equalled or exceeded", 
     ylab="Q, [m3/s]", ylim, col=c("black", "red"), pch=c(1, 15), 
     lwd=c(1, 0.8), lty=c(1, 3), cex=0.2, cex.axis=1.2, cex.lab=1.2,
     leg.txt= c("Qobs", "Qsim", "95PPU"), verbose= TRUE, thr.shw=TRUE, border=NA, 
     bands.col="lightcyan", bands.density=NULL, bands.angle=45, new=TRUE, ...)

\method{fdcu}{matrix}(x, lband, uband, sim, lQ.thr=0.7, hQ.thr=0.2, plot=TRUE, log="y", 
     main="Flow Duration Curve", xlab="\% Time flow equalled or exceeded", 
     ylab="Q, [m3/s]", ylim, col=matrix(c(rep("black", ncol(x)), 
     palette("default")[2:(ncol(x)+1)]), byrow=FALSE, ncol=2), 
     pch=matrix(rep(c(1, 15), ncol(x)), byrow=TRUE, ncol=2),
     lwd=matrix(rep(c(1, 0.8), ncol(x)), byrow=TRUE, ncol=2),
     lty=matrix(rep(c(1, 3), ncol(x)), byrow=TRUE, ncol=2),                        
     cex=rep(0.1, ncol(x)), cex.axis=1.2, cex.lab=1.2, 
     leg.txt=c("OBS", colnames(x), "95PPU"),  verbose= TRUE,  thr.shw=TRUE, 
     border=rep(NA, ncol(x)), bands.col=rep("lightcyan", ncol(x)), 
     bands.density=rep(NULL, ncol(x)), bands.angle=rep(45, ncol(x)), new=TRUE, ...)

\method{fdcu}{data.frame}(x, lband, uband, sim, lQ.thr=0.7, hQ.thr=0.2, plot=TRUE, log="y", 
     main="Flow Duration Curve", xlab="\% Time flow equalled or exceeded", 
     ylab="Q, [m3/s]", ylim, col=matrix(c(rep("black", ncol(x)), 
     palette("default")[2:(ncol(x)+1)]), byrow=FALSE, ncol=2),
     pch=matrix(rep(c(1, 15), ncol(x)), byrow=TRUE, ncol=2),
     lwd=matrix(rep(c(1, 0.8), ncol(x)), byrow=TRUE, ncol=2),
     lty=matrix(rep(c(1, 3), ncol(x)), byrow=TRUE, ncol=2),                        
     cex=rep(0.1, ncol(x)), cex.axis=1.2, cex.lab=1.2,
     leg.txt=c("OBS", colnames(x), "95PPU"), verbose= TRUE, thr.shw=TRUE, 
     border=rep(NA, ncol(x)), bands.col=rep("lightcyan", ncol(x)), 
     bands.density=rep(NULL, ncol(x)), bands.angle=rep(45, ncol(x)), new=TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric, zoo, data.frame or matrix object with the observed streamflows for which the flow duration curve have to be computed. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object, and in that case, each column of \code{x} represent the time series measured in each gauging station, and the column names of \code{x} have to correspond to the ID of each station (starting by a letter). When \code{x} is a matrix or data.frame, the flow duration curve is computed for each column.
}
  \item{lband}{
numeric, zoo, data.frame or matrix object with the streamflows representing the the lower uncertainty bound of \code{x}, for which the flow duration curve have to be computed. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object. When \code{lband} is a matrix or data.frame, the flow duration curve is computed for each column.
}
  \item{uband}{
numeric, zoo, data.frame or matrix object with the streamflows representing the the upper uncertainty bound of \code{x}, for which the flow duration curve have to be computed. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object. When \code{uband} is a matrix or data.frame, the flow duration curve is computed for each column.
}
  \item{sim}{
OPTIONAL. \cr
numeric, zoo, data.frame or matrix object with the streamflows simulated for \code{x}, for which the flow duration curve have to be computed. \cr
Measurements at several gauging stations can be stored in a data.frame of matrix object. When \code{sim} is a matrix or data.frame, the flow duration curve is computed for each column.
}
\item{lQ.thr}{
numeric, low flows separation threshold. If this value is different from 'NA', a vertical line is drawn in this value, and all the values to the left of it are deemed low flows.
}
\item{hQ.thr}{
numeric, high flows separation threshold. If this value is different from 'NA', a vertical line is drawn in this value, and all the values to the right of it are deemed high flows
}
  \item{plot}{
logical. Indicates if the flow duration curve should be plotted or not.
}
  \item{log}{
character, indicates which axis has to be plotted with a logarithmic scale. Default value is \kbd{y}. 
}
  \item{main}{
See \code{\link[graphics]{plot}}. An overall title for the plot: see \code{\link[graphics]{title}}.
}
  \item{xlab}{
See \code{\link[graphics]{plot}}. A title for the x axis: see \code{\link[graphics]{title}}.
}
  \item{ylab}{
See \code{\link[graphics]{plot}}. A title for the y axis: see \code{\link[graphics]{title}}.
}
  \item{ylim}{
See \code{\link[graphics]{plot.default}}. The y limits of the plot.
}
  \item{col}{
See \code{\link[graphics]{plot.default}}. The colors for lines and points.  Multiple colors can be specified so that each point can be given its own color.  If there are fewer colors than points they are recycled in the standard fashion. Lines will all be plotted in the first colour specified.
}
  \item{pch}{
See \code{\link[graphics]{plot.default}}. A vector of plotting characters or symbols: see \code{\link[graphics]{points}}.
}
  \item{lwd}{
See \code{\link[graphics]{plot.default}}. The line width, see \code{\link[graphics]{par}}.
}
  \item{lty}{
See \code{\link[graphics]{plot.default}}. The line type, see \code{\link[graphics]{par}}. 
}
  \item{cex}{
See \code{\link[graphics]{plot.default}}. A numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default.  
This works as a multiple of \env{par("cex")}. 'NULL' and 'NA' are equivalent to '1.0'. Note that this does not affect annotation.
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
logical; if TRUE, progress messages are printed 
}
  \item{thr.shw}{
logical, indicating if the stremflow values corresponding to the user-defined thresholds \code{lQ.thr} and \code{hQ.thr} have to be shown in the plot.
}
  \item{border}{
See \code{\link[graphics]{polygon}}. The color to draw the border of the polygon with the uncertainty bounds. The default, 'NA', means to omit borders. 
}
  \item{bands.col}{
See \code{\link[graphics]{polygon}}. The color for filling the polygon. The default, 'NA', is to leave polygons unfilled, unless \code{bands.density} is specified. If \code{bands.density} is specified with a positive value this gives the color of the shading lines.
}
  \item{bands.density}{
See \code{\link[graphics]{polygon}}. The density of shading lines for the polygon with the uncertainty bounds, in lines per inch. The default value of 'NULL' means that no shading lines are drawn. A zero value of \code{bands.density} means no shading nor filling whereas negative values (and 'NA') suppress shading (and so allow color filling).
}
  \item{bands.angle}{
See \code{\link[graphics]{polygon}}. The slope of shading lines for the polygon with the uncertainty bounds, given as an angle in degrees (counter-clockwise). 
}
  \item{new}{
logical, if TRUE, a new plotting window is created. 
}
  \item{\dots}{
further arguments passed to or from other methods (to the plotting functions)
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\references{
\cite{Vogel, R., and N. M. Fennessey (1994), Flow duration curves I: A new interpretation and confidence intervals, ASCE, Journal of Water Resources Planning and Management, 120(4)}. \cr

\cite{Vogel, R., and N. Fennessey (1995), Flow duration curves II: A review of applications in water resources planning, Water Resources Bulletin, 31(6), 1029-1039, doi:10.1111/j.1752-1688.1995.tb03419.x}. \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
\note{
If you do not want to use logarithmic scale for the streamflow axis, you can do it by passing the \code{log=" "} to the \code{...} argument.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fdc}}
}
\examples{
## Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain) ##
data(OcaEnOnaQts)
q <- OcaEnOnaQts

# Creating a fictitious lower uncertainty band
lband <- q - min(q, na.rm=TRUE)

# Giving a fictitious upper uncertainty band
uband <- q + mean(q, na.rm=TRUE)

# Plotting the flow duration curve corresponding to 'q', with two uncertainty bounds
fdcu(q, lband, uband)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
