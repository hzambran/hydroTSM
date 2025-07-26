%% File matrixplot.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2009-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{matrixplot}
\Rdversion{1.1}
\alias{matrixplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrixplot
}
\description{
Plots a color matrix, representing the values stored in \code{x}. \cr
Originally, it was thought to represent the amount of days with information per year in a set of gauging stations, but it can be used for plotting the information stored in any two dimensional matrix.
}
\usage{
matrixplot(x, ColorRamp="Days", ncolors = 70, main = "", 
           cuts,  cuts.dec=2, cuts.labels, 
           cuts.style=c("equal", "pretty", "fixed", "sd", "quantile", 
                        "kmeans", "bclust", "fisher"), 
           legend.cex=1.1,  legend.title="",  legend.title.cex=1.5, 
           legend.fontsize=15, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix to be plotted. Originally: \cr
-) Each column of \code{x} represent a different gauging station, and it stores the values measured on it \cr
-) Each row of \code{x} represent the years, and they stores the amount of days with information in each station
}
  \item{ColorRamp}{
Character or function defining a personalized color ramp for plotting the maps. \cr
Valid character values are in \kbd{c("Days", "Precipitation", "Temperature", "PCPAnomaly", "PCPAnomaly2" "TEMPAnomaly", "TEMPAnomaly2", "TEMPAnomaly3")}.
}
  \item{ncolors}{
numeric, indicating the number of color intervals that will be used for representing the information content of \code{x}.
}
  \item{main}{
Main title for the plot
}
\item{cuts}{
Numeric, indicating the values used to divide the range of 'x' in the legend of colours. If not provided, it is automatically selected as a function of 'lenght(ColorRamp)'.
}
\item{cuts.dec}{
Number of decimal places used to present the numbers that divide the range of 'x' in the legend of colours.
}
\item{cuts.labels}{
Character indicating the label to be used in the ccolour legend for each one of the values defined by 'cuts'. If not provided, as.character(cuts)' is used.
}
\item{cuts.style}{
character, indicating how to finding class intervals for continuous numerical variables for choosing colours to be used in the figure. See \code{\link[classInt]{classIntervals}}
}
\item{legend.cex}{
character expansion factor *relative* to current \code{par("cex")} used for the legend text.
}
\item{legend.title}{
text to be displayed above the legned of colours (e.g., showing the measurement units of the raster being displayed).
}
\item{legend.title.cex}{
expansion factor(s) for the legend title. Currently it is not used. See \code{legend.fontsize} instead.
}
\item{legend.fontsize}{
size of text (in points) used in the legend title (e.g., showing the measurement units of the raster being displayed).
}
  \item{\dots}{
further arguments passed to \code{\link[lattice]{levelplot}} function (\pkg{lattice} package) or from other methods
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
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
\note{
Adapted from a not available web page (http://www2.warwick.ac.uk/fac/sci/moac/currentstudents/peter_cock/r/matrix_contour/)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dwi}}, \code{\link[classInt]{classIntervals}}
}
\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)

# Selecting only the values up to Dec/1960
x <- window(SanMartinoPPts, end=as.Date("1960-12-31"))

## Daily zoo to monthly zoo
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

# Creating a data.frame with monthly values per year in each column
M <- matrix(m, ncol=12, byrow=TRUE)
colnames(M) <- month.abb
rownames(M) <- unique(format(time(m), "\%Y"))

# Plotting the monthly precipitation values from 1921 to 1960.
# Useful for identifying dry/wet months
matrixplot(M, ColorRamp="Precipitation", 
           main="Monthly precipitation at San Martino st., [mm/month]")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphs}
