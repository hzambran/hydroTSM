\name{drawxaxis}
\Rdversion{1.1}
\alias{drawxaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Customized 'x' axis
}
\description{
For a nice time series plot, this function draws a customized x axis, with annual, monthly, daily and sub-daily time marks and labels.
}
\usage{
drawxaxis(x, tick.tstep = "auto", lab.tstep = "auto", 
          lab.fmt=NULL, cex.axis=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series that will be plotted using the X axis that will be draw class(x) must be ts or zoo
}
  \item{tick.tstep}{
Character indicating the time step that have to be used for putting the ticks on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
  \item{lab.tstep}{
Character indicating the time step that have to be used for putting the labels on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
  \item{lab.fmt}{
Character indicating the format to be used for the label of the axis. See \code{format} in \code{\link[base]{as.Date}}. If not specified (\code{lab.fmt=NULL}), it will try to use:
-) \kbd{"\%Y-\%m-\%d"} when \code{lab.tstep=="days"}, \cr
-) \kbd{"\%b-\%Y"} when \code{lab.tstep=="year"} or \code{lab.tstep=="month"}. 
}
  \item{cex.axis}{
magnification of axis annotation relative to \code{cex} (See \code{\link[graphics]{par}}).
}
  \item{\dots}{
further arguments passed to the \code{Axis} function or from other methods.
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
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## Loading the SanMartino precipitation data
data(SanMartinoPPts)
x <- window(SanMartinoPPts, end=as.Date("1930-12-31"))

## Plotting the daily ts only, and then automatic 'x' axis
plot(x, xaxt = "n", xlab="Time")
drawxaxis(x) 

## Plotting the daily ts only, and then monthly ticks in the 'x' axis, 
## with annual labels.
plot(x, xaxt = "n", xlab="Time")
drawxaxis(x, tick.tstep="months", lab.tstep="years") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
