\name{drawxaxis}
\Rdversion{1.1}
\alias{drawxaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Customized 'x' axis
}
\description{
For a nicer ts plot, this function draws a customized x axis, with daily, monthly, or annual time marks and labels.
}
\usage{
drawxaxis(x, tick.tstep = "months", lab.tstep = "years", 
          lab.fmt, cex.axis=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series that will be plotted using the X axis that will be draw class(x) must be ts or zoo
}
  \item{tick.tstep}{
Character indicating the time step that have to be used for putting the ticks on the time axis. Valid values are: \kbd{days}, \kbd{months}, \kbd{years}.
}
  \item{lab.tstep}{
Character indicating the time step that have to be used for putting the labels on the time axis. Valid values are: \kbd{days}, \kbd{months}, \kbd{years}. 
}
  \item{lab.fmt}{
Character indicating with the format to be used for the label of the axis. See \code{format} in \code{\link[base]{as.Date}}. If not specified, it will try \kbd{"\%Y-\%m-\%d"} when \code{lab.tstep=="days"}, \kbd{"\%b"} when \code{lab.tstep=="month"}, and \kbd{"\%Y"} when \code{lab.tstep=="year"}. 
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
x <- window(SanMartinoPPts, end=as.Date("1935-12-31"))

## Plotting the daily ts without an 'x' axis
plot(x, xaxt = "n", col="blue" )

## Draws monthly ticks in the 'x' axis, but labels only in years
drawxaxis(x, tick.tstep="months", lab.tstep="years", xlab="Time") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
