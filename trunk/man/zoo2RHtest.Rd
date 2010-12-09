\name{zoo2RHtest}
\Rdversion{1.1}
\alias{zoo2RHtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Zoo -> RHTest 
}
\description{
It creates the input file to the \var{'RHtest_dlyPrcp.r'} script,  used for testing the homogeneity of climatological time series (available at: \url{http://ccma.seos.uvic.ca})
}
\usage{
zoo2RHtest(x, fname="pcp.txt", tstep.out="daily", dec=".", na="-999.0")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series that will be written. class(x) must be a zoo object
}
  \item{fname}{
Character, with the filename of the precipitation time series 
}
  \item{tstep.out}{
Character indicating the time step that have to be used for writing \code{x} into the output file
}
  \item{dec}{
the string to use for decimal points in numeric or complex columns: must be a single character.
}
  \item{na}{
character to be used for representing the missing values in the data
}
}
%%\details{
%%
%%}
%%\value{
%%
%%}
\references{
\url{http://ccma.seos.uvic.ca}
}
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
x <- SanMartinoPPts

#Getting the monthly ts
pcp.m <- daily2monthly(x, FUN=sum, na.rm=FALSE)

# From zoo to the input format required by 'FindU.dlyPrcp' function
zoo2RHtest(x=pcp.m, fname="pcp-monthly.txt", tstep.out="monthly", na="-999.0")

\dontrun{
# Homogeneity analysis
FindU.dlyPrcp(InSeries="pcp-monthly.txt", output="pcp-monthly", MissingValueCode="-999.0", 
GUI=FALSE, pthr=0, Mq=10, p.lev=0.95, Iadj=10000)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
