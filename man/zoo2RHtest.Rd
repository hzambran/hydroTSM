%%File zoo2RHtest.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%%                                 http://www.rforge.net/hydroTSM/
%% Copyright 2008-2020 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{zoo2RHtest}
\Rdversion{1.1}
\alias{zoo2RHtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Zoo -> RHTest 
}
\description{
It creates the input file to the \var{'RHtest_dlyPrcp.r'} script,  used for testing the homogeneity of climatological time series (available at: \url{https://etccdi.pacificclimate.org/software.shtml})
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
%\url{http://ccma.seos.uvic.ca} % not available anynmore, reported by Gang Dong on March 12th, 2013
\url{https://etccdi.pacificclimate.org/software.shtml}
\url{https://etccdi.pacificclimate.org/RHtest/RHtestsV4_UserManual_10Dec2014.pdf}
%\url{http://css.escwa.org.lb/sdpd/1802/d2-1.pdf} % not available anynmore, rdetected on March 10th, 2020

% \url{http://www.cmc.org.ve/mediawiki/index.php?title=Preparando_los_datos}
%\url{http://cmc.org.ve/descargas/Cursos/CRRH/}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
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

\dontrun{
# From zoo to the input format required by 'FindU.dlyPrcp' function
zoo2RHtest(x=pcp.m, fname="pcp-monthly.txt", tstep.out="monthly", na="-999.0")

# Homogeneity analysis
FindU.dlyPrcp(InSeries="pcp-monthly.txt", output="pcp-monthly", MissingValueCode="-999.0", 
GUI=FALSE, pthr=0, Mq=10, p.lev=0.95, Iadj=10000)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
