%% File shiftyears.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2023-2025 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{shiftyears}
\Rdversion{1.1}
\alias{shiftyears}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shiftyears
}
\description{
It shifts years backwards for correct computation of annual values when the hydrological year does not start in january. Originally created only for internal usage in this pacakge. 
}
\usage{
shiftyears(ltime, lstart.month)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ltime}{
vector with the date/times of each element of a zoo object.
}
  \item{lstart.month}{
numeric in [1,..,12], representing the starting month to be used in the computation of annual values. By default \code{lstart.month=1} (Jan).}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Integer vector with the shifted years corresponding to each elemen of \code{x}.
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
\code{\link{climograph}}, \code{\link{cmv}}, \code{\link{daily2annual}}
}
\examples{
# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x <- SanMartinoPPts

# Subsetting 'x' to its first three months (Jan/1921 - Mar/1921)
x <- window(x, end="1921-12-31")

# Starting in January, it means no shift
shiftyears(ltime=time(x), lstart.month=1)

# Starting in April:
shiftyears(ltime=time(x), lstart.month=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
