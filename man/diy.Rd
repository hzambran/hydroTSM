%% File diy.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{diy}
\Rdversion{1.1}
\alias{diy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Days in Year
}
\description{
 Given a single numeric value representing a year, it generates: \cr           
1) a vector of dates with all the days within the year, OR \cr
2) the amount of days in the specified year  
}
\usage{
diy(year, out.type = "seq")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
numeric, the year for which the sequence of days will be generated
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
-) \kbd{seq} : a vectorial sequence with all the days within the given year \cr
-) \kbd{nmbr}: the number of days in the vectorial sequence with all the days within the given year
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hip}}, \code{\link{dip}}, \code{\link{mip}}, \code{\link{yip}}
}
\examples{
## Sequence of daily dates for the year 1961
diy(1961)

## Computing the number of days between in 1961
diy(1961, out.type = "nmbr")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
