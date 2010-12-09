\name{rm1stchar}
\Rdversion{1.1}
\alias{rm1stchar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove First Character(s) 
}
\description{
Deletes the first \code{n} character(s) of a character object.
}
\usage{
rm1stchar(x, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character, e.g, each element may represent the name of a single gauging station.
}
  \item{n}{
numeric, indicating the number of characters that have to be removed from the beginning of \code{x}
}
}
%%\details{

%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
character object of the same length as \code{x}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{substr}}
}
\examples{
## Loading the monthly time series of precipitation within the Ebro River basin.
data(EbroPPtsMonthly)

# Getting the name of each gauging station.
names <- colnames(EbroPPtsMonthly)

# Removing the initial letter 'P' of the name of each gauging station.
rm1stchar(names)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
