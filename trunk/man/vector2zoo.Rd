\name{vector2zoo}
\Rdversion{1.1}
\alias{vector2zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vector -> Zoo
}
\description{
Transform a numeric vector and its corresponding dates into a zoo object.
}
\usage{
vector2zoo(x, dates, date.fmt = "\%Y-\%m-\%d")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric vector
}
  \item{dates}{
character, factor or Date object with the dates corresponding to each element of \code{x}. Valid object class for \code{dates} are: \code{character, factor, Date}
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
%%  ~Describe the value returned
a zoo object, with the same number of elements as \code{x}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
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

\seealso{
\code{\link{izoo2rzoo}}, \code{\link{dip}}, \code{\link{mip}}, \code{\link{yip}}
}
\examples{
# Generating a numeric variable (e.g., read from the outputs of an hydrological model)
x <- 1:31

# Generating the dates corresponding to the previous outputs
dates <- dip("1961-01-01", "1961-01-31")

## Transforming from 'numeric' to 'zoo' class
x <- vector2zoo(x, dates)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
