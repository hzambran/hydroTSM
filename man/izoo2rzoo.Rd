\name{izoo2rzoo}
\Rdversion{1.1}
\alias{izoo2rzoo}
\alias{izoo2rzoo.default}
\alias{izoo2rzoo.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Irregular Zoo -> Regular Zoo
}
\description{
It takes an irregular (with missing dates) zoo object and convert it into a regularly spaced object within the time period defined by \code{from} and \code{to}, by filling the missing values with 'NA'. 
}
\usage{
izoo2rzoo(x, ...)

\method{izoo2rzoo}{default}(x, from= start(x), to= end(x), date.fmt= "\%Y-\%m-\%d", tstep= "days", ...)
     
\method{izoo2rzoo}{zoo}(x, from= start(x), to= end(x), date.fmt= "\%Y-\%m-\%d", tstep= "days", ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series of zoo class (very likely read with some user-defined procedure, and with some missing values for particular days/months/years)
}
  \item{from}{
Character indicating the starting date for creating the sequence. It has to be in the format indicated by \code{date.fmt}. The default value corresponds to the date of the first element of \code{x}.
}
  \item{to}{
Character indicating the ending date for creating the sequence. It has to be in the format indicated by \code{date.fmt}. The default value corresponds to the date of the last element of \code{x}.
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. 
}
  \item{tstep}{
character, indicating the time step used for sampling \code{x}, e.g., \kbd{days}, \kbd{months}, \kbd{years}.
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\details{
If the full time period of \code{x} is a subset of the time period defined by \code{from} and \code{to}, the time period of the resulting zoo is the one defined by \code{from} and \code{to}, assigning 'NA' to all the dates in which \code{x} do not have a value.
}
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

\seealso{
\code{\link{vector2zoo}}
}
\examples{
x <- 1:9

## February 29th is missing:
dates <- c("1964-02-25", "1964-02-26", "1964-02-27", "1964-02-28", "1964-03-01", 
           "1964-03-02", "1964-03-03", "1964-03-04", "1964-03-05")

## From 'character' to 'Date' class
dates <- as.Date(dates)

## From 'numeric' to 'zoo' class
x <- vector2zoo(x, dates) #Feb 29th is still missing

## Feb 29th is added to 'y' with a missing value
y <- izoo2rzoo(x, from=dates[1], to=dates[length(dates)])

## Creating a regular sequence from February 1st to the end of March,
## assigning 'NA' to the days in which 'x' do not have a value.
y <- izoo2rzoo(x, from="1964-02-01", to="1964-03-31")

## Working with a zoo matrix:
Y <- cbind(x,x)

rY <- izoo2rzoo(Y, from="1964-02-25", to="1964-03-05")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
