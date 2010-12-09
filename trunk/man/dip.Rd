\name{dip}
\Rdversion{1.1}
\alias{dip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Days in Period
}
\description{
Given any starting and ending dates, it generates:  \cr
1) a vector of class Date with all the days between the two dates, OR \cr
2) the amount of days between the two dates 
}
\usage{
dip(from, to, date.fmt = "\%Y-\%m-\%d", out.type = "seq")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{
Character indicating the starting date for creating the sequence. It has to be in the format indicated by \code{date.fmt}.
}
  \item{to}{
Character indicating the ending date for creating the sequence. It has to be in the format indicated by \code{date.fmt}.
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \var{dates}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
1) \kbd{seq} : a vector of class Date with all the days between the two dates, OR \cr
2) \kbd{nmbr}: a single numeric value with the amount of days between the two dates.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Depending on the value of \code{out.type}, it returns: \cr
1) a vector of class Date with all the days between the two dates, OR \cr
2) the amount of days between the two dates 
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
\code{\link{mip}}, \code{\link{yip}}, \code{\link{diy}}
}
\examples{
## Sequence of daily dates between "1961-01-01" and "1961-12-31" ##
dip("1961-01-01", "1961-12-31")

## Number of days between "1961-01-01" and "1965-06-30", 
## but using "\%d-\%m-\%Y" as date format.
dip("01-01-1961", "30-06-1965", date.fmt= "\%d-\%m-\%Y", out.type = "nmbr")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
