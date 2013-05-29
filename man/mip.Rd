%%File mip.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{mip}
\Rdversion{1.1}
\alias{mip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Months in Period
}
\description{
 Given any starting and ending dates, it generates: \cr      
1) a vector of class 'Date' with all the months between the two dates (both of them included), OR \cr
2) the amount of months between the two dates 
}
\usage{
mip(from, to, date.fmt = "\%Y-\%m-\%d", out.type = "seq")
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
Character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}.
}
  \item{out.type}{
character indicating the type of result that is given by this function. Valid values are: \cr
-) \kbd{seq} : a vectorial sequence with all the months within the given year \cr
-) \kbd{nmbr}: the number of days in the vectorial sequence with all the months within the given year
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Depending on the value of \code{out.type}, it returns: \cr
1) a vector of class Date with all the months between \code{from} and \code{to} (both of them included), OR \cr
2) a single numeric value with the amount of months between the two dates. 
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
\code{\link{dip}}, \code{\link{diy}}, \code{\link{yip}}
}
\examples{
# Sequence of monthly dates between "1961-01-01" and "1961-12-31" ##
mip("1961-01-01", "1961-12-31")

## Computing the number of months between "1961-01-01" and "1965-06-30", 
## with the date format  "\%d-\%m-\%Y" ##
mip("01-01-1961", "30-06-1965", date.fmt= "\%d-\%m-\%Y", out.type = "nmbr")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
