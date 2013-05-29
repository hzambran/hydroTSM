%%File yip.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{yip}
\Rdversion{1.1}
\alias{yip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Years in Period
}
\description{
 Given any starting and ending dates, it generates: \cr       
1) a vector of class Date with all the years between the two dates (both of them included), OR \cr
2) the amount of years between the two dates 
}
\usage{
yip(from, to, date.fmt = "\%Y-\%m-\%d", out.type = "seq")
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
character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}.
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
-) \kbd{seq}  => a vectorial sequence with all the years within the given dates. \cr
-) \kbd{nmbr} => the number of years within the given dates.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
Depending on the value of \code{out.type}, it returns: \cr
1) \kbd{seq} : a vector of class Date with all the years between \code{from} and \code{to} (both of them included), OR \cr
2) \kbd{nmbr}: a single numeric value with the amount of years between the two dates. 
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
\code{\link{dip}}, \code{\link{diy}}, \code{\link{mip}}
}
\examples{
# Sequence of monthly dates between "1961-01-01" and "1961-12-31"
yip("1961-01-01", "1961-12-31")

## Computing the number of years between 1961 and 1975, 
## by using "\%d-\%m-\%Y" as date format   ##
yip("01-01-1961", "01-01-1975", date.fmt= "\%d-\%m-\%Y", out.type = "nmbr")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
