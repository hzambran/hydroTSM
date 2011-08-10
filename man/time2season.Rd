\name{time2season}
\Rdversion{1.1}
\alias{time2season}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time character -> Seasonal character
}
\description{
This function transforms a character vector of dates into a character vector of seasons (summer, winter, autumn, spring), depending on the value of \code{type}:  \cr

When \code{type=default}
-) winter = DJF: December, January, February   \cr              
-) spring = MAM: March, April, May             \cr             
-) summer = JJA: June, July, August            \cr            
-) autumn = SON: September, October, November  \cr

When \code{type=FrenchPolynesia}
-) winter = DJFM: December, January, February, March  \cr              
-) spring = AM  : April, May                   \cr             
-) summer = JJA : June, July, August           \cr            
-) autumn = SON : September, October, November \cr
}
\usage{
time2season(x, out.fmt = "months", type="default")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector with the dates that have to be transformed. class(x) must be Date
}
  \item{out.fmt}{
character, indicating the format of the output seasons. Possible values are: \cr
-) \kbd{seasons} =>  c("winter", "spring",  "summer", autumn")  \cr
-) \kbd{months}  =>  c("DJF", "MAM",  "JJA", SON") or c("DJFM", "AM",  "JJA", SON")
}
  \item{type}{
character, indicating which weather seasons will be used for computing the output. Possible values are: \cr
-) \kbd{default}          =>  "winter"= Dec, Jan, Feb;      "spring"= Mar, Apr, May;  "summer"=Jun, Jul, Aug;  "autumn"= Sep, Oct, Nov  \cr
-) \kbd{FrenchPolynesia}  =>  "winter"= Dec, Jan, Feb, Mar; "spring"= Apr, May;       "summer"=Jun, Jul, Aug;  "autumn"= Sep, Oct, Nov 
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
character vector with the weather season to which each date in \code{x} belongs
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mauricio.zambrano@ing.unitn.it}
}
\note{
  Weather seasons corresponding to French Polynesia were defined following a comment from Lydie Sichoix
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dm2seasonal}}, \code{\link{seasonalfunction}}, \code{\link{extractzoo}}, \code{\link{dip}}, \code{\link{mip}}
}
\examples{
## Sequence of daily dates between "1961-01-01" and "1961-12-31"
t <- dip("1961-01-01", "1961-12-31")
time2season(t)

## Sequence of monthly dates between "1961-01-01" and "1961-12-31"
t <- mip("1961-01-01", "1961-12-31")
time2season(t)
time2season(t, out.fmt="seasons")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
