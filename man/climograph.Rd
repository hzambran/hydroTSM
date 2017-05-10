%%File climograph.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{climograph}
\Rdversion{1.1}
\alias{climograph}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Climograph
}
\description{
Function to draw a climograph based on precipitation and temperature data. 
}
\usage{
climograph(pcp, tmean, tmx, tmn, na.rm=TRUE,  
           from, to, date.fmt="\%Y-\%m-\%d", 
           main="Climograph", 
           pcp.label="Precipitation, [mm]", 
           tmean.label="Temperature, [\U00B0 C]",
           pcp.col="lightblue", 
           tmean.col="red")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pcp}{
variable of type zoo with monthly, daily or subdaily precipitation data.
}
  \item{tmean}{
variable of type 'zoo' with monthly, daily or subdaily mean temperature data.
}
  \item{tmx}{
variable of type 'zoo' with monthly, daily or subdaily maximum temperature data. \cr
ONLY used (togheter with \code{tmn}) when \code{tmean}' is missing.
}
  \item{tmn}{
variable of type 'zoo' with monthly, daily or subdaily minimum temperature data. 
ONLY used (togheter with \code{tmx}) when \code{tmean} is missing.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the resulting average monthly value is NA .
}
  \item{main}{
Character representing the main title of the plot. 
}
  \item{pcp.label}{
Character used in the legend to represent the monthly average precipitation (mostly thought for languages different from English). 
}
  \item{tmean.label}{
Character used in the legend to represent the monthly average temperature (mostly thought for languages different from English). 
}
  \item{pcp.col}{
Color used in the legend to represent the monthly average precipitation. 
}
  \item{tmean.col}{
Color used in the legend to represent the monthly average temperature. 
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
\code{\link{monthlyfunction}}
}
\examples{
######################
## Ex1: Loading the DAILY precipitation, maximum and minimum air temperature at station Maquehue Temuco Ad (Chile)
data(MaquehueTemuco)
pcp <- MaquehueTemuco[, 1]
tmx <- MaquehueTemuco[, 2]
tmn <- MaquehueTemuco[, 3]

## Plotting the climograph
m <- climograph(x, pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
