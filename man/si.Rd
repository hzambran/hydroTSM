%%File si.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%%                                 http://www.rforge.net/hydroTSM/ ; 
%% Copyright 2008-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{si}
\Rdversion{1.1}
\alias{si}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
si
}
\description{
Function to draw a si based on precipitation and temperature data. 
}
\usage{
  si(x, na.rm=TRUE, from=start(x), to=end(x), date.fmt="%Y-%m-%d", start.month=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo object with daily or subdaily precipitation data.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly values  are computed considering only those values different from NA \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month, the resulting average monthly value is NA .
}
  \item{from}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the starting date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
  \item{to}{
OPTIONAL, used for extracting a subset of values. \cr
Character indicating the ending date for the values to be extracted. It must be provided in the format specified by \code{date.fmt}.
}
  \item{date.fmt}{
Character indicating the format in which the dates are stored in \var{dates}, \var{from} and \var{to}. See \code{format} in \code{\link[base]{as.Date}}. \cr
ONLY required when \code{class(dates)=="factor"} or \code{class(dates)=="numeric"}.
}
  \item{start.month}{
[OPTIONAL]. Only used when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represents months in [January, December]. By default \code{start.month=1}.
}
}
\details{
This index can theoretically vary from 0 (when all months have the same rainfall) to 1.83 (when all the rainfall ocurrs in a single month).
A qualitative classification of degrees of seasonality is the following:
-----------------------------------------------------------------------------
 si values   |                    Rainfall regime                            
-----------------------------------------------------------------------------
     <= 0.19  | Very equable                                                 
0.20 - 0.39  | Equable but with a definite wetter season                     
0.40 - 0.59  | Rather seasonal with a short drier season                     
0.60 - 0.79  | Seasonal                                                      
0.80 - 0.99  | Markedly seasonal with a long drier season                    
1.00 - 1.19  | Most rain in 3 months or less                                 
    >= 1.20  | Extreme, almost all rain in 1-2 months                        
}
\value{
numeric with the seasonality index
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\cite{Walsh, R. and Lawler, D. (1981). Rainfall seasonality: Description, spatial patterns and change through time (British Isles, Africa). Weather, 36(7), 201-208. doi:10.1002/j.1477-8696.1981.tb05400.x}.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail}
}
%%\note{
%%
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{subdaily2daily}}
}
\examples{
######################
## Ex1: Loading the DAILY precipitation, maximum and minimum air temperature at 
##      station Maquehue Temuco Ad (Chile)
data(MaquehueTemuco)
pcp <- MaquehueTemuco[, 1]
tmx <- MaquehueTemuco[, 2]
tmn <- MaquehueTemuco[, 3]

## Plotting a full si
m <- si(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE, 
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)

## Plotting a si with uncertainty bands around mean values, 
## but with no labels for tmx and tmn
m <- si(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE, tmx.labels=FALSE, tmn.labels=FALSE, 
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)

## Plotting a si with uncertainty bands around mean values, but with no labels for 
##  tmx, tmn and pcp
m <- si(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE, 
                pcp.labels=FALSE, tmean.labels=FALSE, tmx.labels=FALSE, tmn.labels=FALSE, 
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)

## Plotting a si with no uncertainty bands around mean values
m <- si(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE, plot.pcp.probs=FALSE, plot.temp.probs=FALSE, 
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)

## Plotting the most basic si: only mean values of precipiation and air temperature
m <- si(pcp=pcp, tmean=0.5*(tmn+tmx), na.rm=TRUE, plot.pcp.probs=FALSE, 
                plot.temp.probs=FALSE, main="Maquehue Temuco Ad (Chile)", 
                lat=-38.770, lon=-72.637)


## Plotting a full si, starting in April (start.month=4) instead of January (start.month=1),
## to better represent the hydrological year in Chile (South America)
m <- si(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=TRUE, 
                start.month=4, temp.labels.dx=c(rep(-0.2,4), rep(0.2,6),rep(-0.2,2)),
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)


## Plotting a full si with monthly data
pcp.m <- daily2monthly(pcp, FUN=sum)
tmx.m <- daily2monthly(tmx, FUN=mean)
tmn.m <- daily2monthly(tmn, FUN=mean)
m <- si(pcp=pcp.m, tmx=tmx.m, tmn=tmn.m, na.rm=TRUE, 
                main="Maquehue Temuco Ad (Chile)", lat=-38.770, lon=-72.637)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
