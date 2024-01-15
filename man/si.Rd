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
Seasonality Index
}
\description{
Function to compute the seasonality index defined by Walsh and Lawler (1981) to classify the 
}
\usage{
  si(x, na.rm=TRUE, from=start(x), to=end(x), date.fmt="\%Y-\%m-\%d", start.month=1)
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
The seasonality index is computed as following:\cr

               si = (1/R) *sum(i=1, i=12, abs(xi - R/12) ) \cr            
where: \cr                                                   
 -) xi: mean monthly precipitation for month i \cr
 -) R: mean annual precipitation               \cr             

This index can theoretically vary from 0 (when all months have the same rainfall) to 1.83 (when all the rainfall ocurrs in a single month).
A qualitative classification of degrees of seasonality is the following: \cr
-------------------------------------------------------- \cr
 si values   |                    Rainfall regime        \cr                    
-------------------------------------------------------- \cr
     <= 0.19 | Very equable                              \cr                    
0.20 - 0.39  | Equable but with a definite wetter season \cr                    
0.40 - 0.59  | Rather seasonal with a short drier season \cr                    
0.60 - 0.79  | Seasonal                                  \cr                    
0.80 - 0.99  | Markedly seasonal with a long drier season\cr                    
1.00 - 1.19  | Most rain in 3 months or less             \cr                                  
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

############################
## Ex 1: Seasonality index for a rain gauge with equable precipitation , 
##       but with a definite wetter season 

## Loading daily precipitation data at the station San Martino di Castrozza, 
## Trento Province, Italy, from 01/Jan/1921 to 31/Dec/1990.
data(SanMartinoPPts)
x <- SanMartinoPPts

## Amount of years in 'x' (needed for computations)
( nyears <- yip(from=start(x), to=end(x), out.type="nmbr" ) )


## Boxplot of monthly values, to look at the seasonal cycle

## Daily to Monthly
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

## Mean monthly values at the station
monthlyfunction(m, FUN=sum, na.rm=TRUE) / nyears

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "\%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values of precipitation
boxplot( coredata(m) ~ months, col="lightblue", 
         main="Monthly precipitation, [mm]", ylab="P, [mm]")

# computing seasonality index
( si(x) )

############################
## Ex 2: Seasonality index for a rain gauge with markedly seasonal regime 
##       with a long dry season

## Loading daily precipitation data at the station Cauquenes en El Arrayan, 
## Maule Region, Chile, from 01/Jan/1979 to 31/Dec/2020.
data(Cauquenes7336001)
x <- Cauquenes7336001[, 1] # P is the first column

## Boxplot of monthly values, to look at the seasonal cycle

## Daily to Monthly
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

## Mean monthly values at the station
monthlyfunction(m, FUN=sum, na.rm=TRUE) / nyears

## Vector with the three-letter abbreviations of the month names
cmonth <- format(time(m), "\%b")

## Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

## Boxplot of the monthly values of precipitation
boxplot( coredata(m) ~ months, col="lightblue", 
         main="Monthly precipitation, [mm]", ylab="P, [mm]")

# computing seasonality index
( si(x) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
