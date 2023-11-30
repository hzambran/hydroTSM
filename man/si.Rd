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
  \item{main}{
Character representing the main title of the plot. 
}
  \item{pcp.label}{
Character used in the legend to represent the monthly average precipitation (mostly thought for languages different from English). 
}
  \item{tmean.label}{
Character used in the legend to represent the monthly average temperature (mostly thought for languages different from English). 
}
  \item{start.month}{
[OPTIONAL]. Only used when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represents months in [January, December]. By default \code{start.month=1}.
}
  \item{pcp.solid.thr}{
[OPTIONAL]. Only used when using (sub)daily precipitation and temperature are gives as input data. \cr
numeric, indicating the temperature, in degrees Celsius, used to discriminate between solid and liquid precipitation. \cr

When daily \code{tmean <= pcp.solid.thr} the precipitation for that day is considered as solid precipitation.
}
  \item{pcp.ylim}{
[OPTIONAL] numeric of length 2 with the the range used for the precipitation axis. The second value should be larger than the first one.
}
  \item{temp.ylim}{
[OPTIONAL] numeric of length 2 with the the range used for the secondary temperature axis. The second value should be larger than the first one.
}
  \item{pcp.col}{
Color used in the legend to represent the monthly average precipitation. 
}
  \item{pcp.solid.col}{
Color used in the legend to represent the monthly average solid precipitation. 
}
  \item{tmean.col}{
Color used in the legend to represent the monthly average temperature. 
}
  \item{tmn.col}{
Color used in the legend to represent the monthly minimum temperature. 
}
  \item{tmx.col}{
Color used in the legend to represent the monthly maximum temperature. 
}
  \item{pcp.labels}{
logical. Should monthly precipitation values to be shown above the bars?. By default \code{pcp.labels=TRUE}.
}
  \item{tmean.labels}{
logical. Should monthly mean temperature values to be shown above the lines?. By default \code{tmean.labels=TRUE}.
}
  \item{tmx.labels}{
logical. Should monthly maximum temperature values to be shown above the lines?. By default \code{tmx.labels=TRUE}.
}
  \item{tmn.labels}{
logical. Should monthly minimum temperature values to be shown above the lines?. By default \code{tmn.labels=TRUE}.
} 
  \item{pcp.labels.cex}{
numeric giving the amount by which plotting characters used to show the numeric values of monthly precipitation values are scaled relative to the default. \cr
It is only used when \code{pcp.labels=TRUE}.
}
  \item{temp.labels.cex}{
numeric giving the amount by which plotting characters used to show the numeric values of monthly air temperature values (mean, maximum, minimum) are scaled relative to the default.   \cr
It is only used when \code{tmean.labels=TRUE} or \code{tmx.labels=TRUE} or \code{tmn.labels=TRUE}.
}
  \item{pcp.labels.dx}{
numeric of length 12 giving the amount of horizontal coordinate positions that have to be used to shift the labels of monthly precipitation values. \cr
It is only used when \code{pcp.labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{pcp.labels.dy}{
numeric of length 12 giving the amount of vertical coordinate positions that have to be used to shift the labels of monthly precipitation values. \cr
It is only used when \code{pcp.labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{temp.labels.dx}{
numeric of length 12 giving the amount of horizontal coordinate positions that have to be used to shift the labels of monthly air temperature values (mean, maximum, minimum). \cr
It is only used when \code{tmean.labels=TRUE} or \code{tmx.labels=TRUE} or \code{tmn.labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{temp.labels.dy}{
numeric of length 12 giving the amount of vertical coordinate positions that have to be used to shift the labels of monthly air temperature values (mean, maximum, minimum). \cr
It is only used when \code{tmean.labels=TRUE} or \code{tmx.labels=TRUE} or \code{tmn.labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{plot.pcp.probs}{
logical used to decide whether to show uncertainty values around the monthly mean precipitation values. By default \code{plot.pcp.probs=TRUE}. \cr 
When \code{plot.pcp.probs=TRUE} the \code{pcp.probs} argument is used to define the values of the lower an upper uncertainty bounds.
} 
  \item{pcp.probs}{
numeric of length 2. It defines the quantile values used to compute the lower an upper uncertainty bounds for each one of the 12 monthly precipitation values.\cr 
By default \code{pcp.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly precipitation values. 
If \code{pcp} is a (sub)daily zoo object, it is first aggregated into monthly values using \code{mean}, and then the \code{pcp.probs} quantiles are computed over all the monthly values belonging to a calendar month.
} 
  \item{plot.temp.probs}{
logical used to decide whether to show uncertainty values around the monthly mean temperature values. By default \code{plot.temp.probs=TRUE}. \cr 
When \code{plot.temp.probs=TRUE} the \code{temp.probs} argument is used to define the values of the lower an upper uncertainty bounds.
} 
  \item{temp.probs}{
numeric of length 2. It is used to define quantile values used to compute the lower an upper uncertainty bounds for each one of the 12 monthly mean temperature values.\cr 
If \code{tmx} and \code{tmn} are provided, then \code{temp.probs} are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly maximum/minimum temperature values. \cr
By default \code{temp.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly mean(maximum/minimum) values. 
If \code{tmx/tmn} is provided and is a (sub)daily zoo object, it is first aggregated into monthly values using \code{mean}, and then the \code{temp.probs} quantiles are computed over all the monthly values belonging to a calendar month.
} 
  \item{temp.probs.col}{
character of length 3, with the colors used to for plotting the uncertainty bands around the average monthly values of the minimum, mean and maximum air temperature, respectively. \cr
If \code{tmx} and \code{tmn} are not provided by the user, the second element of \code{temp.probs.col} will still be used to define the color of the  uncertainty band around the mean monthly values of air temperature.
} 
  \item{temp.probs.alpha}{
numeric of length 1, with the factor used to modify the opacity of \code{temp.probs.col}. Typically in [0,1], with 0 indicating a completely transparent colour and 1 indicating no transparency.
} 
  \item{lat}{
[OPTIONAL] numeric or character used to show the latitude for which the si was plotted for.
} 
  \item{lon}{
[OPTIONAL] numeric or character used to show the longitude for which the si was plotted for.
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
\note{
If the output si present some mixed or not legible characters, you might try resizing the graphical window and run si again with the new size, until you get the si in the way you want to.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{monthlyfunction}}
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
