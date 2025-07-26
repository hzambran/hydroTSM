%% File baseflow.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 http://www.rforge.net/hydroTSM/ ; 
%%                                 https://cran.r-project.org/package=hydroTSM
%% Copyright 2023-2025 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{baseflow}
\Rdversion{1.1}
\alias{baseflow}
\alias{baseflow.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Baseflow
 }
\description{
Given a complete (without missing values) series of streamflow, this function computes the baseflow using the filter proposed by Arnold and Allen (1999). 
}
\usage{
baseflow(x, ...)

\method{baseflow}{zoo}(x, beta=0.925, from=start(x), to=end(x),  n.pass=3L,
         pass.start=c("forward", "backward"), date.fmt, tz, 
         na.fill=c("none", "linear", "spline"), out.type=c("last", "all"), 
         plot=TRUE, xcol="black", bfcol=c("blue", "darkcyan", "darkorange3"),
         pch=15, cex=0.3, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo or numeric object with streamflow records. The suggested time frequency should be hourly or daily, but the algorithm will work with any time frequency.
}
  \item{beta}{
numeric representing the filter parameter. Default value is 0.925 as recommended by Arnold and Allen (1999)
}
  \item{from}{
Character indicating the starting date for subsetting \code{x}. It has to be in the format indicated by \code{date.fmt}. \cr
The default value corresponds to the date of the first element of \code{x}.
}
  \item{to}{
Character indicating the ending date for subsetting \code{x}. It has to be in the format indicated by \code{date.fmt}. \cr
The default value corresponds to the date of the last element of \code{x}.
}
  \item{n.pass}{
Integer indicating the number times the filter will be passed over the streamflow data stored in \code{x}. Default is 3.
}
  \item{pass.start}{
Character indicating the direction of the first pass of the filter. Subsequent passes alternate direction (e.g., forward-backward). Valid values are: \cr
-) \kbd{forward}  => the first pass of the filter is applied forward \cr
-) \kbd{backward} => the first pass of the filter is applied backward 
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \sQuote{Details} section in \code{\link[base]{strptime}}.
By default, \code{date.fmt} is missing, and it is automatically set to \kbd{\%Y-\%m-\%d} when \code{time(x)} is \code{Date} object, and set to \kbd{\%Y-\%m-\%d \%H:\%M:\%S} when \code{x} is a sub-daily zoo object.
}
  \item{tz}{
character, with the specification of the time zone used for \code{from}, \code{to}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr

If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr

This argument can be used when working with sub-daily zoo objects to force using  time zones other than the local time zone for \code{from} and \code{to}. It should be used with caution, being well aware of the time zone of the data. See examples.
}
  \item{na.fill}{Character indicating how to fill any NA present in \code{x}. Valid values are: \cr
-) \kbd{remove}      => NAs are not plotted                                                         \cr
-) \kbd{linear}      => NAs are removed by linear interpolation, using \code{\link[zoo]{na.approx}} \cr 
-) \kbd{spline}      => NAs are removed by spline interpolation, using \code{\link[zoo]{na.spline}}
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
-) \kbd{last} => only the baseflow computed after the third pass of the filter is returned. \cr
-) \kbd{all}  => the 3 baseflows computed after each pass of the filter are returned in a matrix or zoo object.
}
  \item{plot}{
logical. Indicates if the baseflow should be plotted or not. If plotted, the original \code{x} values are plotted as well.
}
  \item{xcol}{
character, representing the color to be used for ploting the streamflow time series. Only used when \code{plot=TRUE}.
}
  \item{bfcol}{
character of lenght 3, representing the color(s) to be used for ploting the baseflow time series. The first, second and third element are used to represent the baseflow after the third, second and first pass of the filter, respectively. Only used when \code{plot=TRUE}.
}
  \item{pch}{
numeric, representing the symbols used for ploting the streamflow time series (both, the original series and the baseflow). Only used when \code{plot=TRUE}.
}
  \item{cex}{
a numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default. This works as a multiple of \code{par("cex")}. See \code{\link[graphics]{plot.default}}. Only used when \code{plot=TRUE}.
}
  \item{\dots}{
further arguments passed to or from other methods. Not used yet.
}
}
 \details{
Although most procedures to separate baseflow from total streamflow are based on physical reasoning, some elements of all separation techniques are subjective.

The digital filter technique (Nathan and McMahon, 1990) implemented in this function was originally proposed by Lyne and Hollick (1979) for signal analysis and processing. Although this technique has no true physical meaning, it is objective and reproducible. 

The equation of the filter is: 

q(t) = Beta*q(t-1) + [ (1+Beta)/2 ]*[ Q(t) - Q(t-1) ]

where q(t) is the filtered surface runoff (quick response) at the time step t (one day), Q is the original streamflow, and Beta is the filter parameter (Beta=0.925). The value Beta=0.925 was obtained by Nathan and McMahon (1990) and Arnold et al. (1995) to give realistic results when compared to manual separation techniques.

Baseflow b(t) is then computed as:

b(t) = Q(t) - q(t)

The filter can be passed over the streamflow data three times (forward, backward, and forward), depending on the user' selected estimates of baseflow from pilot studies. In general, each pass will result in less baseflow as a percentage of total streamflow. 
}
\value{
%%  If it is a LIST, use
  \item{If \code{out.type="last"}}{(default value), only the baseflow computed after the third pass of the filter is returned.}
  \item{If \code{out.type="all"}}{the 3 baseflows computed after each pass of the filter are returned in a matrix or zoo object.}
%% ...
}
\references{
\cite{Arnold, J. G., Allen, P. M., Muttiah, R., Bernhardt, G. (1995). Automated base flow separation and recession analysis techniques. Groundwater, 33(6), 1010--1018. doi:10.1111/j.1745-6584.1995.tb00046.x}.

\cite{Arnold, J. G., Allen, P. M. (1999). Automated methods for estimating baseflow and ground water recharge from streamflow records. JAWRA Journal of the American Water Resources Association, 35(2), 411--424. doi:10.1111/j.1752-1688.1999.tb03599.x}.

\cite{Lyne, V., Hollick, M. (1979). Stochastic time-variable rainfall-runoff modelling. Proceedings of the Hydrology and Water Resources Symposium, Perth, 10--12 September. Institution of Engineers National Conference Publication, No. 79/10, 89--92}.

\cite{Nathan, R. J., & McMahon, T. A. (1990). Evaluation of automated techniques for base flow and recession analyses. Water resources research, 26(7), 1465--1473. doi:10.1029/WR026i007p01465}.

\cite{Ladson, A. R., Brown, R., Neal, B., & Nathan, R. (2013). A standard approach to baseflow separation using the Lyne and Hollick filter. Australasian Journal of Water Resources, 17(1), 25-34. doi:10.7158/13241583.2013.11465417}.

}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail} ; Hector Garces-Figueroa
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_pq}}, \code{\link{hydroplot}}, \code{\link{fdc}}, \code{\link{fdcu}}
}

\examples{
######################
## Ex1: Computing and plotting the baseflows for the full time period
##      of a given time series of streamflows.

## First, we load the daily Q time series for the Cauquenes en 
## El Arrayan catchment, where Q, [m3/s] are stored in the sixth column.
data(Cauquenes7336001)
q <- Cauquenes7336001[, 6]

## Computing the daily baseflow for the full time period
#baseflow(q) # it can not run due to NA values in 'x'

# filling the NA values using spline interpolation
baseflow(q, na.fill="spline") 

## Computing and plotting the daily baseflow for the full time period
baseflow(q, na.fill="spline", plot=TRUE)


######################
## Ex2: Computing and plotting the daily baseflow only for a 
##      specific time period, from April to December 2000.
baseflow(q, na.fill="spline", from="2000-04-01", to="2000-12-31")


######################
## Ex3: Computing and plotting the three daily baseflows (one for each pass 
##      of the filter) only for a specific time period, from April to December
##      2000.
baseflow(q, na.fill="spline", from="2000-04-01", to="2000-12-31", 
         out.type="all", plot=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{graphs}
