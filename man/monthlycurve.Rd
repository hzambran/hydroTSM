%%File monthlycurve.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ; 
%%                                 https://CRAN.R-project.org/package=hydroTSM
%%                                 http://www.rforge.net/hydroTSM/ ; 
%% Copyright 2008-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{monthlycurve}
\Rdversion{1.1}
\alias{monthlycurve}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
monthlycurve
}
\description{
Function to draw a monthlycurve of streamflow (and optionally precipitation) data. 
}
\usage{
monthlycurve(x, pcp, na.rm=TRUE, from, to, date.fmt="\%Y-\%m-\%d", 
             main=ifelse(missing(pcp), "Monthly Streamflows", "Monthly P and Q"), 
             FUN=mean, start.month=1, x.col="blue", 
             ubands.probs=c(0.25, 0.75), ubands.col="lightskyblue1", ubands.alpha=0.8,
             pcp.probs=c(0.25, 0.75), pcp.col="lightskyblue1", pcp.alpha=0.8, pcp.ylab="P, [mm]",
             xlab="Month", ylab="Q, [m3/s]", labels=TRUE, labels.cex=0.8,
             labels.dx=c(rep(-0.2,6), rep(0.2,6)), labels.dy=rep(-median(x, na.rm=TRUE)/10, 12)
             )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
variable of type zoo with monthly, daily or subdaily streamflow data (or other variable of interest).
}
  \item{pcp}{
variable of type zoo with monthly, daily or subdaily precipitation data.
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
  \item{FUN}{
function used to aggregate \code{x} from a sub-monthly time frequency (e.g., daily, hourly) into a monthly time frequency. It must support the \code{na.rm} argument.
}
  \item{start.month}{
numeric in [1:12] indicating the starting month for the monthlycurve. Numeric values in [1, 12] represents months in [January, December]. By default \code{start.month=1}.
}
  \item{x.col}{
Color used to plot the monthly average or median streamflow values.
}
  \item{ubands.probs}{
numeric of length 2. It is used to define quantile values used to compute the lower an upper uncertainty bounds around each one of the 12 monthly average or median streamflow .\cr 
If \code{pcp} is provided, then \code{ubands.probs} are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly average or median precipitation values. \cr
By default \code{ubands.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly average or median values. 
If \code{x} is provided and is a (sub)daily zoo object, it is first aggregated into monthly values using \code{FUN}, and then the \code{ubands.probs} quantiles are computed over all the monthly values belonging to a calendar month.
}
  \item{ubands.col}{
character with the color used to plot the uncertainty bands around the average or median streamflow values. \cr
} 
  \item{ubands.alpha}{
numeric of length 1, with the factor used to modify the opacity of \code{ubands.col}. Typically in [0,1], with 0 indicating a completely transparent colour and 1 indicating no transparency.
} 
  \item{pcp.probs}{
numeric of length 2. It defines the quantile values used to compute the lower an upper uncertainty bounds for each one of the 12 monthly precipitation values.\cr 
By default \code{pcp.probs=c(0.25, 0.75)}, which indicates that the quantiles 0.25 and 0.75 are used to compute the lower an upper uncertainty bounds for each one of the 12 monthly precipitation values. 
If \code{pcp} is a (sub)daily zoo object, it is first aggregated into monthly values using \code{mean}, and then the \code{pcp.probs} quantiles are computed over all the monthly values belonging to a calendar month.
} 
  \item{pcp.col}{
Color used to plot the precipitation bars in the upper panel. \cr
It is only used when\code{pcp} is provided by the user.
}
  \item{pcp.alpha}{
numeric of length 1, with the factor used to modify the opacity of \code{pcp.col}. Typically in [0,1], with 0 indicating a completely transparent colour and 1 indicating no transparency.
} 
  \item{pcp.ylab}{
character with the title used for the y axis of the precipitation values presented in the upper panel.\cr
It is only used when\code{pcp} is provided by the user.
}
  \item{xlab}{
character with the title used for the horizontal axis.
}
  \item{ylab}{
character with the title used for the y axis of the streamflow values.
}
  \item{labels}{
logical. Should monthly streamflow values to be shown above the lines?. By default \code{labels=TRUE}.
} 
  \item{labels.cex}{
numeric giving the amount by which plotting characters used to show the numeric values of monthly streamflow values are scaled relative to the default. 
}
  \item{labels.dx}{
numeric of length 12 giving the amount of horizontal coordinate positions that have to be used to shift the labels of monthly streamflow values. \cr
It is only used when \code{labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
}
  \item{labels.dy}{
numeric of length 12 giving the amount of vertical coordinate positions that have to be used to shift the labels of monthly streamflow values. \cr
It is only used when \code{labels=TRUE}. \cr
Lengths smaller than 12 are recycled and larger lengths are not used.
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
If the output monthlycurve present some mixed or not legible characters, you might try resizing the graphical window and run monthlycurve again with the new size, until you get the monthlycurve in the way you want to.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{monthlyfunction}}
}
\examples{
######################
## Ex1: Loading daily streamflows at the station Oca en Ona (Ebro River basin, Spain)
## 
data(OcaEnOnaQts)
q <- OcaEnOnaQts

# Because we do not have the actual precipitation values over the Oca en Ona, we are going to 
# create a dummy values thereof:
p <- q/10

## Plotting a full monthlycurve
m <- monthlycurve(q, pcp=p)

## Plotting a monthlycurve with uncertainty bands around mean values, but with no labels
m <- monthlycurve(q, pcp=p, labels=FALSE)

## Plotting a monthlycurve without precipitation data
m <- monthlycurve(q)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
