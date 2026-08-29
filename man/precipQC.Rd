%%File precipQC.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{precipQC}
\Rdversion{1.1}
\alias{precipQC}

\title{Frequency-aware quality control of precipitation time series}

\description{
Detects the sampling frequency of a precipitation \code{zoo} object with
\code{\link{sfreq}} and dispatches to \code{\link{precipQC_daily}} or
\code{\link{precipQC_subdaily}}. No files are written.
}

\usage{
precipQC(
  x, metadata=NULL, station.id="station", coords=c("lon", "lat"), ...,
  elevation=NULL, elevation.scale=500
)
}

\arguments{
  \item{x}{
A univariate or multivariate numeric \code{zoo} object. Each column is one
precipitation station and must have a unique name.
}
  \item{metadata, station.id, coords, elevation, elevation.scale}{
Optional station metadata, field-name selectors, and elevation-decay control
passed to the selected workflow. See \code{\link{precipQC_daily}}.
}
  \item{...}{
Arguments passed unchanged to the selected QC function. Because the daily and
sub-daily functions have resolution-specific arguments, only arguments
accepted by the detected workflow should be supplied.
}
}

\details{
\code{sfreq(x)} returning \code{"daily"} selects
\code{precipQC_daily(x, ...)}. Values \code{"minute"} and \code{"hourly"}
select \code{precipQC_subdaily(x, ...)}. Weekly, monthly, quarterly, annual,
or other frequencies are rejected with an informative error because the
implemented thresholds and tests are not valid at those resolutions.
}

\value{
An object of class \code{"precipQC"}; see \code{\link{precipQC-class}}.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{sfreq}}, \code{\link{precipQC_daily}},
\code{\link{precipQC_subdaily}}, \code{\link{plot.precipQC}}
}

\examples{
dates <- seq(as.Date("2020-01-01"), by="day", length.out=40)
x <- zoo(cbind(A=rep(0, 40), B=rep(0, 40)), dates)

qc <- precipQC(
  x, min.years=0,
  checks=c(climatology=FALSE, spatial=FALSE, dryspell=FALSE,
           breakpoint=FALSE)
)
qc$settings$resolution
}

\keyword{ts}
\keyword{manip}
