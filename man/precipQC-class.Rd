%%File precipQC-class.Rd
%% Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM
%% Copyright 2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{precipQC-class}
\Rdversion{1.1}
\alias{precipQC-class}
\alias{print.precipQC}
\alias{plot.precipQC}

\title{Precipitation quality-control result}

\description{
Print and plot methods for objects returned by \code{\link{precipQC}},
\code{\link{precipQC_daily}}, and \code{\link{precipQC_subdaily}}.
}

\usage{
\method{print}{precipQC}(x, ...)

\method{plot}{precipQC}(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
}

\arguments{
  \item{x}{
An object of class \code{"precipQC"}.
}
  \item{max.stations}{
Maximum number of stations displayed in the missing/suspicious percentage
panel. Stations with the largest suspicious and missing fractions are shown.
}
  \item{col}{
At least three colours for accepted stations, discarded/rejected values, and
missing/review flags.
}
  \item{\dots}{
For \code{plot}, additional arguments passed to the final timeline
\code{\link[graphics]{plot}} call. Ignored by \code{print}.
}
}

\details{
The plot contains four panels: numbers of accepted and discarded stations;
missing and confirmed-suspicious percentages for the most affected stations;
flag counts by individual test; and the number of confirmed suspicious values
through time.
}

\value{
Both methods return \code{x} invisibly. The print method writes a compact
summary to the console and the plot method draws on the active graphics
device.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{precipQC}}, \code{\link{precipQC_daily}},
\code{\link{precipQC_subdaily}},
\code{\link{precipQC_range}}
}

\keyword{hplot}
\keyword{ts}
