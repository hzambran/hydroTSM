%%File streamflowQC-class.Rd
%% Part of the hydroTSM R package

\name{streamflowQC-class}
\Rdversion{1.1}
\alias{streamflowQC-class}
\alias{print.streamflowQC}
\alias{plot.streamflowQC}

\title{Streamflow quality-control result}

\description{
Structure, print method, and four-panel summary plot for objects returned by
\code{\link{streamflowQC_daily}}, \code{\link{streamflowQC_subdaily}}, and
\code{\link{streamflowQC}}.
}

\usage{
\method{print}{streamflowQC}(x, ...)

\method{plot}{streamflowQC}(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
}

\arguments{
  \item{x}{An object inheriting from class \code{"streamflowQC"}.}
  \item{max.stations}{Maximum number of stations in the affected-station
panel.}
  \item{col}{At least three plotting colours.}
  \item{...}{Additional arguments; the plot method passes them to its final
time-series panel.}
}

\details{
The plot summarizes station recommendations, missing/review/rejected
fractions, counts by active test, and the time distribution of confirmed
suspicious observations. Plotting is diagnostic and does not alter the
result or write a file unless the caller explicitly opens a file device.
}

\value{
Both methods return \code{x} invisibly. A \code{"streamflowQC"} object is a
list with:
\itemize{
  \item \code{accepted.metadata} and \code{discarded.metadata}: original
  station metadata plus completeness, evidence, homogeneity, recommendation,
  and reason fields;
  \item \code{accepted.data} and \code{discarded.data}: original \code{zoo}
  series split by the station recommendation;
  \item \code{accepted.corrected} and \code{discarded.corrected}: the same
  partitions after the requested correction policy;
  \item \code{suspicious}: one row per point flagged for review or rejection,
  including all contributing test names and any spatial estimate;
  \item \code{corrections}: an audit table containing every changed value;
  \item \code{flags}, \code{flag.count}, and \code{rejected}: individual
  logical flag objects, evidence counts, and confirmed decisions;
  \item \code{station.summary}: station completeness, review/rejected
  fractions, breakpoint diagnostics, recommendation, and reason;
  \item \code{breakpoint}: annual-indicator Pettitt-test diagnostics;
  \item \code{spatial.estimate}, \code{spatial.score},
  \code{spatial.support}, and \code{neighbours}: spatial-test diagnostics;
  \item \code{highflow} and \code{return.period}: component high-flow flags,
  GEV parameters, and estimated return periods; and
  \item \code{settings}: all resolved workflow controls.
}
}

\references{
Fileni, F. et al. (2026). UK-Flow15-QC: A quality control framework for
better river flow data in hydrological research. \emph{EGUsphere preprint}.
doi:10.5194/egusphere-2026-277.
}

\seealso{
\code{\link{streamflowQC_daily}}, \code{\link{streamflowQC_subdaily}},
\code{\link{streamflowQC_tests}}
}
