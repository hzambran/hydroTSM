%%File tempQC-class.Rd
%% Part of the hydroTSM R package

\name{tempQC-class}
\Rdversion{1.1}
\alias{tempQC-class}
\alias{print.tempQC}
\alias{plot.tempQC}

\title{Air-temperature quality-control result}

\description{
Structure, print method, and four-panel summary plot for objects returned by
\code{\link{tempQC_daily}}, \code{\link{tempQC_subdaily}}, and
\code{\link{tempQC}}.
}

\usage{
\method{print}{tempQC}(x, ...)

\method{plot}{tempQC}(
  x, max.stations=20L,
  col=c("#2878B5", "#D55E00", "#E69F00"), ...
)
}

\arguments{
  \item{x}{An object inheriting from class \code{"tempQC"}.}
  \item{max.stations}{Maximum number of stations in the affected-station
panel.}
  \item{col}{At least three plotting colours.}
  \item{...}{Additional arguments; the plot method passes them to its final
time-series panel.}
}

\details{
The plot summarizes station recommendations, missing and confirmed-suspicious
fractions, counts by active test, and the time distribution of rejected
observations. It is diagnostic and does not alter the result.
}

\value{
Both methods return \code{x} invisibly. A \code{"tempQC"} object is a list
with:
\itemize{
  \item \code{accepted.metadata} and \code{discarded.metadata}: station
  metadata plus recommendation diagnostics;
  \item \code{accepted.data}: original \code{zoo} series for stations
  recommended for acceptance;
  \item \code{accepted.corrected}: the same accepted stations after the
  requested correction policy;
  \item \code{suspicious}: one row per point flagged for review or rejection;
  \item \code{corrections}: an audit table of changed values;
  \item \code{flags}, \code{flag.count}, and \code{rejected}: individual
  logical flag objects, evidence counts, and confirmed decisions;
  \item \code{station.summary}: completeness, flag fractions, breakpoint
  diagnostics, recommendation, and reason;
  \item \code{breakpoint}: station homogeneity diagnostics;
  \item \code{spatial.estimate}, \code{spatial.score}, and
  \code{neighbours}: spatial-test diagnostics; and
  \item \code{settings}: resolved workflow settings.
}
}

\references{
Brunet, M. et al. (2020). \emph{Best Practice Guidelines for Climate Data
and Metadata Formatting, Quality Control and Submission}. Copernicus Climate
Change Service.
}

\seealso{
\code{\link{tempQC_daily}}, \code{\link{tempQC_subdaily}},
\code{\link{tempQC_tests}}
}
