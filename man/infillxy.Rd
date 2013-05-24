%%File infillxy.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2013 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{infillxy}
\Rdversion{1.1}
\alias{infillxy}
\alias{infillxy.matrix}
\alias{infillxy.default}
\alias{infillxy.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Infills NA values
}
\description{
Infill all the missing values (NA) in \code{x} with the corresponding values in \code{sim}. 
}
\usage{
infillxy(x, ...)
\method{infillxy}{default}(x, sim, ...)
\method{infillxy}{matrix}(x, sim, ...)
\method{infillxy}{data.frame}(x, sim, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric, data.frame or matrix in which some values are missing (NA).
}
  \item{sim}{
numeric, data.frame or matrix, with the same dimension of \code{x}, which contains the values that will be used for infilling the missing (NA) values in \code{x}
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
It gives as a result an object of the same dimension of \code{x}, in which all the NA values were infilled with the corresponding values of \code{sim}.
}
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

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
obs <- c(1, NA, 3, 4, NA, 5)
sim <- rep(2, 6)

## Filling in the missing values in 'x' with the corresponding values in 'sim'
infillxy(x=obs, sim)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
