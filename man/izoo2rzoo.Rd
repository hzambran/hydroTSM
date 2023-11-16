%%File izoo2rzoo.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2023 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{izoo2rzoo}
\Rdversion{1.1}
\alias{izoo2rzoo}
\alias{izoo2rzoo.default}
\alias{izoo2rzoo.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Irregular Zoo -> Regular Zoo
}
\description{
It takes an irregular zoo object (with non-existing values for some dates) and converts it into a regularly spaced zoo object within the time period defined by \code{from} and \code{to}, by filling the missing dates with \sQuote{NA}
}
\usage{
izoo2rzoo(x, ...)

\method{izoo2rzoo}{default}(x, from= start(x), to= end(x), 
                   date.fmt, tstep, tz, ...)
     
\method{izoo2rzoo}{zoo}(x, from= start(x), to= end(x), 
                   date.fmt, tstep, tz, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
irregular zoo object (vector or matrix) representing a time series (very likely read with some user-defined procedure, and with some missing values for particular days/months/years)
}
  \item{from}{
Character indicating the starting date for creating the regularly spaced zoo object. The default value corresponds to the date of the first element of \code{x} \cr
It has to be in the format indicated by \code{date.fmt}. 
}
  \item{to}{
Character indicating the ending date for creating the regularly spaced zoo object. The default value corresponds to the date of the last element of \code{x} \cr
It has to be in the format indicated by \code{date.fmt}.
}
  \item{date.fmt}{
character indicating the format in which the dates are stored in \code{from} and \code{to}, e.g. \kbd{\%Y-\%m-\%d}. See \sQuote{Details} section in \code{\link[base]{strptime}}.
By default, \code{date.fmt} is missing, and it is automatically set to \kbd{\%Y-\%m-\%d} when \code{time(x)} is \code{Date} object, and set to \kbd{\%Y-\%m-\%d \%H:\%M:\%S} when \code{x} is a sub-daily zoo object.
}
  \item{tstep}{
character, indicating the time step used for creating the time sequence going from \code{from} to \code{to} that will be used as \code{time(x)} \cr
Valid values are (but not limited to) \kbd{hours}, \kbd{days}, \kbd{months}, \kbd{years}.
By default, \code{tstep} is missing, and it is automatically set to \kbd{"minutes"} when \code{sfreq(x)} is \kbd{min}, to \kbd{"hours"} when \code{sfreq(x)} is \kbd{hourly}, to \kbd{"days"} when \code{sfreq(x)} is \kbd{daily}, to \kbd{"weeks"} when \code{sfreq(x)} is \kbd{weekly}, to \kbd{"months"} when \code{sfreq(x)} is \kbd{monthly}, to \kbd{"quarters"} when \code{sfreq(x)} is \kbd{quarterly}, and to \kbd{"years"} when \code{sfreq(x)} is \kbd{annual}.
}
  \item{tz}{
character, with the specification of the time zone used for \code{time(x)}, \code{from}, \code{to}. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr
If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr
This argument can be used when working with sub-daily zoo objects to force using the local time zone or any other time zone instead of UTC as time zone.
}
  \item{\dots}{
further arguments passed to or from other methods
}
}
\details{
If the full time period of \code{x} is a subset of the time period defined by \code{from} and \code{to}, the time period of the resulting zoo is the one defined by \code{from} and \code{to}, assigning 'NA' to all the dates in which \code{x} do not have a value.
}
\value{
a regularly spaced zoo object, with values given by \code{x} and time stamps going from \code{from} to \code{to} at intervals defined by \code{tsteps}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
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

\seealso{
\code{\link[zoo]{zoo}}, \code{\link{vector2zoo}}, \code{\link[base]{as.POSIXct}}, \code{\link[base]{Sys.timezone}}
}

\examples{
##
## Example 1: Adding NA for February 29th to an existing zoo object

# dummy values and dates (February 29th is not present !)
x <- 1:9
dates <- c("1964-02-25", "1964-02-26", "1964-02-27", "1964-02-28", "1964-03-01", 
           "1964-03-02", "1964-03-03", "1964-03-04", "1964-03-05")

# From 'character' to 'Date' class
dates <- as.Date(dates)

## From 'numeric' to 'zoo' class
( x <- zoo(x, dates) ) # Feb 29th is still not present in 'x'

## Adding a missing value (NA in this case) for Feb 29th
( y <- izoo2rzoo(x) )

## checking the new length
length(y) # 1 element more than the original 'x' (Feb 29th)


##
## Example 2: Extending the original 'x' object from February 1st to the end of 
#  March, assigning 'NA' to the days in which 'x' do not have a value.
( y <- izoo2rzoo(x, from="1964-02-01", to="1964-03-31") )


##
## Example 3: Working with a zoo matrix
( Y <- cbind(x,x) )

# Adding a missing value (NA in this case) for Feb 29th in all the columns of Y
( rY <- izoo2rzoo(Y, from="1964-02-25", to="1964-03-05") )


##
## Example 4: Working with hourly data, from 01:00 to 10:00 on 12th December 2000
dates  <- ISOdatetime(2000, 12, 12, 1:10, 0, 0)
values <- 1:10
x      <- zoo(values, dates)

# removing values from 02:00 to 05:00 (not present at all in 'x', not even NA !)
x <- x[-c(2:5)]

# Adding missing values (NA in this case) from 02:00 to 05:00
y <-  izoo2rzoo(x)
time(y)


##
## Example 5: Extending hourly data to a DateTime before 'start(x)'
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00")
x  <- zoo(0:20, dt)
y  <- izoo2rzoo(x, from="2020-12-31") # 00:00:00 is ommited

##
## Example 6: Extending hourly data to a DateTime after 'end(x)'
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00")
x  <- zoo(0:20, dt)
( y  <- izoo2rzoo(x, to="2021-01-02 12:00:00") )

##
## Example 7: Extending hourly data to a DateTime before 'start(x)'.
dt <- hip("2021-01-01 00:00:00", "2021-01-01 20:00:00")
x  <- zoo(0:20, dt)
( y  <- izoo2rzoo(x, from="2020-12-31 20:00:00") )

##
## Ex8: Extending hourly data to a date before 'start(x)'. However, this hourly 
##      values are given at HH:15:00 hours instead of HH:00:00 hours.


## Loading the time series of hourly streamflows for the station Karamea at Gorge
data(KarameaAtGorgeQts)
x <- KarameaAtGorgeQts

# Subsetting 'x' to its first three days 
# (01/Jan/1980 08:15:00 - 03/Jan/1980 23:15:00)
x <- window(x, end="1980-01-03 23:59:00")

# Adding NA hourly data since 1979-12-31 21:15:00
izoo2rzoo(x, from="1979-12-31 21:15:00")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
