%%File isComplete.Rd
%% Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2025-2025 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{isComplete}
\Rdversion{1.1}
\alias{isComplete}
\alias{isComplete.zoo}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Is Complete?
}
\description{
Generic function for identifying whether a zoo object has a regular time frequency without missing values from the first one to the last one.
}

\usage{
isComplete(x, ...)

\method{isComplete}{zoo}(x, tz, out.type=c("single", "all"), verbose=TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo object with a time attribute that should have a regular time frequency (e.g., hourly, daily, monthly, annual).
}
 \item{tz}{
character, indicating the time zone in which the date of \code{x} is located. System-specific (see time zones), but \code{""} is the current time zone, and \code{"GMT"} is UTC (Universal Time, Coordinated). See \code{\link[base]{Sys.timezone}} and \code{\link[base]{as.POSIXct}}. \cr

If \code{tz} is missing (the default), it is automatically set to the time zone used in \code{time(x)}. \cr

If \code{tz} is provided, it forces \code{time(x)} to be in the tome zone specified by \code{tz}, without modifying the the values (hours, minutes, seconds, etc). \cr

A list of valid time zones can be obtained by calling the base function \code{OlsonNames()}. \cr

This argument can be used when working with sub-daily zoo objects to force using  time zones other than the local time zone for \code{from} and \code{to}. It should be used with caution, being well aware of the time zone of the data. See examples.
}
  \item{out.type}{
Character indicating the type of result that is given by this function. Valid values are: \cr
-) \kbd{single} => only a logical output is returned. See details in the \kbd{Value} section. \cr
-) \kbd{all}  => a list with 3 elements is returned. See details in the \kbd{Value} section.
}
  \item{verbose}{
logical; if TRUE, informative messages are shown in the screen.

When \code{verbose=TRUE} and \code{x} has some missing temporal layers, the DateTime(s) with missing layer(s) are shown in the screen.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
%%  If it is a LIST, use
  \item{If \code{out.type="single"}}{(default value), it returns a logical value identifying whether \code{x} has a regular time frequency with complete time layers from its first layer to the last one.}
  \item{If \code{out.type="all"}}{it returns a list with the following three elements:

    -) \kbd{isComplete}: logical value identifying whether \code{x} has a regular time frequency with complete time layers from its first layer to the last one.

    -) \kbd{NumberMissingDT}: integer indicating the amount of missing layers from the first layer of \code{x} to the last one.

    -) \kbd{missingDateTimes}: Numeric, Date, or POSIXct vector showing the DateTime of the missing layers in \code{x}. 
    }
}
%% ...

%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{window}}
}
\examples{

###########
# EXAMPLE 1: Hourly zoo object, with a POSIXct time attribute
###########

# Loading the HOURLY streamflow data for Karamea at Gorge
data(KarameaAtGorgeQts)
x.h <- KarameaAtGorgeQts

# Checking whether 'x' is complete or not
isComplete(x.h)

###########
## EXAMPLE 2: Daily object, with a Date time attribute
###########
     
# Loading the DAILY precipitation data at SanMartino
data(SanMartinoPPts)
x.d <- SanMartinoPPts

# Checking whether 'x' is complete or not
isComplete(x.d)

# Removing two values and testing it again
x.d <-x.d[-c(3,5)]
isComplete(x.d)


###########
# EXAMPLE 3: Weekly zoo object, with a Date time attribute
###########

# Creating a dummy weekly object with a Date time attribute
x.w           <- x.d[1:12]
time(x.w) <-  seq(from=as.Date("2001-01-01"), to=as.Date("2001-03-19"), 
                  by="1 week")

# Checking whether 'x.w' is complete or not
isComplete(x.w)

# Removing two values and testing it again
x.w2 <- x.w[-c(3,5)]
isComplete(x.w2)


###########
# EXAMPLE 4: Monthly zoo object, with a yearmon time attribute
###########

# Transforming the daily object into a monthly one
x.m1 <- daily2monthly(x.d, FUN=sum)

# Checking whether 'x.m1' is complete or not
isComplete(x.m1)

# Removing two values and testing it again
x.m12 <- x.m1[-c(3,5)]
isComplete(x.m12)


###########
# EXAMPLE 5: Monthly zoo object, with a Date time attribute
###########

# Creating a monthly object with a Date time attribute
x.m2       <- x.m1
time(x.m2) <-  as.Date( paste0( format(zoo::as.yearmon( time(x.m1) ), "\%Y-\%m"), "-01") )

# Checking whether 'x.m2' is complete or not
isComplete(x.m2)

# Removing two values and testing it again
x.m22 <- x.m2[-c(3,5)]
isComplete(x.m22)


###########
# EXAMPLE 6: Annual zoo object, with a Date time attribute
###########

# Creating a dummy annual object with Date time attribute
x.a1       <- x.m1[1:12]
time(x.a1) <- hydroTSM::yip("2001-01-01", "2012-01-01")

# Checking whether 'x.a1' is complete or not
isComplete(x.a1)

# Removing two values and testing it again
x.a12 <- x.a1[-c(3,5)]
isComplete(x.a12)


###########
# EXAMPLE 7: 3-hourly zoo object, with a POSIXct time attribute
###########

# Creating a dummy 3-hourly object with a POSIXct time attribute
x.3h       <- x.m1[1:12]
time(x.3h) <- seq(from=as.POSIXct("2001-01-01 00:00:00", tz="UTC"), 
                  to=as.POSIXct("2001-01-02 09:00:00", tz="UTC"), 
                  by="3 hours")

# Checking whether 'x.3h' is complete or not
isComplete(x.3h)

# Removing two values and testing it again
x.3h2 <- x.3h[-c(3,5)]
isComplete(x.3h2)


############
# EXAMPLE 8: 8-day zoo object, with a Date time attribute
############

# Creating a dummy 8-day object with a Date time attribute
x.8d       <- x.m1[1:12]
time(x.8d) <- seq(from=as.Date("2001-01-01"), to=as.Date("2001-03-31"), by="8 days")

# Checking whether 'x.8d' is complete or not
isComplete(x.8d)

# Removing two values and testing it again
x.8d2 <- x.8d[-c(3,5)]
isComplete(x.8d2)


############
# EXAMPLE 9: 3-monthly zoo object, with a Date time attribute
############

# Creating a dummy 3-monthly object with a Date time attribute
x.3m       <- x.m1[1:12]
time(x.3m) <- seq(from=as.Date("2001-01-01"), to=as.Date("2003-12-01"), by="3 months")

# Checking whether 'x.3m' is complete or not
isComplete(x.3m)

# Removing two values and testing it again
x.3m2 <- x.3m[-c(3,5)]
isComplete(x.3m2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%%\keyword{ ~kwd2 }
