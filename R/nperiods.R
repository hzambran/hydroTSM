# File nperiods.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# nyears, nmonths, nweeks, ndays, nhours: Temporal span of zoo objects         #
################################################################################
# Author     : Mauricio Zambrano-Bigiarini                                     #
# Contributor:                                                                 #
################################################################################
# Started: 25-Sep-2026                                                         #
# Updates:                                                                     #
################################################################################

nyears <- function(x, ...) {

  UseMethod("nyears")

} # 'nyears' END

nmonths <- function(x, ...) {

  UseMethod("nmonths")

} # 'nmonths' END

nweeks <- function(x, ...) {

  UseMethod("nweeks")

} # 'nweeks' END

ndays <- function(x, ...) {

  UseMethod("ndays")

} # 'ndays' END

nhours <- function(x, ...) {

  UseMethod("nhours")

} # 'nhours' END

nyears.zoo <- function(x, only.integer.output=FALSE, tz, verbose=TRUE, ...) {

  # Computing the temporal span expressed in years
  if (missing(tz)) {
    .nperiods.zoo(x=x, unit="years",
                  only.integer.output=only.integer.output,
                  verbose=verbose, tz.provided=FALSE, ...)
  } else {
      .nperiods.zoo(x=x, unit="years",
                    only.integer.output=only.integer.output,
                    tz=tz, verbose=verbose, tz.provided=TRUE, ...)
    } # ELSE end

} # 'nyears.zoo' END

nmonths.zoo <- function(x, only.integer.output=FALSE, tz, verbose=TRUE, ...) {

  # Computing the temporal span expressed in months
  if (missing(tz)) {
    .nperiods.zoo(x=x, unit="months",
                  only.integer.output=only.integer.output,
                  verbose=verbose, tz.provided=FALSE, ...)
  } else {
      .nperiods.zoo(x=x, unit="months",
                    only.integer.output=only.integer.output,
                    tz=tz, verbose=verbose, tz.provided=TRUE, ...)
    } # ELSE end

} # 'nmonths.zoo' END

nweeks.zoo <- function(x, week.grouping="calendar",
                       verbose=TRUE, ...) {

  # Checking the weekly grouping rule
  week.grouping <- .daily2weekly_check_week_grouping(week.grouping)

  # Checking that 'x' is a zoo object
  if (!is.zoo(x))
    stop("Invalid argument: 'x' must be a 'zoo' object !")

  # Getting the time index of 'x'
  datetimes <- time(x)

  # Checking that 'x' contains enough observations
  if (!(length(datetimes) > 1L))
    stop("Invalid argument: 'x' must contain more than 1 observation !")

  # Checking that the time index does not have missing values
  if (any(is.na(datetimes)))
    stop("Invalid argument: the time index in 'x' must not contain missing values !")

  # Checking that the time index can be grouped as in daily2weekly()
  if (!inherits(datetimes, "Date") && !inherits(datetimes, "POSIXt"))
    stop("Invalid argument: the class of the time index must be 'Date' or 'POSIXt' !")

  # Delegating the completeness check and missing-date report to isComplete()
  isComplete(x, out.type="all", verbose=verbose)

  # Counting the weekly groups defined by daily2weekly()
  weeks <- .daily2weekly_week_groups(dates=datetimes,
                                     week.grouping=week.grouping)
  out <- as.integer(length(unique(weeks)))

  return(out)

} # 'nweeks.zoo' END

ndays.zoo <- function(x, only.integer.output=FALSE, tz, verbose=TRUE, ...) {

  # Computing the temporal span expressed in days
  if (missing(tz)) {
    .nperiods.zoo(x=x, unit="days",
                  only.integer.output=only.integer.output,
                  verbose=verbose, tz.provided=FALSE, ...)
  } else {
      .nperiods.zoo(x=x, unit="days",
                    only.integer.output=only.integer.output,
                    tz=tz, verbose=verbose, tz.provided=TRUE, ...)
    } # ELSE end

} # 'ndays.zoo' END

nhours.zoo <- function(x, only.integer.output=FALSE, tz, verbose=TRUE, ...) {

  # Computing the temporal span expressed in hours
  if (missing(tz)) {
    .nperiods.zoo(x=x, unit="hours",
                  only.integer.output=only.integer.output,
                  verbose=verbose, tz.provided=FALSE, ...)
  } else {
      .nperiods.zoo(x=x, unit="hours",
                    only.integer.output=only.integer.output,
                    tz=tz, verbose=verbose, tz.provided=TRUE, ...)
    } # ELSE end

} # 'nhours.zoo' END

.nperiods.zoo <- function(x, unit, only.integer.output=FALSE,
                          tz="UTC", verbose=TRUE, tz.provided=FALSE, ...) {

  # Checking that 'x' is a zoo object
  if (!is.zoo(x))
    stop("Invalid argument: 'x' must be a 'zoo' object !")

  # Checking that 'only.integer.output' is logical
  if (!is.logical(only.integer.output) ||
      length(only.integer.output) != 1L || is.na(only.integer.output))
    stop("Invalid argument: 'only.integer.output' must be a logical value !")

  # Getting the time index of 'x'
  datetimes <- time(x)

  # Checking that 'x' contains enough observations
  if (!(length(datetimes) > 1L))
    stop("Invalid argument: 'x' must contain more than 1 observation !")

  # Checking that the time index does not have missing values
  if (any(is.na(datetimes)))
    stop("Invalid argument: the time index in 'x' must not contain missing values !")

  # Converting numeric years for compatibility with isComplete.zoo()
  x.complete <- x
  numeric.years <- is.numeric(datetimes) &&
                   all(datetimes == trunc(datetimes))
  if (numeric.years)
    time(x.complete) <- as.Date(paste0(datetimes, "-01-01"))

  # Delegating the completeness check and missing-date report to isComplete()
  if (tz.provided && !numeric.years) {
    isComplete(x.complete, tz=tz, out.type="all", verbose=verbose)
  } else {
      isComplete(x.complete, out.type="all", verbose=verbose)
    } # ELSE end

  # Getting the temporal span, including the inferred final observation length
  tspan <- .nperiods_get_temporal_span(datetimes=datetimes,
                                       tz=tz, tz.provided=tz.provided)

  # Expressing the temporal span in the requested units
  out <- switch(unit,
                "years"  = .nperiods_calendar_units(tspan$start, tspan$end,
                                                     "years", tspan$tz),
                "months" = .nperiods_calendar_units(tspan$start, tspan$end,
                                                     "months", tspan$tz),
                "days"   = as.numeric(difftime(tspan$end, tspan$start,
                                                units="secs")) / 86400,
                "hours"  = as.numeric(difftime(tspan$end, tspan$start,
                                                units="secs")) / 3600)

  # Removing harmless floating-point noise from whole periods
  if (.nperiods_is_whole(out)) out <- round(out)

  # Returning only complete periods when requested
  if (only.integer.output) {
    if (!.nperiods_is_whole(out)) {
      warning("[ The last ", .nperiods_unit_singular(unit),
              " is not complete; returning only the number of whole ",
              unit, ". ]", call.=FALSE)
    } # IF end
    out <- as.integer(floor(out))
  } # IF end

  return(out)

} # '.nperiods.zoo' END

.nperiods_get_temporal_span <- function(datetimes, tz="UTC",
                                        tz.provided=FALSE) {

  # Handling annual numeric time indices
  if (is.numeric(datetimes) && all(datetimes == trunc(datetimes))) {
    years <- as.numeric(datetimes)
    step.years <- round(min(diff(years), na.rm=TRUE))
    if (!is.finite(step.years) || step.years <= 0)
      stop("Invalid argument: the time index in 'x' must be strictly increasing !")

    start <- as.POSIXct(paste0(years[1], "-01-01 00:00:00"), tz="UTC")
    end <- .nperiods_add_months(
             as.POSIXct(paste0(years[length(years)], "-01-01 00:00:00"),
                        tz="UTC"),
             12L * step.years, tz="UTC")

    return(list(start=start, end=end, tz="UTC"))
  } # IF end

  # Handling monthly yearmon time indices
  if (inherits(datetimes, "yearmon") ||
      (is.numeric(datetimes) &&
       !all(datetimes == trunc(datetimes)))) {
    ym <- zoo::as.yearmon(datetimes)
    month.index <- as.integer(format(ym, "%Y")) * 12L +
                   as.integer(format(ym, "%m"))
    step.months <- round(min(diff(month.index), na.rm=TRUE))
    if (!is.finite(step.months) || step.months <= 0)
      stop("Invalid argument: the time index in 'x' must be strictly increasing !")

    start <- as.POSIXct(as.Date(ym[1]), tz="UTC")
    end <- .nperiods_add_months(
             as.POSIXct(as.Date(ym[length(ym)]), tz="UTC"),
             step.months, tz="UTC")

    return(list(start=start, end=end, tz="UTC"))
  } # IF end

  # Using the time zone stored in the time index when available
  tz.out <- tz
  if (!tz.provided) {
    tz.attr <- attr(datetimes, "tzone")
    if (length(tz.attr) > 0L && !is.na(tz.attr[1]) && nzchar(tz.attr[1]))
      tz.out <- tz.attr[1]
  } # IF end

  # Converting supported date-time classes to POSIXct
  if (inherits(datetimes, "Date")) {
    dt <- as.POSIXct(datetimes, tz=tz.out)
  } else if (inherits(datetimes, "POSIXt")) {
      if (tz.provided)
        datetimes <- timechange::time_force_tz(datetimes, tz=tz.out)
      dt <- as.POSIXct(datetimes, tz=tz.out)
    } else {
        stop("Invalid argument: the time index in 'x' must be numeric, Date, yearmon, or POSIXt !")
      } # ELSE end

  # Checking that the time index is strictly increasing
  dsecs <- as.numeric(diff(dt), units="secs")
  if (any(!is.finite(dsecs)) || any(dsecs <= 0))
    stop("Invalid argument: the time index in 'x' must be strictly increasing and must not contain duplicated values !")

  # Inferring the end of the final observation from the regular time step
  if (.nperiods_is_calendar_frequency(dt, "years", tz.out)) {
    step.years <- round(min(.nperiods_year_diff(dt, tz.out), na.rm=TRUE))
    end <- .nperiods_add_months(dt[length(dt)], 12L * step.years, tz.out)
  } else if (.nperiods_is_calendar_frequency(dt, "months", tz.out)) {
      step.months <- round(min(.nperiods_month_diff(dt, tz.out), na.rm=TRUE))
      end <- .nperiods_add_months(dt[length(dt)], step.months, tz.out)
    } else {
        step.secs <- min(dsecs, na.rm=TRUE)
        end <- dt[length(dt)] + step.secs
      } # ELSE end

  return(list(start=dt[1], end=end, tz=tz.out))

} # '.nperiods_get_temporal_span' END

.nperiods_is_calendar_frequency <- function(dt, unit, tz) {

  # Splitting date-times into their calendar components
  lt <- as.POSIXlt(dt, tz=tz)

  # Identifying annual and multi-annual frequencies
  if (unit == "years") {
    same.time <- length(unique(paste(lt$mon, lt$mday, lt$hour,
                                     lt$min, floor(lt$sec)))) == 1L
    if (!same.time) return(FALSE)
    yd <- .nperiods_year_diff(dt, tz)
    return(all(yd == round(yd)) && all(yd >= 1))
  } # IF end

  # Identifying monthly and multi-monthly frequencies
  same.time <- length(unique(paste(lt$mday, lt$hour, lt$min,
                                   floor(lt$sec)))) == 1L
  if (!same.time) return(FALSE)
  md <- .nperiods_month_diff(dt, tz)

  return(all(md == round(md)) && all(md >= 1))

} # '.nperiods_is_calendar_frequency' END

.nperiods_month_diff <- function(dt, tz) {

  # Computing calendar-month differences between consecutive date-times
  lt <- as.POSIXlt(dt, tz=tz)
  out <- (lt$year[-1] - lt$year[-length(lt$year)]) * 12L +
         (lt$mon[-1] - lt$mon[-length(lt$mon)])

  return(out)

} # '.nperiods_month_diff' END

.nperiods_year_diff <- function(dt, tz) {

  # Computing calendar-year differences between consecutive date-times
  out <- .nperiods_month_diff(dt, tz) / 12

  return(out)

} # '.nperiods_year_diff' END

.nperiods_calendar_units <- function(start, end, unit, tz) {

  # Selecting the number of months in the requested calendar unit
  nmonths <- if (unit == "years") 12L else 1L
  whole <- 0L
  current <- start
  next.time <- .nperiods_add_months(current, nmonths, tz)

  # Counting complete calendar periods
  while (next.time <= end) {
    whole <- whole + 1L
    current <- next.time
    next.time <- .nperiods_add_months(current, nmonths, tz)
  } # WHILE end

  # Returning immediately when the span contains only whole periods
  if (identical(as.numeric(current), as.numeric(end))) return(whole)

  # Expressing the remaining span as a fraction of the next calendar period
  residual <- as.numeric(difftime(end, current, units="secs"))
  period.length <- as.numeric(difftime(next.time, current, units="secs"))
  out <- whole + residual / period.length

  return(out)

} # '.nperiods_calendar_units' END

.nperiods_add_months <- function(x, n, tz) {

  # Adding calendar months while preserving valid month-end dates
  lt <- as.POSIXlt(x, tz=tz)
  total.month <- (lt$year + 1900L) * 12L + lt$mon + as.integer(n)
  new.year <- total.month %/% 12L
  new.month <- total.month %% 12L + 1L
  new.day <- pmin(lt$mday, .nperiods_days_in_month(new.year, new.month))

  out <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%06.3f",
                            new.year, new.month, new.day,
                            lt$hour, lt$min, lt$sec), tz=tz)

  return(out)

} # '.nperiods_add_months' END

.nperiods_days_in_month <- function(year, month) {

  # Getting the last calendar day of the requested month
  next.year <- year + (month == 12L)
  next.month <- ifelse(month == 12L, 1L, month + 1L)
  out <- as.integer(format(
           as.Date(sprintf("%04d-%02d-01", next.year, next.month)) - 1L,
           "%d"))

  return(out)

} # '.nperiods_days_in_month' END

.nperiods_is_whole <- function(x) {

  # Checking for a whole number within floating-point tolerance
  out <- abs(x - round(x)) < sqrt(.Machine$double.eps)

  return(out)

} # '.nperiods_is_whole' END

.nperiods_unit_singular <- function(unit) {

  # Returning the singular form used in warning messages
  out <- switch(unit, "years"="year", "months"="month",
                "days"="day", "hours"="hour")

  return(out)

} # '.nperiods_unit_singular' END
