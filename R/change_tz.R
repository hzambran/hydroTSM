# File change_tz.R
# Part of the hydroTSM R package, https://github.com/hzambran/hydroTSM ;
#                                 https://CRAN.R-project.org/package=hydroTSM
# Copyright 2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# change_tz: change the time zone of a sub-daily zoo object                    #
################################################################################

change_tz <- function(x, ...) UseMethod("change_tz")


.change_tz_check_timezone <- function(tz, arg.name) {

  if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz))
    stop("Invalid argument: '", arg.name,
         "' must be a non-empty character string !")

  if (!(tz %in% OlsonNames()))
    stop("Invalid argument: '", arg.name,
         "' must be a valid time zone listed by OlsonNames() !")

  tz

} # '.change_tz_check_timezone' END


.change_tz_time_zone <- function(datetimes) {

  tz <- attr(datetimes, "tzone")

  if (is.null(tz) || length(tz) < 1L || is.na(tz[1L]) || !nzchar(tz[1L]))
    return(NA_character_)

  tz[1L]

} # '.change_tz_time_zone' END


change_tz.zoo <- function(x, new.tz, old.tz=NULL, ...) {

  if (!is.zoo(x))
    stop("Invalid argument: 'x' must be a 'zoo' object !")

  if (missing(new.tz))
    stop("Missing argument: 'new.tz' must be provided !")

  new.tz <- .change_tz_check_timezone(new.tz, "new.tz")

  if (!is.null(old.tz))
    old.tz <- .change_tz_check_timezone(old.tz, "old.tz")

  datetimes <- time(x)
  nvalues   <- NROW(x)

  if (length(datetimes) == 0L)
    stop("Invalid argument: 'x' must have a time index !")

  if (length(datetimes) != nvalues)
    stop("Invalid argument: the length of the time index must be equal to the number of observations in 'x' !")

  if (!inherits(datetimes, "POSIXt"))
    stop("Invalid argument: 'time(x)' must inherit from 'POSIXct' or 'POSIXlt' !")

  detected.old.tz <- .change_tz_time_zone(datetimes)
  if (is.null(old.tz)) {
    if (is.na(detected.old.tz)) {
      stop("Invalid argument: 'old.tz' could not be inferred from 'time(x)' and must be provided !")
    }
    old.tz <- .change_tz_check_timezone(detected.old.tz, "old.tz")
  } else if (!is.na(detected.old.tz) && old.tz != detected.old.tz) {
    stop("Invalid argument: 'old.tz' is '", old.tz,
         "', but 'time(x)' uses '", detected.old.tz, "' !")
  }

  if (old.tz == new.tz)
    stop("Invalid argument: 'old.tz' and 'new.tz' must be different !")

  datetimes <- as.POSIXct(datetimes, tz=old.tz)
  dt.secs   <- as.numeric(diff(datetimes), units="secs")

  if (length(dt.secs) == 0L || any(!is.finite(dt.secs)) || any(dt.secs <= 0))
    stop("Invalid argument: 'time(x)' must be strictly increasing and must not contain duplicated values !")

  if (stats::median(dt.secs) >= 86400)
    stop("Invalid argument: 'x' must have a sub-daily POSIXt time index !")

  out <- x
  time(out) <- as.POSIXct(datetimes, tz=new.tz)

  out

} # 'change_tz.zoo' END
