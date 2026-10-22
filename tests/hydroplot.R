library(hydroTSM)

################################################################################
# hydroplot                                                                    #
################################################################################

dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by="day")
x <- zoo::zoo(seq_along(dates), dates)

# Comparing all writable graphical parameters before and after plotting.
same.par <- function(before, after) {
  isTRUE(all.equal(before, after, check.attributes=TRUE))
} # 'same.par' end

pdf.file <- tempfile(fileext=".pdf")
grDevices::pdf(pdf.file)

# Defining non-default values makes accidental graphical changes observable.
graphics::par(mfrow=c(2, 2), mar=c(3.1, 4.2, 2.3, 1.4),
              mgp=c(2.2, 0.7, 0), las=2, xpd=NA)
before <- graphics::par(no.readonly=TRUE)

hydroplot(x, FUN=mean, ptype="ts+boxplot+hist", pfreq="dma")
after.regular <- graphics::par(no.readonly=TRUE)

hydroplot(x, FUN=mean, pfreq="seasonal")
after.seasonal <- graphics::par(no.readonly=TRUE)

# Triggering an error after hydroplot has changed the multi-panel arrangement.
plot.error <- try(hydroplot(x, FUN=mean, ptype="ts", pfreq="dma", log="bad"),
                  silent=TRUE)
after.error <- graphics::par(no.readonly=TRUE)

# Verifying that single plots advance through a user-defined four-panel layout.
graphics::par(mfcol=c(4, 1))
panel.positions <- matrix(NA_integer_, nrow=4, ncol=4)
for (i in seq_len(4)) {
  hydroplot(x, ptype="ts", pfreq="o")
  panel.positions[i, ] <- graphics::par("mfg")
} # FOR end

# Checking a user-defined format for data-frame dates and subset limits.
dates.custom <- format(dates, "%d/%m/%Y")
x.data.frame <- data.frame(date=dates.custom, value1=seq_along(dates),
                           value2=rev(seq_along(dates)))
data.frame.plot <- try(hydroplot(x.data.frame, dates=1,
                                date.fmt="%d/%m/%Y",
                                from="01/03/2000", to="31/03/2000",
                                ptype="ts", pfreq="o"), silent=TRUE)

# Checking that a user-defined sub-daily format is not overwritten.
hourly.dates <- seq(as.POSIXct("2000-01-01 00:00:00", tz="UTC"),
                    by="hour", length.out=48)
hourly <- zoo::zoo(seq_along(hourly.dates), hourly.dates)
subdaily.plot <- try(hydroplot(hourly, date.fmt="%d/%m/%Y %H:%M",
                              from="01/01/2000 06:00",
                              to="02/01/2000 06:00", ptype="ts", pfreq="o"),
                     silent=TRUE)

# Recording the values received by each explicitly drawn Y axis.
axis.trace <- new.env(parent=emptyenv())
axis.trace$values <- numeric()
trace("axis", where=asNamespace("hydroTSM"), print=FALSE,
      tracer=quote({
        dots <- list(...)
        if (side == 2)
          axis.trace$values <- c(axis.trace$values, dots$cex.axis)
      }))
hydroplot(x, ptype="ts", pfreq="o", cex.axis=0.65)
hydroplot(x, FUN=mean, pfreq="seasonal", cex.axis=0.85)
untrace("axis", where=asNamespace("hydroTSM"))

grDevices::dev.off()

stopifnot(
  same.par(before, after.regular),
  same.par(before, after.seasonal),
  inherits(plot.error, "try-error"),
  same.par(before, after.error),
  identical(panel.positions[, 1], seq_len(4)),
  identical(panel.positions[, 2], rep(1L, 4)),
  identical(panel.positions[, 3:4], matrix(rep(c(4L, 1L), each=4),
                                          nrow=4)),
  !inherits(data.frame.plot, "try-error"),
  !inherits(subdaily.plot, "try-error"),
  all(axis.trace$values %in% c(0.65, 0.85)),
  sum(axis.trace$values == 0.65) >= 1,
  sum(axis.trace$values == 0.85) >= 4,
  file.exists(pdf.file), file.info(pdf.file)$size > 0
)
