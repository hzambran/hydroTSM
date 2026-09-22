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
  file.exists(pdf.file), file.info(pdf.file)$size > 0
)
