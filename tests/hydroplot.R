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

grDevices::dev.off()

stopifnot(
  same.par(before, after.regular),
  same.par(before, after.seasonal),
  inherits(plot.error, "try-error"),
  same.par(before, after.error),
  file.exists(pdf.file), file.info(pdf.file)$size > 0
)
