library(hydroTSM)

################################################################################
# matrixplot                                                                  #
################################################################################

x <- matrix(1:12, nrow=3)
colnames(x) <- month.abb[1:4]
rownames(x) <- paste0("S", 1:3)

p <- matrixplot(x, cuts=seq(1, 13, length.out=11), main="matrixplot test")

stopifnot(inherits(p, "trellis"))

pdf.file <- tempfile(fileext=".pdf")
grDevices::pdf(pdf.file)
print(p)
grDevices::dev.off()

stopifnot(file.exists(pdf.file), file.info(pdf.file)$size > 0)
