# File hydropairs.R
# Part of the hydroTSM R package, http://www.rforge.net/hydroTSM/ ; 
#                                 http://cran.r-project.org/web/packages/hydroTSM/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'hydropairs' : Visualization of a Correlation Matrix                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 29-Jul-2009                                                         #
################################################################################
# On top the (absolute) value of the correlation plus the result of the cor.test as points. 
# On botttom, the bivariate scatterplots, with a fitted line
# On diagonal, histograms (from '?pairs')

# Original idea taken from: http://addictedtor.free.fr/graphiques/graphcode.php?graph=137
# Histogram panles was taken form the R help of the original 'pairs' function

# x     : a numeric vector, matrix or data frame
# dec   : decimals places to be used for showing the correlation values

# use   : an optional character string giving a method for computing
#          covariances in the presence of missing values.  This must be
#          (an abbreviation of) one of the strings '"everything"',
#          '"all.obs"', '"complete.obs"', '"na.or.complete"', or
#          '"pairwise.complete.obs"'.

# method: a character string indicating which correlation coefficient
#          (or covariance) is to be computed.  One of '"pearson"'
#          (default), '"kendall"', or '"spearman"', can be abbreviated.

hydropairs <- function(x, dec=3, use="pairwise.complete.obs", method="pearson",... ) {

  # Checking that the user provied a valid argument for 'x'
  if ( is.na( match( class(x), c("matrix", "data.frame") ) ) )
      stop("Invalid argument: 'class(x)' must be in c('data.frame')")

  panel.cor <- function(x, y, digits=dec, prefix="", cex.cor)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))

        r <- abs(cor(x, y, method= method, use= use))

        txt <- format(c(r, 0.123456789), digits=dec)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

        test <- cor.test(x,y)
        # borrowed from printCoefmat
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                      symbols = c("***", "**", "*", ".", " "))

        text(0.5, 0.5, txt, cex = cex * r)
        text(.8, .8, Signif, cex=cex, col=2)
    } # 'panel.cor' END

  panel.hist <- function(x, ...)
         {
             usr <- par("usr"); on.exit(par(usr))
             par(usr = c(usr[1:2], 0, 1.5) )
             h <- hist(x, plot = FALSE)
             breaks <- h$breaks; nB <- length(breaks)
             y <- h$counts; y <- y/max(y)
             rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
         } # 'panel.hist' END


  # 'font.labels' =2 : bold font for the variables
  # 'cex.labels' controls the size of the fonts
  pairs(x, lower.panel=panel.smooth, upper.panel=panel.cor,
        diag.panel=panel.hist, ...)

} # 'hydropairs' END
