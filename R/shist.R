shist <- function(z, unit = 1, plotHist = FALSE, add = FALSE,
                  xlim, ylim, main, ...) {
  n <- length(z)
  if(missing(main)){
      main <- paste("Shistogram of", deparse(substitute(z)))
  }
  d <- density(z)
  ymax <- max(d$y * n * unit)
  if (missing(xlim)) {
    xlim <- c(min(z) - unit, max(z) + unit)
  }
  if (missing(ylim)) {
    ylim <- c(0, 1.1 * ymax)
  }
  if (plotHist) {
    h <- hist(z, breaks = seq(from = min(z) - unit, to = max(z) + unit, 
                   by = unit), plot = FALSE)
    ylim[2] <- max(ylim[2], 1 * 1 * h$count)
    plot(h, col = "grey", main = main, xlab = deparse(substitute(z)), ylim = ylim, 
         xlim = xlim)
  } else {
    if (!add) {
      plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = deparse(substitute(z)), 
           ylab = "Frequency", main = main)
    }
  }
  
  # here is the one line of shist-ty code:
  lines(d$x, d$y * n * unit, ...)
  
  if (!add) {
    arrows(max(z) - unit, ymax, max(z), ymax, angle = 90, code = 3, length = 0.1)
    text(max(z)-unit, ymax, paste("unit =", unit), pos = 2)
  }
}
