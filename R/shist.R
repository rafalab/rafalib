shist <- function(z, unit = .5 * sd(z),
                  bw = unit, n, from, to,
                  plotHist = FALSE, add = FALSE,
                  xlab = deparse(substitute(z)),
                  ylab = "Frequency",
                  xlim, ylim,
                  main = paste("Shistogram of", deparse(substitute(z))), ...) {
  n <- length(z)
  d <- density(z, bw=bw, n=n, from=from, to=to)
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
    plot(h, col = "grey", main = main, xlab = xlab, ylab = ylab,
         ylim = ylim, xlim = xlim)
  } else {
    if (!add) {
      plot(0, 0, type = "n", xlim = xlim, ylim = ylim, xlab = xlab,
           ylab = ylab, main = main)
    }
  }
  
  # here is the one line of shist-ty code:
  lines(d$x, d$y * n * unit, ...)
  
  if (!add) {
    arrows(max(z) - unit, ymax, max(z), ymax, angle = 90, code = 3, length = 0.1)
    text(max(z) - unit, ymax, paste("unit =", sprintf("%.3f", unit)), pos = 2)
  }
}
