shist <- function(z, unit,
                  bw="nrd0", n, from, to,
                  plotHist = FALSE, add = FALSE,
                  xlab, ylab = "Frequency",
                  xlim, ylim, main, ...) {

  if (is.data.frame(z) && is.numeric(as.matrix(z))) {
    z <- as.matrix(z)
  }
  if (is.matrix(z) && ncol(z) == 1) {
    z <- as.vector(z)
  }
  stopifnot(is.vector(z) | is.matrix(z))
  
  if (missing(xlab)) {
    xlab <- deparse(substitute(z))
  }
  if (missing(main)) {
    main <- paste("Shistogram of", deparse(substitute(z)))
  }
  
  if (is.vector(z)) {
    if (missing(unit)) {
      unit <- .5 * sd(z)
    }
  } else if (is.matrix(z)) {
    if (missing(unit)) {
      unit <- .5 * mean(apply(z, 2, sd))
    }
  }

  shist.onesample <- function(z, unit, bw, n, from, to,
                              plotHist, add, xlab, ylab,
                              xlim, ylim, main, maxz, ...) {
    lz <- length(z)
    if (missing(n)) {
      n <- 512
    }
    d <- density(z, bw=bw, n=n, from=from, to=to)
    ymax <- max(d$y * lz * unit)
    if (missing(xlim)) {
      xlim <- c(min(z) - unit, max(z) + unit)
    }
    if (missing(ylim)) {
      ylim <- c(0, 1.2 * ymax)
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
    lines(d$x, d$y * lz * unit, ...)
    if (!add) {
      arrows(maxz - unit, 1.1*ymax, maxz, 1.1*ymax, angle = 90, code = 3, length = 0.1)
      text(maxz - unit, 1.1*ymax, paste("unit =", sprintf("%.2g", unit)), pos = 2)
    }
  }

  if (is.vector(z)) {
    shist.onesample(z=z, unit=unit, bw=bw, n=n, from=from, to=to,
                    plotHist=plotHist, add=add, xlab=xlab, ylab=ylab,
                    xlim=xlim, ylim=ylim, main=main, maxz=max(z), ...)
  } else if (is.matrix(z)) {
    if (missing(xlim)) {
      xlim <- c(min(z) - unit, max(z) + unit)
    }
    shist.onesample(z=z[,1], unit=unit, bw=bw, n=n, from=from, to=to,
                    plotHist=FALSE, add=FALSE, xlab=xlab, ylab=ylab,
                    xlim=xlim, ylim=ylim, main=main, maxz=max(z), ...)
    for (j in 2:ncol(z)) {
      shist.onesample(z=z[,j], unit=unit, bw=bw, n=n, from=from, to=to,
                      plotHist=FALSE, add=TRUE, ...)
    }
  }
}
