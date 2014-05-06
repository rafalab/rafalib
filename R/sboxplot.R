sboxplot <- function(x, ...) {
  cuts <- c(20, 100)
  lens <- sapply(x, length)
  if (all(lens >= cuts[2])) {
    boxplot(x, range=0, ...)
  } else if (all(lens >= cuts[1] & lens < cuts[2])) {
    boxplot(x, ...)
  } else if (all(lens < cuts[1])) {
    stripchart(x, method="jitter", vertical=TRUE, pch=1, ...)
  } else {
    r <- range(unlist(x))
    d <- diff(r)
    n <- length(x)
    plot(0,0,type="n",xlab="",ylab="",xlim=c(0.5,n+.5),ylim=c(r[1] - .05 * d, r[2] + .05 * d),xaxt="n")
    for (i in seq_len(n)) {
      if (lens[i] < cuts[1]) {
        points(rep(i, lens[i]) + rnorm(lens[i], 0, .05), x[[i]])
      } else if (lens[i] >= cuts[1]) {
        box <- quantile(x[[i]], c(.25, .75))
        med <- median(x[[i]])
        polygon(i + c(-.1,-.1,.1,.1), c(box, rev(box)))
        segments(i - .1, med, i + .1, med, lwd=3)
        if (lens[i] < cuts[2]) {
          iqr <- diff(box)
          out <- box[1] - x[[i]] > iqr | x[[i]] - box[2] > iqr
          segments(i, box[1], i, min(x[[i]][!out]))
          segments(i, box[2], i, max(x[[i]][!out]))
          if (sum(out) > 0) {
            points(rep(i, sum(out)) + rnorm(sum(out), 0, .05), x[[i]][out])
          }
        } else {
          segments(i, box[1], i, min(x[[i]]))
          segments(i, box[2], i, max(x[[i]]))
        }
      }
    }
    axis(1, at=seq_len(n), labels=names(x))
  }
}
