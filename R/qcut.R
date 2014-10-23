qcut <- function(x, n, ...) {
  qs <- quantile(x, (0:n)/n)
  qs[1] <- qs[1] - 1
  qs[length(qs)] <- qs[length(qs)] + 1
  cut(x, qs, ...)
}
