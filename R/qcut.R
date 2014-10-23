qcut <- function(x, n, ...) {
  # qcut(x,n) produces a factor from x by cutting x along quantiles 0/n, 1/n, 2/n, ..., 1
  qs <- quantile(x, (0:n)/n)
  qs[1] <- qs[1] - 1
  qs[length(qs)] <- qs[length(qs)] + 1
  cut(x, qs, ...)
}
