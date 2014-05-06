qcut <- function(x, n, ...) {
  cut(x, quantile(x, c( min(x) - 1, ( 0:(n - 3) )/( n - 3 ), max(x) + 1)), ...)
}
