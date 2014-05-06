qcut <- function(x, n, ...) {
  cut(x, quantile(x, ( 0:(n - 1) )/( n - 1 ),...))
}
