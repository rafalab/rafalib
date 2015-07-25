#' quantile cut
#'
#' \code{qcut} calls \code{\link{cut}} with the quantiles of \code{x} as breaks.
#'
#' @param x a numeric vector
#' @param n the number of quantiles 
#' @param ... further arguments passed on to \code{\link{cut}}
#' @return a factor with n levels, from cutting x into quantiles
#' (0/n, 1/n, 2/n, ..., 1) of x. the first and last quantile have
#' 1 subtracted and added respectively so all data falls into the bins.
#' @seealso \code{\link[lsr]{quantileCut}} from the lsr package and
#' \code{\link[classInt]{classIntervals}} from the classInt package
#' @examples
#'
#' qcut(rnorm(10), 4)
#' 
qcut <- function(x, n, ...) {
  # qcut(x,n) produces a factor from x by cutting x along quantiles 0/n, 1/n, 2/n, ..., 1
  qs <- quantile(x, (0:n)/n)
  qs[1] <- qs[1] - 1
  qs[length(qs)] <- qs[length(qs)] + 1
  cut(x, qs, ...)
}
