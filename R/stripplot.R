#' Better defaults for stripchart
#'
#' This simply calls \code{stripchart} but specifies
#' a vertical plot with jitter and using \code{pch=1}.
#' 
#' @param ... passed to \code{stripchart}
#'
#' @return a plot
#'
#' @export
stripplot <- function(...) {
  stripchart(..., vertical=TRUE, pch=1, method="jitter")
}
