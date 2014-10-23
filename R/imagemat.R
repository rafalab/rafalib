#' image of a matrix
#'
#' produces an image of a matrix which matches
#' the natural orientation
#'
#' @param x the matrix
#' @param col the colors
#' @param ... arguments passed to image
#'
#' @examples
#'
#' x <- matrix(c(1,0,0,0,1,
#'               1,1,0,1,1,
#'               1,0,1,0,1,
#'               1,0,0,0,1,
#'               1,0,0,0,1),
#' ncol=5,byrow=TRUE)
#'
#' imagemat(x)
#'
imagemat <- function(x,col=colorRampPalette(c("white","black"))(9),...) {
  image(1:ncol(x),1:nrow(x),t(x[nrow(x):1,]),col=col,xlab="",ylab="")
}
