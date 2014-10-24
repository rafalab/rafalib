#' image of a matrix
#'
#' produces an image of a matrix which matches
#' the natural orientation
#'
#' @param x the matrix
#' @param col the colors
#' @param las as in par
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
imagemat <- function(x,col=colorRampPalette(c("white","black"))(9),las=1,...) {
  image(1:ncol(x),1:nrow(x),t(x),col=col,xlab="",ylab="",ylim=c(nrow(x)+.5,1),las=las,...)
}
