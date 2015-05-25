#' bartab
#'
#' plot the overlap of three groups with a barplot
#'
#' @param x logical
#' @param y logical
#' @param z logical
#' @param names a character vector of length 3
#' @param ... to barplot
#' 
bartab <- function(x,y,z,names,...) {
  x <- factor(x,c("FALSE","TRUE"))
  y <- factor(y,c("FALSE","TRUE"))
  z <- factor(z,c("FALSE","TRUE"))
  tabs <- as.vector(table(x, y, z))
  names(tabs) <- c("none",names[1],names[2],paste(names[1:2],collapse="+"),
                   names[3],paste(names[c(1,3)],collapse="+"),
                   paste(names[2:3],collapse="+"),"all")
  tabs <- tabs[c(1,2,3,5,4,6,7,8)]
  barplot(tabs, ...)
}
