#' peek at the top of a text file
#'
#' this returns a character vector which shows the top n lines of a file
#'
#' @param x a filename
#' @param n the number of lines to return
#' 
peek <- function(x,n=2) scan(x,what="char",n=n,sep="\n")
