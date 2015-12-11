#' peek at the top of a text file
#'
#' this returns a character vector which shows the top n lines of a file.
#' Note: I realized after the fact that this is essentially a duplicate
#' of the base R function \code{readLines}.
#'
#' @param x a filename
#' @param n the number of lines to return
#' 
#' @author Michael I. Love
#' 
#' @examples
#' 
#' filename <- tempfile()
#' x<-matrix(round(rnorm(10^4),2),1000,10)
#' colnames(x)=letters[1:10]
#' write.csv(x,file=filename,row.names=FALSE)
#' peek(filename)
peek <- function(x,n=2) scan(x,what="char",n=n,sep="\n")
