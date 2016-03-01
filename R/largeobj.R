#' What are the largest objects in memory?
#'
#' This function lists all the objects in the global environmnet and lists the \code{n} largest.
#' 
#' @param n the number of objects to return
#' @param units units to display, see \code{?object.size}
#'
#' @return a named character string of the size of the 'n' largest objects
#' 
#' @author Michael I. Love
#' 
#' @examples
#' x<-rnorm(10^5)
#' y<-rnorm(10^6)
#' z<-rnorm(2*10^6)
#' w<-rnorm(3*10^6)
#' largeobj(n=3)

largeobj <- function(n=5, units="Mb") {
  # use 'aaa' here in anonymous function because there seems
  # to be an issue with actual objects named same as the anon fn's argument
  objs <- sapply(ls(envir=.GlobalEnv), function(aaa) object.size(get(aaa)))
  o <- order(objs, decreasing=TRUE)
  out <- sapply(ls(envir=.GlobalEnv), function(aaa) format(object.size(get(aaa)),units=units))
  head(out[o], n=n)
}
