#' What are the largest objects in memory?
#'
#' @param n the number of objects to return
#' @param units units to display, see \code{?object.size}
#'
#' @return a character string of the 'n' largest objects
#' 
largeobj <- function(n=5, units="Mb") {
  objs <- sapply(ls(envir=.GlobalEnv), function(x) object.size(get(x)))
  o <- order(objs, decreasing=TRUE)
  out <- sapply(ls(envir=.GlobalEnv), function(x) format(object.size(get(x)),units=units))
  head(out[o], n=n)
}
