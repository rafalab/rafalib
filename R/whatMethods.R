#' What generics are defined for this S4 object?
#'
#' This is a very beta prototype, and it only works with S4 objects.
#' 
#' @references the bulk of this code was found with help from Gabe Becker within
#' \code{promptClass}
#' 
#' @param object either an object, or a character specifying
#' the name of a class
#'
#' @return a character vector of generics defined for the
#' class of the object
#'
#' @examples
#'
#' # from Advanced R: OO essentials material
#' # http://adv-r.had.co.nz/OO-essentials.html
#'
#' library(stats4) 
#' y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
#' nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
#' fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))
#' whatMethods(fit)
#'
#' @author Michael I. Love
#' 
whatMethods <- function(object) {
  cl <- if (is.character(object)) {
    object
  } else {
    if (!isS4(object)) stop("only works for S4 classes")
    class(object)
  }
  classInSig <- function(g, where, cl) {
    cl %in% unique(unlist(findMethods(g, where)@signatures))
  }
  genWithClass <- function(cl, where) {
    allgen <- getGenerics(where = where)
    ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl,
                                   where = where)))
    allgen[ok]
  }
  genWithClass(cl, find(classMetaName(cl)))
}
