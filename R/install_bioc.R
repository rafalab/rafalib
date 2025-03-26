#' Install or update Bioconductor and CRAN packages
#' 
#' This function is simply a wrapper for \code{link[BiocManager]{install}}. If \code{BiocManager} is not installed it it automatically installed. 
#' 
#' @param ... arguments passed on to \code{link[BiocManager]{install}}
#' 
#' @author Rafael A. Irizarry
#' 
#' @details If \code{BiocManager} is installed you can simply call \code{BiocManager::install} instead. 
#' 
#' @examples
#' \donttest{
#'   install_bioc("affy")
#' }
#' @export

install_bioc <- function(...) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop("The BiocManager package is required. Please install it using install.packages('BiocManager').")
  }
  
  result <- try(BiocManager::install(...), silent = TRUE)
  if (!inherits(result, "try-error")) {
    message("Installation via BiocManager was successful.")
    return(invisible(result))
  } else {
    stop("Installation via BiocManager::install failed.\n")
  }
}
