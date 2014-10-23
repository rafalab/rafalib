#' as factor then numeric
#'
#' converts to factors then to numeric
#'
#' @param x any vector
#' @param levels optional
#'
#' @examples
#'
#' as.fumeric(c("a","x","x","y"))
#' 
as.fumeric <- function(x,levels=unique(x)) as.numeric(factor(x,levels=levels))
