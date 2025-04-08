#' population variance 
#' 
#'  
#' Returns the population variance. Note that \code{\link{var}} returns
#' the unbiased sample estimate of the population variance. 
#' It simply multiplies the result of \code{\link{var}} by (n-1)  / n with n 
#' the population size.
#' 
#' @param x a numeric vector, matrix or data frame.
#' @param ... further arguments passed along to \code{\link{var}}
#' 
#' @examples
#' x <- c(0,1) ##variance should be 0.5^2=0.25
#' var(x) 
#' popvar(x)
#' @importFrom stats var
#' @export

popvar <- function(x,...){
	if(is.matrix(x)){
		n <- nrow(x) } else{
		if(is.atomic(x)) n <- length(x)
		}
	var(x,...)*(n-1)/n
	}

#' population standard deviation
#' 
#'  
#' Returns the population standard deviation. Note that \code{\link{sd}} returns
#' the unbiased sample estimate of the population standard deviation. 
#' It simply multiplies the result of \code{\link{var}} by (n-1)  / n with n 
#' the population size and takes the square root.
#' 
#' @param x a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}.
#' @param na.rm logical. Should missing values be removed?
#' 
#' @export

popsd <- function(x, na.rm=FALSE){
	sqrt(popvar(if (is.vector(x)) x else as.double(x), na.rm = na.rm))

}
