popvar <- function(x,...){
	if(is.matrix(x)){
		n <- nrow(x) } else{
		if(is.atomic(x)) n <- length(x)
		}
	var(x,...)*(n-1)/n
	}
	
popsd <- function(x, na.rm=FALSE){
	sqrt(popvar(if (is.vector(x)) x else as.double(x), na.rm = na.rm))

}
