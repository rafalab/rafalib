splot <- function(x,y,n=10000,subset=NULL,xlab=NULL,ylab=NULL,...){
    if(is.null(xlab)) xlab=deparse(substitute(x))
    if(is.null(ylab)) ylab=deparse(substitute(x))
    if(!is.null(subset)){
        x=x[subset]
        y=y[subset]
    }
    if(length(x)>n){
        ind=sample(length(x),10000)
        x=x[ind]
        y=y[ind]
    }
    plot(x,y,xlab=xlab,ylab=ylab,...)
}
