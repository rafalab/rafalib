maplot <- function(x,y,n=10000,subset=NULL,xlab=NULL,ylab=NULL,
                   curve.add=TRUE,curve.col=2,curve.span=1/3,
                   curve.lwd=2,curve.n=2000,...){
    if(is.null(xlab)) xlab="A"
    if(is.null(ylab)) ylab="M"
    if(!is.null(subset)){
        x=x[subset]
        y=y[subset]
    }
    if(length(x)>n){
        ind=sample(length(x),10000)
        x=x[ind]
        y=y[ind]
    }
    m=y-x
    a=(x+y)/2
    plot(a,m,xlab=xlab,ylab=ylab,...)
    if(curve.add){
        o=order(a)
        aa=a[o]
        mm=m[o]
        o=seq(1,length(aa),len=curve.n)
        aa=aa[o]
        mm=mm[o]
        fit1=loess(mm~aa,span=curve.span,degree=1)
        lines(aa,fit1$fitted,col=curve.col,lwd=curve.lwd)
    }
}
