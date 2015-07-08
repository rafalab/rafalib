nullplot <- function(x1,x2,y1,y2,xlab="",ylab="",...) {
  plot(0,0,xlim=c(x1,x2),ylim=c(y1,y2),type="n",xlab=xlab,ylab=ylab,...)
}
