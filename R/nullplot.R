nullplot <- function(x1=0,x2=1,y1=0,y2=1,xlab="",ylab="",...) {
  plot(0,0,xlim=c(x1,x2),ylim=c(y1,y2),type="n",xlab=xlab,ylab=ylab,...)
}
