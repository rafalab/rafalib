mypar2 <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",mar=c(2.5,2.5,1.6,1.1),mgp=c(1.5,.5,0),...){
    require(RColorBrewer)
    par(mar=mar,mgp=mgp)
    par(mfrow=c(a,b),...)
    palette(brewer.pal(brewer.n,brewer.name))
}


##this is for R studio
mypar <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",cex.lab=2,cex.main=2,cex.axis=1.5,mar=c(5.1,4.6,3.5,2.1),mgp=c(3,1,0),...){
    require(RColorBrewer)
    par(mar=mar,mgp=mgp,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
    par(mfrow=c(a,b),...)
    palette(brewer.pal(brewer.n,brewer.name))
}
mypar(1,1)
plot(1,1,xlab="caca",ylab="Caca",main="Caca")
