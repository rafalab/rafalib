mypar2 <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",mar=c(2.5,2.5,1.6,1.1),mgp=c(1.5,.5,0),...){
    require(RColorBrewer)
    par(mar=mar,mgp=mgp)
    par(mfrow=c(a,b),...)
    palette(brewer.pal(brewer.n,brewer.name))
}

#' rafalib
#'
#' lots of great functions. find them by typing rafalib:: and then hitting <tab>
#' 
#' @name rafalib
#' @docType package
NA

#' mypar
#'
#' runs \code{\link{par}} with better settings (larger text)
#' 
#' @param a number of rows of plots
#' @param b number of columns of plots
#' @param ... as in par()
#' 
#' @examples
#' mypar(1,1)
#' plot(1,1,xlab="caca",ylab="Caca",main="Caca")
#' 
mypar <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",cex.lab=2,cex.main=2,cex.axis=1.5,mar=c(5.1,5.1,3.5,2.1),mgp=c(3,1,0),...){
    require(RColorBrewer)
    par(mar=mar,mgp=mgp,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
    par(mfrow=c(a,b),...)
    palette(brewer.pal(brewer.n,brewer.name))
}
