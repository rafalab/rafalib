mypar <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",...){
    require(RColorBrewer)
    par(mar=c(2.75,2.5,1.6,1.1),mgp=c(1.5,.5,0))
    par(mfrow=c(a,b),...)
    palette(brewer.pal(brewer.n,brewer.name))
}

