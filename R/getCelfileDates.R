getCelfileDates <- function(filenames,...){
    chardates<-vector("character",length(filenames))
    for(i in seq(along=chardates)){
        tmp<-affyio::read.celfile.header(filenames[i],info="full")
        chardates[i]<-strsplit(tmp$ScanDate,"T|\ ")[[1]][1]
     
    }
    dates<-as.Date(rep(NA,length(chardates)))
    ind <- grep("-",chardates)
    if(length(ind)>0) dates[ind]<-as.Date(chardates[ind],"%Y-%m-%d")
    ind <- grep("/",chardates)
    if(length(ind)>0) dates[ind]<-as.Date(chardates[ind],"%m/%d/%y")

    return(dates)
}
