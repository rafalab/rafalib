getCelfileDates <- function(filenames,...){
    dates<-vector("character",length(filenames))
    for(i in seq(along=dates)){
        tmp<-affyio::read.celfile.header(filenames[i],info="full")
        dates[i]<-strsplit(tmp$ScanDate,"\ ")[[1]][1]
    }
    dates<-as.Date(dates,"%m/%d/%y")
    return(dates)
}
