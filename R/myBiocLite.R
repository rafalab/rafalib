myBiocLite <- function(...){
    internet <- try(source("http://bioconductor.org/biocLite.R"), silent=TRUE) 
    if(!class(internet)=="try-error") 
        { 
            biocLite(...)
        } else 
            { 
                stop("connection to http://bioconductor.org not successfull\n") 
            } 
}

