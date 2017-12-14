#' plot coefficients
#' 
#' plot coefficients of a gene given a DESeqDataSet (DESeq2 > v1.16) 
#' 
#' @param dds DESeq2 object (DESeqDataSet) 
#' @param geneName name of the gene to be plotted, needs to be in the rownames of dds
#' @param legend (TRUE/FALSE, default TRUE), whether or not to plot the legend inside the plot.
#'    the position is chosen automatically based on the last coefficient to be plotted. 
#' @author Bernd Jagla, Institut Pasteur, Paris, France
#' 
#' @examples
#' 
#' library(DESeq2)
#' dds <-  dds <- makeExampleDESeqDataSet()
#' dds = DESeq(dds)
#' plotCoefficients(dds, "gene930")
#' 
plotCoefficients <- function(dds, geneName, legend=TRUE){
  # find ylim and number of arrow to plot per slot
  # bp_names = names of the slots/columns
  # dds = DESeq2 object
  # intgrp = expanded list of elements from the design
  # coefs = coefficients from dds
  
 
  if(!class(dds)=="DESeqDataSet"){
    warning("dds is not of class DESeqDataSet")
  }
  if(!geneName %in% rownames(dds)){
    warning("geneName is not in dds")
  }
  
  arrowDist = 0.1 # distance between arrows
  
  # individual terms of the formula
  intgrp = attr(terms.formula(DESeq2::design(dds)), "term.labels")
  
  if(length(grep("\\.",names(dds@colData@listData))) > 0 ){
    warning("column names of factors shouldn't contain '.'")
  }
  
  # check that intgrp has no "." in one of its names
  intgrpNoInt = intgrp[!grepl(":", intgrp)]
  
  # gets the transformed counts from dds object and the conditions needed for plotting
  # also contains the sample names
  gene1Counts = plotCounts(dds, gene=geneName, 
                           intgroup=intgrpNoInt, 
                           returnData=T,
                           transform = T)
  # normalize (should use rlog???)
  gene1Counts[rownames(gene1Counts), "count"] = log2(gene1Counts[rownames(gene1Counts), "count"]+1)
  countsName=names(gene1Counts)[1]
  coefs = as.data.frame(coef(dds[geneName]))
  
  # brewer can only handle 12 different colors
  if(length(coef) > 12){
    warning("more than 12 coefficients")
  }
  if(length(coefs) < 8){
    if(length(coefs)<3){
      cols <- brewer.pal(3,"Dark2")
    }else{
      cols <- brewer.pal(length(coefs),"Dark2")
    }
  }else{
    cols <- brewer.pal(length(coefs),"Paired")
  }
  
  
  # calculate some values for defining the plot
  coldat = colData(dds)
  xlim = 1
  for (idx in 1: length(intgrpNoInt)){
    xlim = xlim * length(levels(coldat[,intgrpNoInt[idx]]))
  }
  
  # we use boxplot to get the names of the columns in the strip chart below.
  bp <- boxplot(formula(paste(countsName, 
                              paste(as.character(DESeq2::design(dds)), collapse=""))), 
                data = gene1Counts, plot = F)
  bp_names = bp$names
  
  # find ylim and number of arrow to plot per slot
  plotParameters = ._findplotParameters(bp_names, dds, intgrpNoInt, coefs)
  ylim = plotParameters[[1]]
  arrowCount = plotParameters[[2]]
  legendPos = plotParameters[[3]]
  
  # base plot
  stripchart(formula(paste(countsName, paste(as.character(DESeq2::design(dds)), collapse = ""))), 
             data=gene1Counts, vertical=TRUE,las=2, method="jitter",
             cex.axis=0.7, main=paste(geneName), 
             ylim=ylim)
  
  lgth <- .1 # length of the edges of the arrow head (in inches).
  abline(h=0) # reference line (intercept)
  pos = 1 
  # first one is the Intercept so we handle this separately
  a = arrowCount[1] * arrowDist * -1
  arrows(pos+a,0,pos+a,coefs[["Intercept"]],lwd=3,col=cols[1],length=lgth)
  abline(h=coefs[["Intercept"]],col=cols[1])
  
  for (colIdx in 2:length(bp_names)){
    a = arrowCount[colIdx] * arrowDist * -1 # x position of arrow
    fctLevels = strsplit(bp_names[colIdx],"\\.")[[1]] # actual elements of the factor
    rIdx = 1 # index in which factor is being looked at
    baseLine = coefs[["Intercept"]]
    #non interaction terms
    coefIdx = rep(1, length(fctLevels)) # coefficient index
    for(fctIdx in 1:length(fctLevels)){
      lIdx = which(levels(dds@colData@listData[[intgrp[rIdx]]]) == fctLevels[fctIdx])
      coefIdx[fctIdx] = lIdx
      if(lIdx>1){ #need to print arrow
        coefName = paste(intgrp[rIdx], fctLevels[fctIdx],"vs", levels(dds@colData@listData[[intgrp[rIdx]]])[1], sep = "_")
        arrows(colIdx+a,baseLine,colIdx+a,baseLine+coefs[[coefName]],
               lwd=3,col=cols[which(colnames(coefs) ==coefName)],length=lgth)
        baseLine = baseLine + coefs[[coefName]]
        a = a + arrowDist
      }
      # interaction terms (only the case if min 2 are not reference)
      if(sum(coefIdx>1)>1){
        # check if current is part of the factors 
        # since interactions are build by concatenating the strings we can do the following:
        interName1 = paste0(intgrp[rIdx], fctLevels[fctIdx])
        interName1Pos = grep(interName1, colnames(coefs))
        if(length(interName1Pos) > 0) {
          interName2 = gsub(interName1, "", colnames(coefs)[interName1Pos])
          interName2 = gsub("\\.", "", interName2)
          coefName = ""
          for(fctIdx2 in 1:length(fctLevels)){
            if(grep(fctLevels[fctIdx],bp_names[colIdx])>0 & coefIdx[fctIdx2]>1){ # needs to be in the name of column we are working on & not reference
              coefName = paste(coefName, paste0(intgrp[fctIdx2], fctLevels[fctIdx2]), sep = ".")
            }
          }
          coefName = substring(coefName, 2) # remove the first "."
          arrows(colIdx+a,baseLine,colIdx+a,baseLine+coefs[[coefName]],
                 lwd=3,col=cols[which(colnames(coefs) ==coefName)],length=lgth)
          baseLine = baseLine + coefs[[coefName]]
          a = a + arrowDist
        }
      }
      rIdx = rIdx + 1
    }
  }
  if(legend){
    legend(legendPos, legend=names(coefs), 
           text.col = cols, cex = 0.7,
           merge = F)
  }
}

# only for internal usage
._findplotParameters <- function(bp_names,dds,intgrp, coefs){
  arrowCount = rep(0,length(bp_names))
  ylim1 = min(0,coefs[[1]])
  ylim2 = max(0,coefs[[1]])
  arrowCount[1] = 1
  for (colIdx in 2:length(bp_names)){
    fctLevels = strsplit(bp_names[colIdx],"\\.")[[1]] # actual elements of the factor
    rIdx = 1 # index in which factor is being looked at
    baseLine = coefs[["Intercept"]]
    #non interaction terms
    coefIdx = rep(1, length(fctLevels))
    for(fctIdx in 1:length(fctLevels)){
      lIdx = which(levels(dds@colData@listData[[intgrp[rIdx]]]) == fctLevels[fctIdx])
      coefIdx[fctIdx] = lIdx
      if(lIdx>1){ #need to print arrow
        coefName = paste(intgrp[rIdx], fctLevels[fctIdx],"vs", levels(dds@colData@listData[[intgrp[rIdx]]])[1], sep = "_")
        baseLine = baseLine + coefs[[coefName]]
        ylim1 = min(ylim1, baseLine)
        ylim2 = max(ylim2, baseLine)
        arrowCount[colIdx] = arrowCount[colIdx] + 1
      }
      # interaction terms (only the case if min 2 are not reference)
      if(sum(coefIdx>1)>1){
        # check if current is part of the factors 
        # since interactions are build by concatenating the strings we can do the following:
        interName1 = paste0(intgrp[rIdx], fctLevels[fctIdx])
        interName1Pos = grep(interName1, colnames(coefs))
        if(length(interName1Pos) > 0) {
          interName2 = gsub(interName1, "", colnames(coefs)[interName1Pos])
          interName2 = gsub("\\.", "", interName2)
          coefName = ""
          for(fctIdx2 in 1:length(fctLevels)){
            if(grep(fctLevels[fctIdx],bp_names[colIdx])>0 & coefIdx[fctIdx2]>1){ # needs to be in the name of column we are working on & not reference
              coefName = paste(coefName, paste0(intgrp[fctIdx2], fctLevels[fctIdx2]), sep = ".")
            }
          }
          coefName = substring(coefName, 2) # remove the first "."
          baseLine = baseLine + coefs[[coefName]]
          ylim1 = min(ylim1, baseLine)
          ylim2 = max(ylim2, baseLine)
          arrowCount[colIdx] = arrowCount[colIdx] + 1
        }
      }
      rIdx = rIdx + 1
    }
  }
  if(baseLine > ylim2/2){legendPos = "bottomright"}else{legendPos = "topright"}
  return(list(c(floor(ylim1), ceiling(ylim2)), arrowCount, legendPos))
}# end of findplotParameters()

