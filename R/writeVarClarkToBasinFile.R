## This function takes a .basin file that has all of its subbasins in either Normal Clark or 
## variable Clark transforms and changes the transforms methods to variable Clark with the index
## values in the data frame 'varClarkDF' or updates the existing variable Clark index parameters
## with the index values in the data frame 'varClarkDF'. The data frame 'varClarkDF' consists of 
## 4 columns. The columns are 'Subbasin', 'indexPrecip', 'tc', and 'r' and must be named appropriately. 
## Also, you should have a DSS file set up with the appropriate tables, as links will be created in 
## the .basin files. If the subbasin name is 'theSubbasin', the link to the TC table will be 
## 'theSubbasin_TC' and the link to the R table will be 'theSubbasin_R'. 

addVarClark <- function(basinFile, varClarkDF) {
  
  #basinFile <- readLines(basinFile)
  
  typeRf <- c("Source:", "Basin:", "Reservoir:", "Reach:", "Subbasin:", "Junction:", 
              "Diversion:", "Sink:", "Zone Configuration:", "Computation Point:", 
              "Basin Schematic Properties:", "Basin Spatial Properties:", "Basin Layer Properties:")
  
  endRf <- c("End:", "End Zone Configuration:", "End Computation Point:") 
  
  findType <- grepl(paste(typeRf, collapse = "|"), basinFile)
  
  findEndRf <- grepl(paste(endRf, collapse = "|"), basinFile)
  
  findSubbasins <- grepl("Subbasin:", basinFile)
  
  subbasinRfs <- which(findSubbasins == TRUE)
  
  bsnFlEndRf <- which(findEndRf == TRUE)
  
  for (i in 1:length(subbasinRfs)) {
    
    bsnFlTypeTst <- subbasinRfs[i] 
    
    bsnFlEndRfSub <- bsnFlEndRf[which(bsnFlEndRf > bsnFlTypeTst)]
    
    bsnFlEndRfSub <- bsnFlEndRfSub[1]
    
    bsnFlSub <- basinFile[bsnFlTypeTst:bsnFlEndRfSub]
    
    trnsfrmMthd <- grepl("Transform:", bsnFlSub)
    
    trnsfrmMthd <- which(trnsfrmMthd == TRUE)
    
    trnsfrmMthd_ <- grepl("", bsnFlSub)
    
    trnsfrmMthd_ <- which(nchar(bsnFlSub) == 0)
    
    trnsfrmMthd_ <- trnsfrmMthd_[which(trnsfrmMthd_ > trnsfrmMthd)][1]
    
    transformSub <- bsnFlSub[trnsfrmMthd:trnsfrmMthd_]
    
    clrkMthd <- transformSub[grepl("Clark Method:", transformSub)]
    
    sbbsnNm <- stringr::str_remove(bsnFlSub[1], "Subbasin: ")
    
    sbbsnNmUpr <- toupper(sbbsnNm)
    
    varClarkSub <- dplyr::filter(varClarkDF, Subbasin == sbbsnNmUpr)
    
    retBasinFile <- basinFile
    
    if (stringr::str_detect(clrkMthd, "Specified")) {
      
      newClrkMthd <- c("     Transform: Clark", 
                       "     Clark Method: Variable", 
                       paste0("     Time of Concentration: ", signif(varClarkSub$tc, 3)), 
                       paste0("     Storage Coefficient: ", signif(varClarkSub$r, 3)), 
                       "     Time Area Method: Default", 
                       paste0("     Index Excess: ", signif(varClarkSub$indexPrecip, 2)), 
                       paste0("     Excess-Tc Percentage Curve: ", paste0(varClarkSub$Subbasin, "_TC")), 
                       paste0("     Excess-R Percentage Curve: ", paste0(varClarkSub$Subbasin, "_R")))
      
      pos1 <- (bsnFlTypeTst + trnsfrmMthd) - 1
      
      pos2Old <- pos1 + 5
      
      pos2New <- pos2Old + 3
      
      #basinFileT <- basinFile
      
      basinFile <- c(basinFile[1:(pos1 - 1)], newClrkMthd, basinFile[pos2Old:(length(basinFile))])
      
      subbasinRfs <- subbasinRfs + 3
      
      bsnFlEndRf <- bsnFlEndRf + 3
      
    } else {
      
      newClrkMthd <- c("     Transform: Clark", 
                       "     Clark Method: Variable", 
                       paste0("     Time of Concentration: ", signif(varClarkSub$tc, 3)), 
                       paste0("     Storage Coefficient: ", signif(varClarkSub$r, 3)), 
                       "     Time Area Method: Default", 
                       paste0("     Index Excess: ", signif(varClarkSub$indexPrecip, 2)), 
                       paste0("     Excess-Tc Percentage Curve: ", paste0(varClarkSub$Subbasin, "_TC")), 
                       paste0("     Excess-R Percentage Curve: ", paste0(varClarkSub$Subbasin, "_R")), 
                       "")
      
      pos1 <- (bsnFlTypeTst + trnsfrmMthd) - 1
      
      pos2 <- pos1 + 9
      
      basinFile <- c(basinFile[1:(pos1 - 1)], newClrkMthd, basinFile[pos2:(length(basinFile))])
      
    }
    
  }
  
  return(basinFile)
  
}
