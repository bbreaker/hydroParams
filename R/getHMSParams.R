getHMSParams <- function(basinFile, type) {
  
  basinFile <- readLines(basinFile)
  
  endRf <- "End:"
  
  if(type == "all") {
    typeRf <- c("Source:", "Basin:", "Reservoir:", "Reach:", "Subbasin:", "Junction:", "Diversion:", "Sink:")
  } else {
    typeRf <- paste0(type, ": ")
  }
  
  findType <- grepl(paste(typeRf, collapse = "|"), basinFile)
  
  findEndRf <- stringr::str_detect(basinFile, endRf)
  
  bsnFlType <- which(findType == TRUE)
  
  bsnFlEndRf <- which(findEndRf == TRUE)
  
  refDF <- data.frame()
  
  for (i in 1:length(bsnFlType)) {
    
    bsnFlTypeTst <- bsnFlType[i] 
    
    bsnFlEndRfSub <- bsnFlEndRf[which(bsnFlEndRf > bsnFlTypeTst)]
    
    bsnFlEndRfSub <- bsnFlEndRfSub[1]
    
    refDFSub <- basinFile[bsnFlTypeTst:bsnFlEndRfSub]
    
    refDFWide <- data.frame(fileN = 1)
    
    for (j in 1:length(refDFSub)) { 
      
      if(j == 1) {
        
        refDFWide_ <- data.frame(Type = (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[1]), 
                                 Name = (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[2]), 
                                 stringsAsFactors = FALSE)
        
        refDFWide_ <- na.omit(refDFWide_)
        
        refDFWide <- dplyr::bind_cols(refDFWide, refDFWide_)
        
      } else {
        
        refDFWide_ <- data.frame(val = (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[2]), 
                                 stringsAsFactors = FALSE)
        
        refDFWide_ <- na.omit(refDFWide_)
        
        names(refDFWide_) <- (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[1])
        
        if(nrow(refDFWide_) == 0) {
          refDFWide <- refDFWide
        } else {
          refDFWide <- dplyr::bind_cols(refDFWide, refDFWide_, .name_repair = "minimal")
        }
        
      }
      
    }
    
    refDFWide <- refDFWide[, -1] 
    
    #refDFWide <- refDFWide[, colSums(is.na(refDFWide)) < nrow(refDFWide)] 
    
    refDF <- dplyr::bind_rows(refDF, refDFWide)
    
  } 
  
  return(refDF)
  
}
