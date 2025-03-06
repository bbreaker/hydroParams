getPDataFile <- function(pDataFile, fixHeader = TRUE) {
  
  pFile <- readLines(pDataFile)
  
  beginRef <- "Table: "
  
  endRf <- "End:"
  
  beginRf <- stringr::str_detect(pFile, beginRef)
  
  pFileBeginRf <- which(beginRf == TRUE)
  
  findEndRf <- stringr::str_detect(pFile, endRf)
  
  pFileFlEndRf <- which(findEndRf == TRUE)[-1]
  
  for (i in 1:length(pFileBeginRf)) {
    
    if (i == 1) {
      
      retDF <- data.frame()
      
    }
    
    pFileSub <- pFile[pFileBeginRf[i]:pFileFlEndRf[i]] 
    
    for (j in 1:(length(pFileSub) - 1)) {
      
      if (j == 1) {
        
        refDF <- data.frame()
        
      }
      
      refDFT_ <- data.frame(name = (unlist(stringr::str_split(pFileSub[j], pattern = ": "))[1]), 
                            value = (unlist(stringr::str_split(pFileSub[j], pattern = ": "))[2]), 
                            stringsAsFactors = FALSE)
      
      refDF_ <- data.frame(names = refDFT_$value)
      
      names(refDF_)[1] <- refDFT_$name
      
      if (j == 1) {
        
        refDF <- refDF_
        
      } else {
        
        refDF <- dplyr::bind_cols(refDF, refDF_)
        
      }
      
    }
    
    retDF <- dplyr::bind_rows(retDF, refDF)
    
  } 
  
  if (fixHeader == TRUE) {
    
    names(retDF) <- gsub("     ", "", names(retDF))
    names(retDF) <- gsub(" ", "_", names(retDF))
    names(retDF) <- gsub("/", "_", names(retDF))
    names(retDF) <- gsub("-", "_", names(retDF))
    
  }
  
  
  return(retDF)
  
}