writeHMSParams <- function(hmsDF, outFile) {
  
  file.create(paste0(outFile))
  
  fileCon <- file(paste0(outFile))
  
  toWriteVec <- c()
  
  for(i in 1:nrow(hmsDF)) {
    
    toWrite <- hmsDF[i, ]
    
    toWrite <- toWrite[, !is.na(toWrite)]
    
    for(j in 1:(ncol(toWrite) + 1)) {
      
      if(j == 1) {
        
        toWrite2 <- paste(toWrite[1, 1], toWrite[1, 2], sep = ": ")
        
        toWriteVec <- c(toWriteVec, toWrite2)
        
      } else if (j == 2) {
        
        toWrite2 <- NA
        
      } else if (j > 2 & j < (ncol(toWrite) + 1)) {
        
        toWrite2 <- paste0("", names(toWrite[j]), ": ", toWrite[1, j])
        
        toWriteVec <- c(toWriteVec, toWrite2)
        
      } else {
        
        toWrite2 <- c("End:", "")
        
        toWriteVec <- c(toWriteVec, toWrite2)
        
      }
    }
  }
  
  writeLines(toWriteVec, fileCon)
  
  close(fileCon)
  
}
