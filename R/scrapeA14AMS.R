scrapeA14AMS <- function(url) {
  
  library(rvest)
  library(dplyr) 
  library(stringr)
  
  finList <- list()
  
  txtDat <- readLines(url)
  
  txtDat <- txtDat[-1]
  
  txtRef <- c(1, which(txtDat == ""))
  
  for (i in 1:(length(txtRef) - 1)) {
    
    if (i == 1) {
      
      testDat <- txtDat[txtRef[i]:(txtRef[i + 1] - 1)]
      
    } else {
      
      testDat <- txtDat[(txtRef[i] + 1):(txtRef[i + 1] - 1)]
      
    }
    
    #testDat <- txtDat[txtRef[i]:(txtRef[i + 1] - 1)]
    
    testRow1 <- testDat[1]
    
    testRow1 <- unlist(stringr::str_split(testRow1, pattern = " "))
    
    testRow1 <- testRow1[!(testRow1 %in% c("", ","))]
    
    location <- testRow1[-c(1, seq(length(testRow1) - 3, length(testRow1), by = 1))]
    
    location <- ifelse(length(location) > 1, paste(location, collapse = " "), location)
    
    testDf <- data.frame(ref = testRow1[1], location = location, state = gsub(",", "", testRow1[length(testRow1) - 3]), 
                         latitude = testRow1[length(testRow1) - 2], longitude = testRow1[length(testRow1) - 1], 
                         elev = testRow1[length(testRow1)]) 
    
    datDF <- read.table(text = testDat[2:length(testDat)]) %>% 
      dplyr::rename(date = V1, val = V2) %>% 
      dplyr::mutate(date = as.Date(date, format = "%m/%d/%Y"))
    
    finList[[paste0(testDf$ref, "_info")]] <- testDf
    
    finList[[paste0(testDf$ref, "_dat")]] <- datDF
    
  }
  
  return(finList)
  
}
