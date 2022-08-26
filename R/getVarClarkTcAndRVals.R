getVarClarkTcAndRVals <- function(logFile) {
  
  logFile <- readLines(logFile)
  
  theLines <- logFile[grepl("NOTE 45913:", logFile)]
  
  theLines1 <- unlist(stringr::str_split_fixed(theLines, "for period ending at", n = 2))[, 1]
  
  theLines1 <- stringr::str_remove_all(theLines1, "NOTE 45913:  Clark ")
  
  tcVals <- gsub("[^0-9.-]", "", unlist(stringr::str_split_fixed(theLines1, ", ", n = 2))[, 1])
  
  rVals <- gsub("[^0-9.-]", "", unlist(stringr::str_split_fixed(theLines1, ", ", n = 2))[, 2])
  
  theLines2 <- unlist(stringr::str_split_fixed(theLines, "for period ending at", n = 2))[, 2]
  
  theLines3 <- unlist(stringr::str_split_fixed(theLines2, 'for subbasin \"', n = 2))[, 2]
  
  theLines2 <- stringr::str_sub(theLines2, 1, 22)
  
  theLines3 <- gsub('\".', "", theLines3)
  
  theDates <- as.POSIXct(theLines2, format = "%d %B %Y, %H:%M", tz = "UTC")
  
  retDF <- data.frame(dateTime = theDates, tc = as.numeric(tcVals), r = as.numeric(rVals), subbasin = theLines3)
  
  return(retDF)
  
}