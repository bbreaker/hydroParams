ratioToPeak <- function(flow, baseQ, dates) {
  
  library(dplyr)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow, baseQ = baseQ)
    
    testDF$diffQ <- testDF$flow - testDF$baseQ
    
    testDF$diffLog <- dplyr::if_else(testDF$diffQ == 0, "base", "event")
    
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    
    testRle$cumVal <- cumsum(testRle$lengths)
    
    testRleDF <- testRle %>% 
      mutate(qualk = if_else(vals == "base", 0, 1)) %>% 
      group_by(qualk) %>% 
      mutate(index = ifelse(qualk == 0, 0, 1:n())) %>% 
      data.frame()
    
    retVal <- as.numeric()
    
    for (i in seq(2, max(testRleDF$index) - 1, 1)) {
      
      chunk <- testRleDF[(which(testRleDF$index == i) - 1):which(testRleDF$index == i), ]
      
      chunk <- testDF[(chunk[1, 3]):(chunk[2, 3] + 1), ]
      
      kVal <- chunk[nrow(chunk), 3] / max(chunk$flow, na.rm = TRUE)
      
      kVal <- signif(kVal, 3)
      
      retVal <- append(retVal, kVal)
      
    }
    
    retVal <- signif(mean(retVal, na.rm = TRUE), 3)
    
  }
  
  return(retVal)
  
}
