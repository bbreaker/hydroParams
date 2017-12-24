recessK <- function(flow, baseQ, dates) {
  
  library(dplyr, quietly = TRUE)
  
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
    
    kValEvent <- as.numeric()
    
    for (i in seq(2, max(testRleDF$index) - 1, 1)) {
      
      chunk <- testRleDF[(which(testRleDF$index == i) - 1):(which(testRleDF$index == i) + 1), ]
      
      chunk <- testDF[(chunk[2, 3]):(chunk[nrow(chunk), 3]), ]
      
      chunk$baseQ <- dplyr::if_else(chunk$baseQ == 0, 0.0001, chunk$baseQ)
      
      if (nrow(chunk) <= 4) {
        
        kValEvent_ <- NA
        
      } else {
        
        kValEvent_ <- chunk[2, 2] / chunk[1, 2]
        
        kValEvent <- append(kValEvent, kValEvent_)
          
      }
      
    }
    
    kVal <- signif(quantile(kValEvent, probs = 0.01, na.rm = TRUE), 3)
    
  }
  
  return(kVal)
  
}
