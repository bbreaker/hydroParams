recessKuv <- function(flow, dates) {
  
  library(dplyr, quietly = TRUE)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow)
    
    testDF$numDate <- c(NA, cumsum(diff(as.numeric(testDF$dates)) / 15))
    
    testDF$diffQ <- c(NA, diff(testDF$flow))
    
    testDF$slope <- c(NA, diff(testDF$flow) / diff(as.numeric(testDF$dates)))
    
    testDF$absSlope <- abs(testDF$slope)
    
    gamMod <- gamlss(log10(absSlope) ~ pb(numDate, df = 15), sigma.fo = ~pb(numDate, df = 15), 
                     nu.fo = ~15, data = na.omit(testDF), 
                     family=GG(mu.link = "log", 
                               sigma.link = "log", 
                               nu.link = "identity"))
    
    testDF$diffLog <- dplyr::if_else(testDF$diffQ < 0, "fall", "rise")
    
    testRle <- data.frame(lengths = rle(testDF$diffLog)$lengths,
                          vals = rle(testDF$diffLog)$values)
    
    testRle$cumVal <- cumsum(testRle$lengths)
    
    testRle <- testRle %>% 
      mutate(qualk = if_else(vals == "fall", 0, 1)) %>% 
      group_by(qualk) %>% 
      mutate(index = ifelse(qualk == 0, 1:n(), 0)) %>% 
      data.frame()
    
    indexVal <- na.omit(as.numeric(testRle$index))
    
    indexVal <- indexVal[indexVal > 0]
    
    kValEvent <- as.numeric()
    
    for (i in seq(1, length(indexVal), 1)) {
      
      chunk <- testRleDF[(which(testRleDF$index == i) - 1):(which(testRleDF$index == i) + 1), ]
      
      chunk$slope <- c(NA, diff(chunk$Flow_Inst) / diff(as.numeric(chunk$dateTime)))
      
      round(((breakpoints(chunk$slope ~ 1)[[1]][1] + breakpoints(chunk$slope ~ 1)[[1]][2]) / 2), 0)
      
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