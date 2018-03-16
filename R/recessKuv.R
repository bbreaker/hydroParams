moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

recessKuv <- function(flow, dates, nDays = 0.5, eventProb = 0.98) {
  
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(segmented, quietly = TRUE)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow)
    
    nDayVal <- testDF %>%
      dplyr::mutate(dayT = as.Date(testDF$dates)) %>% 
      group_by(dayT) %>% 
      dplyr::summarize(lDayT = length(dayT)) %>% 
      dplyr::summarize(totDay = round(max(lDayT) * nDays, 0)) %>% 
      unlist(c())
    
    testDF <- testDF %>% 
      dplyr::mutate(numDate = as.numeric(dates)) %>%  
      dplyr::mutate(aveMove = as.numeric(moveAve(testDF$flow, nDayVal))) %>%  
      dplyr::mutate(diffAve = c(NA, diff(aveMove))) %>%   
      dplyr::mutate(slopeAve = c(NA, diff(aveMove) / diff(numDate))) %>%  
      dplyr::mutate(slope = c(NA, diff(flow) / diff(numDate))) %>%  
      dplyr::mutate(absSlope = abs(slope)) %>%   
      dplyr::mutate(qual = if_else(slopeAve > 0, "rise", 
                                   if_else(slopeAve == 0, "flat", "fall"))) %>% 
      dplyr::filter(!is.na(qual)) %>% 
      dplyr::mutate(cumVal = 1:n()) %>% 
      dplyr::mutate(dateNew = as.Date(dates, "%Y-%m-%d")) %>% 
      data.frame()
    
    testRle <- data.frame(lengths = rle(testDF$qual)$lengths,
                          vals = rle(testDF$qual)$values,
                          stringsAsFactors = FALSE)
    
    testRle <- testRle %>% 
      dplyr::mutate(cumVal = cumsum(testRle$lengths)) %>% 
      dplyr::mutate(event = if_else(vals == "rise" & lead(vals) == "fall", 1, 0)) %>% 
      dplyr::mutate(eventVal = cumsum(event)) %>% 
      dplyr::select(cumVal, eventVal) %>% 
      data.frame()
    
    testDF <- dplyr::left_join(testDF, testRle, "cumVal")
    
    if (is.na(testDF[1, 12])) {testDF[1, 12] <- 1}
    
    if (is.na(testDF[nrow(testDF), 12])) {testDF[nrow(testDF), 12] <- max(testDF$eventVal, na.rm = TRUE)}
    
    testDF$eventVal <- na.locf(testDF$eventVal, fromLast = TRUE)
    
    eventCut <- quantile(testDF$flow, probs = eventProb)
    
    eventsTest <- testDF %>% 
      dplyr::group_by(eventVal) %>% 
      dplyr::summarize(eventLength = n(),
                       maxQ = ifelse(max(flow) < eventCut, NA, max(flow))) %>% 
      dplyr::filter(eventLength > nDayVal) %>% 
      na.omit()
    
    testDFEvents <- dplyr::filter(testDF, eventVal %in% eventsTest$eventVal)
    
    eventNums <- unique(testDFEvents$eventVal)
    
    kVal <- as.numeric()
    
    for (i in seq(1, length(eventNums), 1)) {
      
      chunk <- dplyr::filter(testDFEvents, eventVal == eventNums[i])
      
      minRise <- dplyr::filter(chunk, qual == "rise")[1, 1]
      
      chunk <- dplyr::filter(chunk, dates >= minRise)
      
      maxFlowDate <- dplyr::filter(chunk, flow == max(chunk$flow, na.rm = TRUE)) [1, 1]
      
      fallChunk <- dplyr::filter(chunk, dates > maxFlowDate & qual == "fall")
      
      fallChunk$numTime <- as.numeric(fallChunk$dates)
      
      testLm <- lm(slope ~ numTime, data = fallChunk)
      
      testSeg <- tryCatch({ 
        
        segmented(testLm)
        
      },
      
      error = function(cond) {
        
        "failure"
        
      })
      
      if (testSeg == "failure") {
        
        kVal_ <- NA
        
      } else {
        
        breakDate <- as.POSIXct(summary.segmented(testSeg)$psi[1, 2], origin = "1970-01-01")
        
        baseVec <- fallChunk[which(fallChunk$dates >= breakDate), 2]
        
        kVal_ <- signif(baseVec[1] / fallChunk[1, 2], 2)

      }
      
      kVal <- c(kVal, kVal_)
    
    }
    
    kVal <- mean(kVal, na.rm = TRUE)
    
  }
  
  return(kVal)
  
}
