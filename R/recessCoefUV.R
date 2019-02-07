moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

recessCoefUV <- function(flow, dates, nDays = 1, eventProb = 0.99, getDF = TRUE, siteID = NULL, drnArea = NULL) {
  
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(segmented, quietly = TRUE)
  library(strucchange, quietly = TRUE)
  library(mgcv, quietly = TRUE)
  
  if (any(is.na(flow))) {
    
    stop("No NA flow values are allowed.")
    
  } else {
    
    testDF <- data.frame(dates = dates, flow = flow)
    
    if (is.null(drnArea)) {
      
      stop("Drainage area must be specified.")
      
    } else {
      
      testDFDaily <- testDF %>% 
        dplyr::mutate(Date = as.Date(dates)) %>% 
        dplyr::group_by(Date) %>% 
        dplyr::summarize(dailyQ = mean(flow)) %>% 
        dplyr::mutate(bfiQ = runBFI(dailyQ, Date)) %>% 
        dplyr::mutate(runOffBfiQ = dailyQ - bfiQ) %>% 
        dplyr::mutate(partQ = runPART(dailyQ, Date, drnArea = drnArea)) %>% 
        dplyr::mutate(runOffPartQ = dailyQ - partQ) %>% 
        data.frame()
      
      drnAreaAdj <- sum(testDFDaily$runOffPartQ, na.rm = TRUE) / sum(testDFDaily$dailyQ, na.rm = TRUE)
      
      testDFDailyInterQ <- data.frame(Date = testDFDaily$Date, 
                                      runOffQ = testDFDaily$runOffPartQ)
      
      testDFDailyInterQ <- na.omit(testDFDailyInterQ)
      
      testDFDailyInterQ$runOffQ <- ifelse(testDFDailyInterQ$runOffQ <= 0, 0.001, testDFDailyInterQ$runOffQ)
      
      testDFDailyInterQ$interQ <- runPART(flow = testDFDailyInterQ$runOffQ, dates = testDFDailyInterQ$Date, 
                                          drnArea = drnArea * drnAreaAdj)
      
      #testDFDailyInterQ$interQ <- runBFI(flow = testDFDailyInterQ$runOffQ, dates = testDFDailyInterQ$Date)
      
      testDFDailyInterQ <- testDFDailyInterQ[, c(1, 3)]
      
      testDFDaily <- dplyr::left_join(testDFDaily, testDFDailyInterQ, "Date")
      
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
        dplyr::mutate(cumVal = 1:n()) %>% 
        dplyr::mutate(Date = as.Date(dates, "%Y-%m-%d")) %>% 
        dplyr::left_join(testDFDaily, "Date") %>% 
        dplyr::filter(!is.na(qual)) %>% 
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
      
      if (is.na(testDF[1, 18])) {testDF[1, 18] <- 0}
      
      if (is.na(testDF[nrow(testDF), 18])) {testDF[nrow(testDF), 18] <- max(testDF$eventVal, na.rm = TRUE)}
      
      testDF$eventVal <- zoo::na.locf(testDF$eventVal, fromLast = TRUE)
      
      eventCut <- quantile(testDF$flow, probs = eventProb)
      
      eventsTest <- testDF %>% 
        dplyr::group_by(eventVal) %>% 
        dplyr::summarize(eventLength = n(),
                         maxQ = ifelse(max(flow) < eventCut, NA, max(flow))) %>% 
        dplyr::filter(eventLength > nDayVal) %>% 
        na.omit()
      
      testDFEvents <- dplyr::filter(testDF, eventVal %in% eventsTest$eventVal)
      
      eventNums <- unique(testDFEvents$eventVal)
      
      retVal <- data.frame(siteID = NA, dates = as.POSIXct("1970-01-01, 00:00:00"), eventPeak = NA, 
                           recessCoefGW1 = NA, storDepGW1 = NA, recessCoefGW2 = NA, storDepGW1 = NA)
      
      for (i in seq(1, length(eventNums), 1)) {
        
        chunk <- dplyr::filter(testDFEvents, eventVal == eventNums[i])
        
        minRise <- dplyr::filter(chunk, qual == "rise")[1, 1]
        
        chunk <- dplyr::filter(chunk, dates >= minRise)
        
        maxFlowDate <- dplyr::filter(chunk, flow == max(chunk$flow, na.rm = TRUE)) [1, 1]
        
        fallChunk <- dplyr::filter(chunk, dates > maxFlowDate & qual == "fall" & slope <= 0)
        
        fallChunk$numTime <- as.numeric(fallChunk$dates)
        
        testBP <- tryCatch({ 
          
          strucchange::breakpoints(slopeAve ~ 1, data = fallChunk, h = 4, breaks = 3)
          
        },
        
        error = function(cond) {
          
          "failure"
          
        })
        
        if (length(testBP) == 1) {
          
          recessCoefGW1 <- 9999
          
          storDepGW1 <- 9999
          
          recessCoefGW2 <- 9999
          
          storDepGW2 <- 9999
          
        } else {
          
          breaksDF <- fallChunk[testBP$breakpoints, ]
          
          baseDF <- breaksDF[2, ]
          
          chunkDaily <- chunk %>% 
            dplyr::group_by(Date) %>% 
            dplyr::summarize(flow = mean(dailyQ), 
                             bfiQ = mean(bfiQ), 
                             runOffBfiQ = mean(runOffBfiQ), 
                             partQ = mean(partQ), 
                             runOffPartQ = mean(runOffPartQ), 
                             interQ = mean(interQ)) %>% 
            dplyr::mutate(numDate = as.numeric(Date)) %>%  
            dplyr::mutate(slopeBfiQ = c(NA, diff(log10(bfiQ)) / diff(numDate)), 
                          absSlopeBfiQ = abs(slopeBfiQ)) %>%  
            dplyr::mutate(slopeRunOffBfiQ = c(NA, diff(log10(runOffBfiQ)) / diff(numDate)), 
                          absSlopeRunOffBfiQ = abs(slopeRunOffBfiQ)) %>%  
            dplyr::mutate(slopePartQ = c(NA, diff(log10(partQ)) / diff(numDate)), 
                          absSlopePartQ = abs(slopePartQ)) %>%  
            dplyr::mutate(slopeRunOffPartQ = c(NA, diff(log10(runOffPartQ)) / diff(numDate)), 
                          absSlopeRunOffPartQ = abs(slopeRunOffPartQ)) %>% 
            dplyr::mutate(slopeInterQ = c(NA, diff(log10(interQ)) / diff(numDate)), 
                          absSlopeInterQ = abs(slopeInterQ)) %>% 
            dplyr::mutate(percDiff = (flow - partQ) / (flow + partQ)) %>% 
            dplyr::filter(between(row_number(), which.max(flow), n())) %>% 
            data.frame()
          
          chunkGAM <- gam(percDiff ~ s(log10(flow), k = (nrow(chunkDaily) - 1)), data = chunkDaily)
          
          fallChunk <- fallChunk %>% 
            dplyr::mutate(percDiff = as.numeric(predict(chunkGAM, fallChunk))) %>% 
            dplyr::mutate(bFlow = flow - (flow * percDiff)) %>% 
            dplyr::mutate(runOff = flow - bFlow) %>% 
            data.frame()
          
          runOffBP <- breakpoints(log(runOff) ~ 1, data = fallChunk, h = 4, breaks = 3)
          
          runOffBPDF <- fallChunk[runOffBP$breakpoints, ]
          
          # next step... convert daily means to uv time-series based on % diff between daily means and interpolation from max values
          
          chunkDailyEval <- chunkDaily %>% 
            dplyr::summarize(maxBfiQ = max(bfiQ), 
                             maxRunOffBfiQ = max(runOffBfiQ), 
                             maxPartQ = max(partQ), 
                             maxRunOffPartQ = max(runOffPartQ), 
                             maxInterQ = max(interQ)) %>% 
            data.frame()
          
          recessChunk <- fallChunk %>% 
            dplyr::filter(dates >= runOffBPDF[3, 1]) %>% 
            dplyr::mutate(diffTime = ((numDate / 3600) - lag(numDate / 3600, 1))) %>% 
            dplyr::mutate(diffTime = ifelse(is.na(diffTime), 0, diffTime)) %>% 
            dplyr::mutate(diffTime = cumsum(diffTime)) %>% 
            dplyr::mutate(runOff = log(runOff)) %>% 
            dplyr::mutate(bFlow = log(bFlow)) %>% 
            #dplyr::top_n(-6) %>% 
            data.frame()
          
          recessLmGW1 <- lm(runOff ~ diffTime, data = na.omit(recessChunk))
          
          recessCoefGW1 <- 1 / (recessLmGW1$coefficients[2] * -1)
          
          storDepGW1 <- ((exp(max(recessChunk$runOff, na.rm = TRUE))) / (3.9*27878400)) / (recessLmGW1$coefficients[2] * -1)
          
          recessLmGW2 <- lm(bFlow ~ diffTime, data = na.omit(recessChunk))
          
          recessCoefGW2 <- 1 / (recessLmGW2$coefficients[2] * -1)
          
          storDepGW2 <- ((exp(max(recessChunk$bFlow, na.rm = TRUE))) / (3.9*27878400)) / (recessLmGW2$coefficients[2] * -1)
          
          peakRow <- dplyr::slice(chunk, which.max(flow))
          
          if (nrow(peakRow) > 1) {
            
            peakRow <- peakRow[nrow(peakRow), ]
            
          }
          
          retVal_ <- data.frame(siteID = siteID, dates = peakRow$dates, eventPeak = peakRow$flow, 
                                recessCoefGW1 = recessCoefGW1, storDepGW1 = storDepGW1, 
                                recessCoefGW2 = recessCoefGW2, storDepGW2 = storDepGW2)
          
        }
        
        retVal <- dplyr::bind_rows(retVal, retVal_)
        
      } 
      
      return(retVal)
    }
    
  }
    
}
