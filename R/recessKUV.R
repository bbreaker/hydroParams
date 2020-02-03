# started Dec 23, 2017 for package development

moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

recessKUV <- function(flow, dates, nDays = 1, eventProb = 0.99, getDF = FALSE, siteID = NULL, drnArea = NULL) {
  
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(segmented, quietly = TRUE)
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
        dplyr::summarize(dailyQ = mean(flow, na.rm = TRUE)) %>% 
        dplyr::mutate(partQ = runPART(dailyQ, Date, drnArea = drnArea)) %>% 
        dplyr::mutate(runOffPartQ = dailyQ - partQ) %>% 
        data.frame()
      
      drnAreaAdj <- sum(testDFDaily$runOffPartQ, na.rm = TRUE) / sum(testDFDaily$dailyQ, na.rm = TRUE)
      
      testDFDailyInterQ <- data.frame(Date = testDFDaily$Date, 
                                      runOffQ = testDFDaily$runOffPartQ)
      
      testDFDailyInterQ <- na.omit(testDFDailyInterQ)
      
      testDFDailyInterQ$runOffQ <- ifelse(testDFDailyInterQ$runOffQ == 0, 0.001, testDFDailyInterQ$runOffQ)
      
      testDFDailyInterQ$interQ <- runPART(flow = testDFDailyInterQ$runOffQ, dates = testDFDailyInterQ$Date, 
                                          drnArea = (drnArea * drnAreaAdj))
      
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
      
      if (is.na(testDF[1, 16])) {testDF[1, 16] <- 0}
      
      if (is.na(testDF[nrow(testDF), 16])) {testDF[nrow(testDF), 16] <- max(testDF$eventVal, na.rm = TRUE)}
      
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
      
      kDF <- data.frame()
      
      for (i in seq(1, length(eventNums), 1)) {
        
        chunk <- dplyr::filter(testDFEvents, eventVal == eventNums[i])
        
        minRise <- dplyr::filter(chunk, qual == "rise")[1, 1]
        
        chunk <- dplyr::filter(chunk, dates >= minRise)
        
        maxFlowDate <- dplyr::filter(chunk, flow == max(chunk$flow, na.rm = TRUE)) [1, 1]
        
        fallChunk <- dplyr::filter(chunk, dates > maxFlowDate & qual == "fall" & slope <= 0)
        
        fallChunk$numTime <- as.numeric(fallChunk$dates)
        
        testLm2 <- tryCatch({ 
          
          glm(log10(flow) ~ numTime, data = fallChunk) 
          
        },
        
        error = function(cond) {
          
          "failure"
          
        })
        
        testSeg2 <- tryCatch({ 
          
          segmented(testLm2) 
          
        }, 
        
        error = function(cond) { 
          
          "failure" 
          
        })
        
        if (length(testSeg2) == 1) {
          
          kVal_ <- NA
          
          kDF_ <- data.frame(peakDate = as.POSIXct("1970-01-01, 00:00:00"), peakFlow = NA, 
                             breakDate = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                             recessK = NA)
          
        } else {
          
          breakDate2 <- as.POSIXct(summary.segmented(testSeg2)$psi[1, 2], 
                                   origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                   tz = attr(testDF$dates, "tzone"))
          
          endDate <- breakDate2 + lubridate::days(1)
          
          #baseDF <- fallChunk[which(fallChunk$dates > breakDate2), ]
          
          baseDF <- dplyr::filter(fallChunk, dplyr::between(dates, breakDate2, endDate))
          
        }
        
        #baseDF$numHours <- (baseDF$numTime / 60) / 60 
        
        #recessK1 <- lm(log10(flow) ~ numHours, data = baseDF) 
        
        #recessK <- -1 * recessK1$coefficients[2] 
        
        recessK <- baseDF[nrow(baseDF), 2] / baseDF[1, 2]
        
        kVal_ <- recessK 
        
        kDF_ <- data.frame(peakDate = fallChunk[1, 1], peakFlow = fallChunk[1, 2], 
                           breakDate = baseDF[1, 1], breakFlow = baseDF[1, 2], 
                           breakPlus24Date = baseDF[nrow(baseDF), 1], breakPlus24Flow[nrow(baseDF), 2], 
                           recessK = recessK) 
        
        kVal <- c(kVal, kVal_) 
        
        kDF <- dplyr::bind_rows(kDF, kDF_) 
          
        }
      }
      
      if (getDF == FALSE) {
        
        if (is.null(siteID)) {
          
          retVal <- list(kVal = signif(mean(kVal, na.rm = TRUE), 3),
                         nEvents = length(na.omit(kVal)))
          
        } else {
          
          retVal <- list(siteID = siteID,
                         kVal = signif(mean(kVal, na.rm = TRUE), 3),
                         nEvents = length(na.omit(kVal)))
          
        }
        
      } else {
        
        if (is.null(siteID)) {
          
          retVal <- na.omit(kDF)
          
        } else {
          
          retVal <- na.omit(kDF)
          
          retVal$siteID <- siteID
          
        }
        
      }
    
  }
  
  return(retVal)
  
}
