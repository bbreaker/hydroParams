# started Dec 23, 2017 for package development

moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

recessKUV <- function(flow, dates, eventProb = 0.99, getDF = FALSE, siteID = NULL, drnArea = NULL) {
  
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(segmented, quietly = TRUE)
  
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
        #dplyr::mutate(bfiQ = runBFI(dailyQ, Date)) %>% 
        #dplyr::mutate(runOffBfiQ = dailyQ - bfiQ) %>% 
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
      
      kDF <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                        eventPeak = NA, kVal = NA)
      
      for (i in seq(1, length(eventNums), 1)) {
        
        chunk <- dplyr::filter(testDFEvents, eventVal == eventNums[i])
        
        minRise <- dplyr::filter(chunk, qual == "rise")[1, 1]
        
        chunk <- dplyr::filter(chunk, dates >= minRise)
        
        maxFlowDate <- dplyr::filter(chunk, flow == max(chunk$flow, na.rm = TRUE)) [1, 1]
        
        fallChunk <- dplyr::filter(chunk, dates > maxFlowDate & qual == "fall" & slope <= 0)
        
        fallChunk$numTime <- as.numeric(fallChunk$dates)
        
        testLm <- tryCatch({ 
          
          lm(slope ~ numTime, data = fallChunk) 
          
        },
        
        error = function(cond) {
          
          "failure"
          
        })
        
        testSeg <- tryCatch({ 
          
          segmented(testLm) 
          
        }, 
        
        error = function(cond) { 
          
          "failure" 
          
        })
        
        testLm2 <- tryCatch({ 
          
          lm(flow ~ numTime, data = fallChunk) 
          
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
        
        if (length(testSeg) == 1 & length(testSeg2) == 1) {
          
          kVal_ <- NA
          
          kDF_ <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                             eventPeak = NA, kVal = NA)
          
        } else if (length(testSeg) != 1 & length(testSeg2) == 1) {
          
          breakDate <- as.POSIXct(summary.segmented(testSeg)$psi[1, 2], 
                                  origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                  tz = attr(testDF$dates, "tzone"))
          
          breakDate <- lubridate::round_date(breakDate, unit = "15 minute")
          
          baseDF <- fallChunk[which(fallChunk$dates == breakDate), ]
          
          baseDF <- baseDF[, c(1, 2)] 
          
          nRowVal <- round(nrow(baseDF) / 2, 0)
          
          nRowVal <- ifelse(nRowVal == 0, 1, nRowVal)
          
          baseDF <- baseDF[nRowVal, ]
          
        } else if (length(testSeg) == 1 & length(testSeg2) != 1) {
          
          breakDate2 <- as.POSIXct(summary.segmented(testSeg2)$psi[1, 2], 
                                   origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                   tz = attr(testDF$dates, "tzone"))
          
          breakDate2 <- lubridate::round_date(breakDate2, unit = "15 minute")
          
          baseDF <- fallChunk[which(fallChunk$dates == breakDate2), ]
          
          baseDF <- baseDF[, c(1, 2)] 
          
          nRowVal <- round(nrow(baseDF) / 2, 0)
          
          nRowVal <- ifelse(nRowVal == 0, 1, nRowVal)
          
          baseDF <- baseDF[nRowVal, ]
          
        } else {
          
          breakDate <- as.POSIXct(summary.segmented(testSeg)$psi[1, 2], 
                                  origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                  tz = attr(testDF$dates, "tzone"))
          
          breakDate2 <- as.POSIXct(summary.segmented(testSeg2)$psi[1, 2], 
                                   origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                   tz = attr(testDF$dates, "tzone"))
          
          if (abs(difftime(breakDate, breakDate2, units = "mins")) < 15) {
            
            #breakDates <- as.POSIXct(min(c(breakDate, breakDate2)), 
            #                         origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
            #                         tz = attr(testDF$dates, "tzone"))
            
            breakDates <- as.POSIXct(round(as.double(breakDate) / (15*60)) * (15*60), 
                                     origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                     tz = attr(testDF$dates, "tzone"))
            
            baseDF <- fallChunk[which(fallChunk$dates >= breakDates), ]
            
            baseDF <- baseDF[1, ]
            
          } else {
            
            breakDates <- data.frame(dates = as.POSIXct("1970-01-01 00:00:00", 
                                                        origin = as.POSIXct("1970-01-01", tz = attr(testDF$dates, "tzone")), 
                                                        tz = attr(testDF$dates, "tzone")))
            
            breakDates[2, 1] <- breakDate; breakDates[3, 1] <- breakDate2; breakDates <- breakDates[-1, ]
            
            baseDF <- fallChunk[which(fallChunk$dates >= min(breakDates) & fallChunk$dates <= max(breakDates)), ]
            
            baseDF <- baseDF[, c(1, 2)] 
            
            nRowVal <- round(nrow(baseDF) / 2, 0)
            
            nRowVal <- ifelse(nRowVal == 0, 1, nRowVal)
            
            baseDF <- baseDF[nRowVal, ]
            
          }
          
          kVal_ <- signif(baseDF$flow / max(chunk$flow, na.rm = TRUE), 2)
          
          kDF_ <- data.frame(dates = baseDF$dates, breakFlow = baseDF$flow, eventPeak = max(chunk$flow, na.rm = TRUE))
          
          chunkDaily <- chunk %>% 
            dplyr::group_by(Date) %>% 
            dplyr::summarize(dailyQ = mean(dailyQ), 
                             #bfiQ = mean(bfiQ), 
                             #runOffBfiQ = mean(runOffBfiQ), 
                             partQ = mean(partQ), 
                             runOffPartQ = mean(runOffPartQ), 
                             interQ = mean(interQ)) %>% 
            dplyr::mutate(numDate = as.numeric(Date)) %>%  
            #dplyr::mutate(slopeBfiQ = c(NA, diff(log10(bfiQ)) / diff(numDate)), 
            #              absSlopeBfiQ = abs(slopeBfiQ)) %>%  
            #dplyr::mutate(slopeRunOffBfiQ = c(NA, diff(log10(runOffBfiQ)) / diff(numDate)), 
            #              absSlopeRunOffBfiQ = abs(slopeRunOffBfiQ)) %>%  
            dplyr::mutate(slopePartQ = c(NA, diff(log10(partQ)) / diff(numDate)), 
                          absSlopePartQ = abs(slopePartQ)) %>%  
            dplyr::mutate(slopeRunOffPartQ = c(NA, diff(log10(runOffPartQ)) / diff(numDate)), 
                          absSlopeRunOffPartQ = abs(slopeRunOffPartQ)) %>% 
            dplyr::mutate(slopeInterQ = c(NA, diff(log10(interQ)) / diff(numDate)), 
                          absSlopeInterQ = abs(slopeInterQ)) %>% 
            data.frame()
          
          chunkDailyEval <- chunkDaily %>% 
            dplyr::summarize(#maxBfiQ = max(bfiQ), 
                             #maxRunOffBfiQ = max(runOffBfiQ), 
                             maxPartQ = max(partQ), 
                             maxRunOffPartQ = max(runOffPartQ), 
                             maxInterQ = max(interQ)) %>% 
            data.frame()
          
          recessChunk <- fallChunk %>% 
            dplyr::filter(dates >= baseDF$dates) %>% 
            dplyr::mutate(diffTime = ((numDate / 3600) - lag(numDate / 3600, 1))) %>% 
            dplyr::mutate(diffTime = ifelse(is.na(diffTime), 0, diffTime)) %>% 
            dplyr::mutate(diffTime = cumsum(diffTime)) %>% 
            dplyr::top_n(-6) %>% 
            data.frame()
          
          recessK <- lm(diffTime ~ log10(flow), data = recessChunk)
          
          recessK <- 1 / recessK$coefficients[1]
          
          kVal_ <- recessK
          
          kDF_$kVal <- recessK
          
        }
        
        kVal <- c(kVal, kVal_)
        
        kDF <- dplyr::bind_rows(kDF, kDF_)
        
        kDF <- kDF[!duplicated(kDF$dates), ]
        
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
          
          retVal <- retVal[, c(5, 1:4)]
          
        }
        
      }
      
    }
      
  }
  
  return(retVal)
  
}
