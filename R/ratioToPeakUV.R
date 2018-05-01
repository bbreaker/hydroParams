moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

ratioToPeakUV <- function(flow, dates, nDays = 0.5, eventProb = 0.998, getDF = FALSE, siteID = NULL) {
  
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
      
      if (testSeg == "failure") {
        
        kVal_ <- NA
        
        kDF_ <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                           eventPeak = NA, kVal = NA)
        
      } else if (testSeg2 == "failure") {
        
        kVal_ <- NA
        
        kDF_ <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                           eventPeak = NA, kVal = NA)
        
      } else if (testLm == "failure") {
        
        kVal_ <- NA
        
        kDF_ <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                           eventPeak = NA, kVal = NA)
        
      } else if (testLm2 == "failure") {
        
        kVal_ <- NA
        
        kDF_ <- data.frame(dates = as.POSIXct("1970-01-01, 00:00:00"), breakFlow = NA, 
                           eventPeak = NA, kVal = NA)
        
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
        
        kDF_$kVal <- signif(kDF_$breakFlow / kDF_$eventPeak)

      }
      
      kVal <- c(kVal, kVal_)
      
      kDF <- dplyr::bind_rows(kDF, kDF_)
      
      kDF <- kDF[!duplicated(kDF$dates), ]
      
    }
    
    thresholds <- quantile(kVal, probs = c(0.05, 0.95), na.rm = TRUE)
    
    kVal <- kVal[((kVal > thresholds[1]) + 0.01) == TRUE]
      
    kVal <- kVal[((kVal < thresholds[2]) - 0.01) == TRUE]
      
    kDF <- dplyr::filter(kDF, (kVal + 0.01) > thresholds[1] & (kVal - 0.01) < thresholds[2])

    kVal <- na.omit(kVal)
    
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
  
  return(retVal)
  
}
