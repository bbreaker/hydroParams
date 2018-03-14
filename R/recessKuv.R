moveAve <- function(series, numDays) {
  
  stats::filter(series, rep(1 / numDays, numDays), sides = 2)
  
}

recessKuv <- function(flow, dates, nDays, drnArea) {
  
  library(dplyr, quietly = TRUE)
  library(mgcv, quietly = TRUE)
  library(gamlss, quietly = TRUE)
  library(zoo, quietly = TRUE)
  
  if (any(is.na(flow))) {
    
    retVal <- NA
    
  } else {
    
    #nDays <- 0.05
    
    testDF <- data.frame(dates = dates, flow = flow)
    
    nDayVal <- testDF %>%
      dplyr::mutate(dayT = as.Date(testDF$dates)) %>% 
      group_by(dayT) %>% 
      dplyr::summarize(lDayT = length(dayT)) %>% 
      dplyr::summarize(totDay = round(max(lDayT) * nDays, 0)) %>% 
      unlist(c())
    
    #testDF$numDate <- as.numeric(testDF$dates)
    #
    #testGAM <- gam(log10(flow) ~ s(numDate, k = as.numeric(nDays / 2)), data = testDF)
    #
    #testDF$smooth <- as.numeric(10^predict(testGAM, testDF))
    #
    #testDF$aveMove <- as.numeric(moveAve(testDF$flow, nDayVal))
    #
    #testDF$diffAve <- c(NA, diff(testDF$aveMove))
    #
    #testDF$slope <- c(NA, diff(testDF$aveMove) / diff(testDF$numDate))
    #
    #testDF$absSlope <- abs(testDF$slope)
    #
    #testDF$qual <- if_else(testDF$slope > 0, "rise", 
    #                       if_else(testDF$slope == 0, "flat", "fall"))
    
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
    
    testDaily <- testDF %>% 
      dplyr::group_by(dateNew) %>% 
      dplyr::summarize(dailyQ = mean(flow)) %>% 
      data.frame()

    testDaily$baseQ <- runPART(testDaily$dailyQ, testDaily$dateNew, drnArea = drnArea)
    
    testDF <- dplyr::left_join(testDF, testDaily, "dateNew")
    
    testRle <- data.frame(lengths = rle(testDF$qual)$lengths,
                          vals = rle(testDF$qual)$values,
                          stringsAsFactors = FALSE)
    
    testRle <- testRle %>% 
      dplyr::mutate(cumVal = cumsum(testRle$lengths)) %>% 
      dplyr::mutate(event = if_else(vals == "rise" & lead(vals) == "fall", 1, 0)) %>% 
      dplyr::mutate(eventVal = cumsum(event)) %>% 
      dplyr::select(cumVal, eventVal) %>% 
      data.frame()
    
    #testDF <- testDF %>% 
    #  dplyr::left_join(testRle, "cumVal") %>% 
    #  na.locf(eventNum, fromLast = TRUE)
    
    testDF <- dplyr::left_join(testDF, testRle, "cumVal")
    
    if (is.na(testDF[1, 14])) {testDF[1, 14] <- 1}
    
    if (is.na(testDF[nrow(testDF), 14])) {testDF[nrow(testDF), 14] <- max(testDF$eventVal, na.rm = TRUE)}
    
    testDF$eventVal <- na.locf(testDF$eventVal, fromLast = TRUE)
    
    testDF <- dplyr::mutate(testDF, modNum = eventVal) 
    
    #gamMod <- gamlss(slope ~ pb(numDate, df = 1), data = na.omit(testDF), 
                     #family = LO(mu.link = "identity", sigma.link = "log"))
    
    #testDFnon <- dplyr::mutate(testDFnon, dayVal = format(dates, "%Y-%m-%d"))
    
    testEvents <- unique(testDF$eventVal)
    
    for (j in 2:length(testEvents)) {
      
      testDFSub <- dplyr::filter(testDF, eventVal == j)
      
      if (nrow(dplyr::filter(testDF, eventVal == j & qual == "rise")) < 
          
          (0.8 * nrow(dplyr::filter(testDF, eventVal == j & qual == "fall")))) { 
        
        testDF[which(testDF$eventVal == j), 15] <- testDF[which(testDF$eventVal == j), 15] + 1
        
      } else if (0.8 * nrow(dplyr::filter(testDF, eventVal == j & qual == "rise")) > 
                 
                 (nrow(dplyr::filter(testDF, eventVal == j & qual == "fall")))) {
        
        testDF[which(testDF$eventVal == j), 15] <- testDF[which(testDF$eventVal == j), 15] - 1
        
      } 
      
    }
    
    
    
    #testVals <- data.frame(testDF[0, ], muP = as.numeric(), sigP = as.numeric())
    #
    ##i <- 2920
    #
    #for (i in 1:length(testEvents)) {
    #  
    #  testDFsub <- dplyr::filter(testDF, eventNum == testEvents[i])
    #  
    #  gamMod <- tryCatch({
    #    
    #    gamlss(slope ~ cs(numDate, df = 3), data = testDFsub, 
    #           family = LO(mu.link = "identity", sigma.link = "log"))
    #    
    #  },
    #  
    #  error = function(cond) {
    #    
    #    "failure"
    #    
    #  })
    #  
    #  if (gamMod == "failure") {
    #    
    #    testDFsub <- testDFsub %>% 
    #      dplyr::mutate(muP = NA)  %>% 
    #      dplyr::mutate(sigP = NA) 
    #      #dplyr::mutate(slpThrshldHgh = NA) %>%
    #      #dplyr::mutate(slpThrshldLow = NA)
    #    
    #  } else {
    #    
    #    testDFsub <- testDFsub %>% 
    #      dplyr::mutate(muP = predict(gamMod, what = "mu", data = testDFsub))  %>% 
    #      dplyr::mutate(sigP = exp(predict(gamMod, what = "sigma", data = testDFsub)))
    #      #dplyr::mutate(slpThrshldHgh = qLO(0.55, mu = mean(muP), sigma = mean(sigP))) %>%
    #      #dplyr::mutate(slpThrshldLow = qLO(0.45, mu = mean(muP), sigma = mean(sigP)))
    #    
    #  }
    #  
    #  testVals <- dplyr::bind_rows(testVals, testDFsub)
    #  
    #}
    #
    #testVals$sigP <- if_else(testVals$sigP > quantile(testVals$sigP, probs = 0.99, na.rm = TRUE),
    #                         quantile(testVals$sigP, probs = 0.99, na.rm = TRUE), testVals$sigP)
    #
    #testVals$muP <- zoo::na.approx(testVals$muP)
    #
    #testVals$sigP <- zoo::na.approx(testVals$sigP)
    #
    #testVals <- testVals %>% 
    #  dplyr::mutate(slpThrshldHgh = qLO(0.75, mu = muP, sigma = sigP)) %>% 
    #  dplyr::mutate(slpThrshldLow = qLO(0.25, mu = muP, sigma = sigP))
    #  
    #testDFnon$muP <- predict(gamMod, what = "mu", data = testDFnon)
    #
    #testDFnon$sigP <- exp(predict(gamMod, what = "sigma", data = testDFnon))
    #
    #testDFnon$nuP <- predict(gamMod, what = "nu", data = testDFnon)
    #
    #testDFnon <- testDFnon %>% 
    #  dplyr::mutate(dayVal = format(dates, "%Y-%m-%V")) %>% 
    #  group_by(dates) %>% 
    #  dplyr::mutate() %>% 
    #  dplyr::mutate()
    
    ################ updates to here ##############################################
    
    testDF$diffLog <- dplyr::if_else(testDF$absSlope < slpThrshld, "base", "event")
    
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