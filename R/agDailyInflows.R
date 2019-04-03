agDailyInflows <- function(dates, flows, drnArea, adjVal = NULL, daysLim = NULL) {
  
  # create a new data frame
  newDF <- data.frame(date = dates, flow = flows, stringsAsFactors = FALSE) 
  
  # use PART for baseflow separation
  newDF$bFlow <- runPART(flow = newDF$flow, dates = newDF$date, drnArea = drnArea)
  
  # compute runoff
  newDF$runOff <- newDF$flow - newDF$bFlow
  
  # default is don't mess with runOff
  # if runOff is less than 'adjVal' of total flow, make it 0
  if (is.null(adjVal)) {
    newDF$runOff <- newDF$runOff
  } else {
    newDF$runOff <- dplyr::if_else(newDF$runOff < (newDF$flow * 0.05), 0, round(newDF$runOff, 2))
  }
  
  # if runOff is 0, at baseflow, otherwise, event is occurring
  newDF$diffLog <- dplyr::if_else(newDF$runOff == 0, "base", "event")
  
  # run length encoding on events
  newDFRLE <- rle(newDF$diffLog)
  newDFRLE <- data.frame(lengths = newDFRLE[1], values = newDFRLE[2], stringsAsFactors = FALSE)
  
  # cumulative sum of event lengths in days
  newDFRLE$cumVal <- cumsum(newDFRLE$lengths)
  
  # number the events
  newDFRLEDF <- newDFRLE %>% 
    dplyr::mutate(qualk = dplyr::if_else(values == "base", 0, 1)) %>% 
    group_by(qualk) %>% 
    dplyr::mutate(index = ifelse(qualk == 0, 0, 1:n())) %>% 
    data.frame()
  
  # create an empty data frame to store information about the events
  retVal <- data.frame()
  
  # add data for event peak, event volume, event date, and water year
  for (i in seq(2, max(newDFRLEDF$index) - 1, 1)) {
    
    chunk <- newDFRLEDF[(which(newDFRLEDF$index == i) - 1):which(newDFRLEDF$index == i), ] 
    
    chunk <- newDF[(chunk[1, 3]):(chunk[2, 3] + 1), ] 
    
    startBaseQ <- chunk[1, 2]; endBaseQ <- chunk[nrow(chunk), 2]
    
    if(is.null(daysLim)) {
      
      retVal_ <- chunk %>% 
        dplyr::filter(diffLog == "event") %>% 
        dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = sum(flow), meanFlow = mean(flow), 
                         midDate = as.Date(median(date), format = "%Y-%m-%d"),
                         beginDate = as.Date(min(date), format = "%Y-%m-%d"), 
                         endDate = as.Date(max(date), format = "%Y-%m-%d")) %>% 
        dplyr::mutate(mn = as.numeric(format(midDate, "%m")), yr = as.numeric(format(midDate, "%Y")),
                      watYr = dplyr::if_else(mn >= 10, yr + 1, yr), 
                      startBaseQ = startBaseQ, endBaseQ = endBaseQ) %>%
        data.frame()
      
    } else if ((nrow(chunk) - 2) <= daysLim) {
      
      retVal_ <- chunk %>% 
        dplyr::filter(diffLog == "event") %>% 
        dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = sum(flow), meanFlow = mean(flow), 
                         midDate = as.Date(median(date), format = "%Y-%m-%d"),
                         beginDate = as.Date(min(date), format = "%Y-%m-%d"), 
                         endDate = as.Date(max(date), format = "%Y-%m-%d")) %>% 
        dplyr::mutate(mn = as.numeric(format(midDate, "%m")), yr = as.numeric(format(midDate, "%Y")),
                      watYr = dplyr::if_else(mn >= 10, yr + 1, yr), 
                      startBaseQ = startBaseQ, endBaseQ = endBaseQ) %>% 
        data.frame()
      
    } else if ((nrow(chunk) - 2) > daysLim) { 
      
      chunk <- dplyr::filter(chunk, diffLog == "event")
      
      pkFlow <- max(chunk$flow)
      
      testDF <- data.frame() 
      
      for (j in seq(1, nrow(chunk) - daysLim, 1)) { 
        
        testDF_ <- data.frame(first = j, last = j + (daysLim - 1), 
                              cumVal = sum(chunk[j:(j + (daysLim - 1)), 2])) 
        
        testDF <- dplyr::bind_rows(testDF, testDF_) 
        
      }
      
      testDF <- dplyr::filter(testDF, cumVal == max(cumVal, na.rm = TRUE)) 
      
      chunk <- chunk[testDF$first:testDF$last, ] 
      
      retVal_ <- chunk %>% 
        dplyr::summarize(nDays = n(), volFlow = sum(flow), meanFlow = mean(flow), 
                         midDate = as.Date(median(date), format = "%Y-%m-%d"), 
                         beginDate = as.Date(min(date), format = "%Y-%m-%d"), 
                         endDate = as.Date(max(date), format = "%Y-%m-%d")) %>% 
        dplyr::mutate(pkFlow = pkFlow) %>% 
        dplyr::mutate(mn = as.numeric(format(midDate, "%m")), yr = as.numeric(format(midDate, "%Y")), 
                      watYr = dplyr::if_else(mn >= 10, yr + 1, yr), 
                      startBaseQ = startBaseQ, endBaseQ = endBaseQ) %>% 
        dplyr::select(nDays, pkFlow, volFlow, midDate, beginDate, endDate, mn, yr, watYr, startBaseQ, endBaseQ) %>% 
        data.frame() 
      
    }
    
    retVal <- dplyr::bind_rows(retVal, retVal_) 
    
  }
  
  return(retVal)
  
}
