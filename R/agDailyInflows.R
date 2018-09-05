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
    newDF$runOff <- if_else(newDF$runOff < (newDF$flow * 0.05), 0, round(newDF$runOff, 2))
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
    dplyr::mutate(qualk = if_else(values == "base", 0, 1)) %>% 
    group_by(qualk) %>% 
    dplyr::mutate(index = ifelse(qualk == 0, 0, 1:n())) %>% 
    data.frame()
  
  # create an empty data frame to store information about the events
  retVal <- data.frame()
  
  # add data for event peak, event volume, event date, and water year
  for (i in seq(2, max(newDFRLEDF$index) - 1, 1)) {
    
    chunk <- newDFRLEDF[(which(newDFRLEDF$index == i) - 1):which(newDFRLEDF$index == i), ] 
    
    chunk <- newDF[(chunk[1, 3]):(chunk[2, 3] + 1), ] 
    
    if(is.null(daysLim)) {
      
      retVal_ <- chunk %>% 
        dplyr::filter(diffLog == "event") %>% 
        dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = (sum(flow) * 86400), 
                         date = as.Date(median(date), format = "%Y-%m-%d")) %>% 
        dplyr::mutate(mn = as.numeric(format(date, "%m")), yr = as.numeric(format(date, "%Y")),
                      watYr = if_else(mn >= 10, yr + 1, yr)) %>% 
        data.frame()
      
    } else if (nrow(chunk) <= daysLim) {
      
      retVal_ <- chunk %>% 
        dplyr::filter(diffLog == "event") %>% 
        dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = (sum(flow) * 86400), 
                         date = as.Date(median(date), format = "%Y-%m-%d")) %>% 
        dplyr::mutate(mn = as.numeric(format(date, "%m")), yr = as.numeric(format(date, "%Y")),
                      watYr = if_else(mn >= 10, yr + 1, yr)) %>% 
        data.frame()
      
    } else {
      
      if ((nrow(chunk) - 1) == daysLim) {
        
        retVal_ <- chunk %>% 
          dplyr::filter(diffLog == "event") %>% 
          dplyr::top_n(-1, flow) %>% 
          dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = (sum(flow) * 86400), 
                           date = as.Date(median(date), format = "%Y-%m-%d")) %>% 
          dplyr::mutate(mn = as.numeric(format(date, "%m")), yr = as.numeric(format(date, "%Y")),
                        watYr = if_else(mn >= 10, yr + 1, yr)) %>% 
          data.frame()
        
      } else {
        
        testDF <- data.frame()
        
        for (j in seq(1, nrow(chunk) - daysLim, 1)) {
          
          testDF_ <- data.frame(first = j, last = j + (daysLim - 1), 
                                cumVal = sum(chunk[j, 2]:chunk[j + (daysLim - 1), 2]))
          testDF <- dplyr::bind_rows(testDF, testDF_)
          
        }
        
        testDF <- dplyr::filter(testDF, cumVal == max(cumVal, na.rm = TRUE))
        
        chunk <- chunk[testDF$first:testDF$last, ]
        
        retVal_ <- chunk %>% 
          dplyr::filter(diffLog == "event") %>% 
          dplyr::summarize(nDays = n(), pkFlow = max(flow), volFlow = (sum(flow) * 86400), 
                           date = as.Date(median(date), format = "%Y-%m-%d")) %>% 
          dplyr::mutate(mn = as.numeric(format(date, "%m")), yr = as.numeric(format(date, "%Y")),
                        watYr = if_else(mn >= 10, yr + 1, yr)) %>% 
          data.frame() 
        
        }
      
    }
    
    retVal <- dplyr::bind_rows(retVal, retVal_) 
    
    }
  
  return(retVal)
  
}
