## startDate and/or endDate should be a character object that resembles a POSIXct object... ie "2024-02-01 14:45:00"
## startDate must be specified, endDate can be left as NULL.
## Leaving as only dealing with UTC for now. Might change this later. 
## The user needs to specify 'regular' or 'irregular' for the time series typ. It seems like daily is 'irregular' and sub-daily is 'regular'.
## Right now, this function will really only work with daily and hourly... need to make it recognize 'regular' time-step intervals and create the datetime sequence appropriatly.

scrapeCWMS_TS <- function(time_series_id, office, startDate, endDate = NULL, 
                          dataType = "irregular") {
  
  if (is.null(endDate)) {
    
    newStart <- paste0(format(as.POSIXct(startDate), "%Y-%m-%d"), "T", 
                       format(as.POSIXct(startDate), "%H"), "%3A", 
                       format(as.POSIXct(startDate), "%M"))
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/timeseries?name=", 
                  time_series_id, 
                  "&office=", office, 
                  "&begin=", newStart)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  } else {
    
    newStart <- paste0(format(as.POSIXct(startDate), "%Y-%m-%d"), "T", 
                       format(as.POSIXct(startDate), "%H"), "%3A", 
                       format(as.POSIXct(startDate), "%M"))
    
    newEnd <- paste0(format(as.POSIXct(endDate), "%Y-%m-%d"), "T", 
                     format(as.POSIXct(endDate), "%H"), "%3A", 
                     format(as.POSIXct(endDate), "%M"))
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/timeseries?name=", 
                  time_series_id, 
                  "&office=", office, 
                  "&begin=", newStart, 
                  "&end=", newEnd)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  }
  
  if (dataType == "irregular") {
    
    newDat <- data.frame(hold[[1]][[3]][[4]][[3]][[1]]) %>% 
      dplyr::mutate(dateTime = as.POSIXct(X1, "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>% 
      dplyr::rename(value = X2) %>% 
      dplyr::select(dateTime, value)
    
  } else if (dataType == "regular") {
    
    testIt <- hold$`time-series`$`time-series`[[4]]
    
    if (any(testIt == "Not enough memory for CONNECT BY operation")) {
      
      newDat <- "try shorter period"
      
    } else {
      
      tStep <- hold$`time-series`$`time-series`$`regular-interval-values`$interval
      
      tStep <- stringr::str_remove(tStep, "PT")
      
      tStepNum <- readr::parse_number(tStep)
      
      tStepAlpha <- stringr::str_remove(tStep, as.character(tStepNum))
      
      tStepSeq <- ifelse(tStepAlpha == "H", "hour", "min")
      
      newDat <- data.frame(index = seq(from = as.POSIXct(hold$`time-series`$`time-series`$`regular-interval-values`$segments[[1]]$`first-time`, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), 
                                       to = as.POSIXct(hold$`time-series`$`time-series`$`regular-interval-values`$segments[[1]]$`last-time`, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), 
                                       by = paste(tStepNum, tStepSeq))) %>% 
        dplyr::mutate(value = hold$`time-series`$`time-series`$`regular-interval-values`$segments[[1]]$`values`[[1]][, 1])
      
    }
    
  } else {
    
    newDat <- "Something went wrong..."
    
  }
  
  return(newDat)
  
}
