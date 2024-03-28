## startDate and/or endDate should be a character object that resembles a POSIXct object... ie "2024-02-01 14:45:00"
## startDate must be specified, endDate can be left as NULL.
## Leaving as only dealing with UTC for now. Might change this later.

scrapeCWMS_TS <- function(time_series_id, office, startDate, endDate = NULL) {
  
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
  
  newDat <- data.frame(hold[[1]][[3]][[4]][[3]][[1]]) %>% 
    dplyr::mutate(dateTime = as.POSIXct(X1, "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>% 
    dplyr::rename(value = X2) %>% 
    dplyr::select(dateTime, value)
  
  return(newDat)
  
}
