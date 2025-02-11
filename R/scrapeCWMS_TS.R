## startDate and/or endDate should be a character object that resembles a POSIXct object... ie "2024-02-01 14:45:00"
## startDate must be specified, endDate can be left as NULL.
## Leaving as only dealing with UTC for now. Might change this later. 
## The user needs to specify 'regular' or 'irregular' for the time series typ. It seems like daily is 'irregular' and sub-daily is 'regular'.
## Right now, this function will really only work with daily and hourly... need to make it recognize 'regular' time-step intervals and create the datetime sequence appropriatly.

scrapeCWMS_TS <- function(time_series_id, office, startDate, endDate = NULL, 
                          dataType = "irregular") {
  
  time_series_id <- stringr::str_replace_all(time_series_id, "&", "%26")
  
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
                  "&end=", newEnd, 
                  "&page-size=15000")
    
    url <- stringr::str_replace_all(url, " ", "%20")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  }
  
  newDat <- hold$values %>% 
    data.frame() %>% 
    dplyr::rename(index = 1, value = 2, flag = 3) %>% 
    dplyr::mutate(index = as.POSIXct(index/1000, tz = "UTC"))
  
  return(newDat)
  
}
