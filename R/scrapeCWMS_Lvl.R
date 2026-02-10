scrapeCWMS_Lvl <- function(level_id, office, unit = "feet", startDate = NULL, endDate = NULL) {
  
  level_id <- stringr::str_replace_all(level_id, "&", "%26")
  
  if (!is.null(startDate) & is.null(endDate)) {
    
    newStart <- paste0(format(as.POSIXct(startDate), "%Y-%m-%d"), "T", 
                       format(as.POSIXct(startDate), "%H"), "%3A", 
                       format(as.POSIXct(startDate), "%M"))
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/levels/", 
                  level_id, 
                  "/timeseries", 
                  "?office=", office, 
                  "&begin=", newStart, 
                  "&unit=", unit)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    url <- stringr::str_replace_all(url, ";", "%3B")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  } else if (!is.null(startDate) & !is.null(endDate)) {
    
    newStart <- paste0(format(as.POSIXct(startDate), "%Y-%m-%d"), "T", 
                       format(as.POSIXct(startDate), "%H"), "%3A", 
                       format(as.POSIXct(startDate), "%M"))
    
    newEnd <- paste0(format(as.POSIXct(endDate), "%Y-%m-%d"), "T", 
                     format(as.POSIXct(endDate), "%H"), "%3A", 
                     format(as.POSIXct(endDate), "%M"))
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/levels/", 
                  level_id, 
                  "/timeseries", 
                  "?office=", office, 
                  "&begin=", newStart, 
                  "&end=", newEnd, 
                  "&unit=", unit)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    url <- stringr::str_replace_all(url, ";", "%3B")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  } else {
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/levels/", 
                  level_id, 
                  "/timeseries", 
                  "?office=", office, 
                  "&unit=", unit)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    url <- stringr::str_replace_all(url, ";", "%3B")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  }
  
  retLev <- hold$values %>% 
    data.frame() %>% 
    dplyr::rename(index = 1, value = 2, flag = 3) %>% 
    dplyr::mutate(index = as.numeric(index), 
                  value = as.numeric(value)) %>% 
    dplyr::mutate(index = as.POSIXct(index/1000, tz = "UTC"))
  
  return(retLev)
  
}
