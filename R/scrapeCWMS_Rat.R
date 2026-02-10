## startDate and/or endDate should be a character object that resembles a POSIXct object... ie "2024-02-01 14:45:00"
## startDate and endDate can be left as NULL.

scrapeCWMS_Rat <- function(rating_id, office, startDate = NULL, endDate = NULL) {
  
  rating_id <- stringr::str_replace_all(rating_id, "&", "%26")
  
  if (!is.null(startDate) & is.null(endDate)) {
    
    newStart <- paste0(format(as.POSIXct(startDate), "%Y-%m-%d"), "T", 
                       format(as.POSIXct(startDate), "%H"), "%3A", 
                       format(as.POSIXct(startDate), "%M"))
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/ratings/", 
                  rating_id, 
                  "?office=", office, 
                  "&begin=", newStart)
    
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
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/ratings/", 
                  rating_id, 
                  "?office=", office, 
                  "&begin=", newStart, 
                  "&end=", newEnd)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    url <- stringr::str_replace_all(url, ";", "%3B")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  } else {
    
    url <- paste0("https://cwms-data.usace.army.mil/cwms-data/ratings/", 
                  rating_id, 
                  "?office=", office)
    
    url <- stringr::str_replace_all(url, " ", "%20")
    url <- stringr::str_replace_all(url, ";", "%3B")
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
  }
  
  newDat <- data.frame(hold[[4]]$`rating-points`)
  
  for (i in 1:nrow(newDat)) {
    
    if (i == 1) {
      
      retRat <- newDat[[1]][[i]]
    } else {
      
      retRat <- dplyr::bind_rows(retRat, newDat[[1]][[i]])
    }
  }
    
  retRat <- retRat %>% 
    dplyr::mutate(isDup = duplicated(ind)) %>% 
    dplyr::filter(isDup != TRUE) %>% 
    dplyr::select(-isDup) %>% 
    dplyr::mutate(ind = as.numeric(ind), 
                  dep = as.numeric(dep)) %>% 
    dplyr::arrange(ind)
  
  return(retRat)
  
}
