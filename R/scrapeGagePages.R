scrapeGagePages <- function(siteIds) { 
  
  library(jsonlite) 
  library(dplyr) 

  results <- data.frame()
  
  for (i in 1:length(siteIds)) {
    
    N <- length(siteIds)
    station <- siteIds[i]
    url <- paste0("https://streamstats.usgs.gov/gagestatsservices/statistics?stationIDOrCode=", station)
    print(paste0(round((i / N) * 100, 2), "%", " complete"))
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
    hold$station_id <- station
    
    if (length(hold) == 1) {
      
      next
      
    } else { 
      
      results <- dplyr::bind_rows(results, hold); rm(finDF)
      
    }
    
  }
  
  return(results)
}

