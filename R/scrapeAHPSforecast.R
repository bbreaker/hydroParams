scrapeAHPSforecast <- function(localName, altName = NA) {
  
  library(dplyr); library(jsonlite)
  
  retDF <- data.frame()
  
  for (i in 1:length(localName)) {
    
    N <- length(localName)
    station <- localName[i]
    altStation <- altName[i]
    
    url <- paste0("https://api.water.noaa.gov/nwps/v1/gauges/", station, "/stageflow") 
    
    hold <- tryCatch(
      fromJSON(url),
      error = function(e) {"failure"}
    )
    
    observedDat <- hold$observed$data %>% 
      dplyr::mutate(dateTime = as.POSIXct(validTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>% 
      dplyr::select(dateTime, primary, secondary) %>% 
      dplyr::mutate(localName = station, altName = altStation, groupName = "Observed") %>% 
      dplyr::select(localName, altName, groupName, dateTime, primary, secondary)
    
    obsPrmrUnits <- hold$observed$primaryUnits
    
    obsScndrUnits <- hold$observed$secondaryUnits
    
    if (obsScndrUnits == "kcfs") {
      
      observedDat <- observedDat %>% 
        dplyr::mutate(secondary = secondary * 1000) %>% 
        dplyr::rename(stage = primary, flow = secondary)
      
    } else if (obsPrmrUnits  == "kcfs") {
      
      observedDat <- observedDat %>% 
        dplyr::mutate(primary = primary * 1000) %>% 
        dplyr::rename(stage = secondary, flow = primary)
      
    } else if (obsScndrUnits == "cfs") {
      
      observedDat <- observedDat %>% 
        dplyr::rename(stage = primary, flow = secondary)
      
    } else if (obsPrmrUnits == "cfs") {
      
      observedDat <- observedDat %>% 
        dplyr::rename(stage = secondary, flow = primary)
      
    }
    
    forecastDat <- hold$forecast$data %>% 
      dplyr::mutate(dateTime = as.POSIXct(validTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>% 
      dplyr::select(dateTime, primary, secondary) %>% 
      dplyr::mutate(localName = station, altName = altStation, groupName = "Forecast") %>% 
      dplyr::select(localName, altName, groupName, dateTime, primary, secondary)
    
    forPrmrUnits <- hold$forecast$primaryUnits
    
    forScndrUnits <- hold$forecast$secondaryUnits
    
    if (forScndrUnits == "kcfs") {
      
      forecastDat <- forecastDat %>% 
        dplyr::mutate(secondary = secondary * 1000) %>% 
        dplyr::rename(stage = primary, flow = secondary)
      
    } else if (forPrmrUnits  == "kcfs") {
      
      forecastDat <- forecastDat %>% 
        dplyr::mutate(primary = primary * 1000) %>% 
        dplyr::rename(stage = secondary, flow = primary)
      
    } else if (forScndrUnits == "cfs") {
      
      forecastDat <- forecastDat %>% 
        dplyr::rename(stage = primary, flow = secondary)
      
    } else if (forPrmrUnits == "cfs") {
      
      forecastDat <- forecastDat %>% 
        dplyr::rename(stage = secondary, flow = primary)
      
    }
    
    retDF_ <- dplyr::bind_rows(observedDat, forecastDat)
    
    retDF <- dplyr::bind_rows(retDF, retDF_)
    
    if (N > 1) {
      
      Sys.sleep(30)
      
    }
    
  }
  
  return(retDF)
  
}
