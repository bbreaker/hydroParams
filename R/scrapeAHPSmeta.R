scrapeAHPSmeta <- function(localName, altName = NULL, getSimpDF = FALSE) {
  
  url <- paste0("https://api.water.noaa.gov/nwps/v1/gauges/", localName) 
  
  hold <- tryCatch(
    fromJSON(url),
    error = function(e) {"failure"}
  )
  
  if (getSimpDF == TRUE) {
    
    retDF <- data.frame() 
    
    for (i in 1:length(hold$flood$categories)) {
      
      catVals <- data.frame(hold$flood$categories[[i]])
      
      catNames <- names(hold$flood$categories)[i]
      
      retDF_ <- data.frame(dataType = names(hold$flood$categories)[i]) %>% 
        dplyr::bind_cols(valStage = as.character(catVals$stage), 
                         valFlow = as.character(catVals$flow))
      
      retDF <- dplyr::bind_rows(retDF, retDF_)
        
    }
    
    retDF_ <- data.frame(dataType = c("conversionElevUnit", "conversionElev"), 
                         valStage = c(hold$datums$vertical$value$abbrev, 
                                      as.character(hold$datums$vertical$value$value)))
    
    retDF <- dplyr::bind_rows(retDF, retDF_)
    
    return(retDF)
    
  }
  
  return(hold) 

}
