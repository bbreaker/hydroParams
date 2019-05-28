scrapeAHPSforecast <- function(localName, altName = NA) {
  
  library(rvest) 
  library(dplyr) 
  library(stringr) 
  
  results <- data.frame(localName = as.character(), 
                        altName = as.character(), 
                        groupName = as.character(), 
                        datetime = as.character(), 
                        stage = as.character(), 
                        flow = as.character(), 
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(localName)) {
    
    N <- length(localName)
    station <- localName[i]
    altStation <- altName[i]
    url <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=", station, "&output=tabular")
    #print(paste0(round((i / N) * 100, 2), "%", " complete"))
    
    hold <- tryCatch(
      url %>%
        read_html() %>% 
        html_nodes('table'),
      error = function(e) {"failure"}
    )
    
    if (any(hold == "failure")) {
      
      finDF <- data.frame(localName = station, 
                          altName = altStation, 
                          groupName = "failed", 
                          datetime = "failed", 
                          value = "failed", 
                          unit = "failed", 
                          stringsAsFactors = FALSE)
      
      results <- dplyr::bind_rows(results, finDF)
      
    } else {
      
      holdTables <- hold %>%
        html_nodes(xpath = paste0('/html/body/table[1]')) %>% 
        html_table(fill = TRUE) %>% 
        as.data.frame()
      
      holdTables <- holdTables[-1, ]
      
      if (!any(grepl("Flow", holdTables))) {
        
        holdTables <- holdTables[, 1:2]
        
        fCastSplt <- "Forecast"
        
        holdFcast <- holdTables[(grep(pattern = fCastSplt, holdTables$X1)):(nrow(holdTables)), ]
        
        holdFcast1 <- holdFcast %>% 
          dplyr::slice(-1, -2) %>% 
          dplyr::mutate(date = stringr::str_sub(X1, 1, 5), 
                        time = stringr::str_sub(X1, 7, 11), 
                        datetime = paste0(date, "/19 ", time)) %>% 
          dplyr::mutate(localName = station, 
                        altName = altStation, 
                        groupName = "forecast", 
                        datetime = as.POSIXct(datetime, format = "%m/%d/%y %H:%M", tz = "GMT"))
        
        fCastStageN <- as.numeric(str_remove(holdFcast1$X2, "[aA-zZ]+"))
        
        fCastStageC <- as.character(str_extract(holdFcast1$X2, "[aA-zZ]+"))
        
        fCastAddDF <- data.frame(stage = fCastStageN, flow = NA)
        
        holdFcast2 <- holdFcast1 %>% 
          dplyr::select(localName, altName, groupName, datetime) %>% 
          bind_cols(fCastAddDF)
        
        holdObs <- holdTables[1:(grep(pattern = fCastSplt, holdTables$X1) - 1), ]
        
        holdObs1 <- holdObs %>% 
          dplyr::slice(-1, -2) %>% 
          dplyr::mutate(date = stringr::str_sub(X1, 1, 5), 
                        time = stringr::str_sub(X1, 7, 11), 
                        datetime = paste0(date, "/19 ", time)) %>% 
          dplyr::mutate(localName = station, 
                        altName = altStation, 
                        groupName = "observed", 
                        datetime = as.POSIXct(datetime, format = "%m/%d/%y %H:%M", tz = "GMT"))
        
        obsStageN <- as.numeric(str_extract(holdObs1$X2, "[0-9]+"))
        
        obsStageC <- as.character(str_extract(holdObs1$X2, "[aA-zZ]+"))
        
        obsAddDF <- data.frame(stage = obsStageN, flow = NA)
        
        holdObs2 <- holdObs1 %>% 
          dplyr::select(localName, altName, groupName, datetime) %>% 
          bind_cols(obsAddDF)
        
        finDF <- dplyr::bind_rows(holdObs2, holdFcast2)
        
        finDF <- arrange(finDF, datetime)
        
        results <- rbind(results, finDF)
        
      } else {
        
        holdTables <- holdTables[, 1:3]
        
        fCastSplt <- "Forecast"
        
        holdFcast <- holdTables[(grep(pattern = fCastSplt, holdTables$X1)):(nrow(holdTables)), ]
        
        holdFcast1 <- holdFcast %>% 
          dplyr::slice(-1, -2) %>% 
          dplyr::mutate(date = stringr::str_sub(X1, 1, 5), 
                        time = stringr::str_sub(X1, 7, 11), 
                        datetime = paste0(date, "/19 ", time)) %>% 
          dplyr::mutate(localName = station, 
                        altName = altStation, 
                        groupName = "forecast", 
                        datetime = as.POSIXct(datetime, format = "%m/%d/%y %H:%M", tz = "GMT"))
        
        fCastStageN <- as.numeric(str_remove(holdFcast1$X2, "[aA-zZ]+"))
        
        fCastStageC <- as.character(str_extract(holdFcast1$X2, "[aA-zZ]+"))
        
        fCastFlowN <- as.numeric(str_remove(holdFcast1$X3, "[aA-zZ]+"))
        
        fCastFlowC <- as.character(str_extract(holdFcast1$X3, "[aA-zZ]+"))
        
        fCastAddDF <- data.frame(stage = fCastStageN, flow = fCastFlowN, flowC = fCastFlowC)
        
        fCastAddDF <- fCastAddDF %>% 
          dplyr::mutate(flow = ifelse(flowC == "kcfs", flow * 1000, flow)) %>% 
          dplyr::select(-flowC)
        
        holdFcast2 <- holdFcast1 %>% 
          dplyr::select(localName, altName, groupName, datetime) %>% 
          bind_cols(fCastAddDF)
        
        holdObs <- holdTables[1:(grep(pattern = fCastSplt, holdTables$X1) - 1), ]
        
        holdObs1 <- holdObs %>% 
          dplyr::slice(-1, -2) %>% 
          dplyr::mutate(date = stringr::str_sub(X1, 1, 5), 
                        time = stringr::str_sub(X1, 7, 11), 
                        datetime = paste0(date, "/19 ", time)) %>% 
          dplyr::mutate(localName = station, 
                        altName = altStation, 
                        groupName = "observed", 
                        datetime = as.POSIXct(datetime, format = "%m/%d/%y %H:%M", tz = "GMT"))
        
        obsStageN <- as.numeric(str_extract(holdObs1$X2, "[0-9]+"))
        
        obsStageC <- as.character(str_extract(holdObs1$X2, "[aA-zZ]+"))
        
        obsFlowN <- as.numeric(str_extract(holdObs1$X3, "[0-9]+"))
        
        obsFlowC <- as.character(str_extract(holdObs1$X3, "[aA-zZ]+"))
        
        obsAddDF <- data.frame(stage = obsStageN, flow = obsFlowN, flowC = obsFlowC)
        
        obsAddDF <- obsAddDF %>% 
          dplyr::mutate(flow = ifelse(flowC == "kcfs", flow * 1000, flow)) %>% 
          dplyr::select(-flowC)
        
        holdObs2 <- holdObs1 %>% 
          dplyr::select(localName, altName, groupName, datetime) %>% 
          bind_cols(obsAddDF)
        
        finDF <- dplyr::bind_rows(holdObs2, holdFcast2)
        
        finDF <- arrange(finDF, datetime)
        
        results <- rbind(results, finDF)
        
      }
    
    }
    
  }
  
  return(results)
  
}

