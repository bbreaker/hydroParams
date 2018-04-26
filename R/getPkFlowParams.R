getPkFlowParams <- function(siteIds) {
  
  library(rvest) 
  library(dplyr)    
  library(stringr)
  
  stats <- c("EMA_Mean_of_Logs_of_Annual_Peaks",
             "EMA_Std_Dev_of_Logs_of_Annual_Peaks",
             "EMA_Skew_of_Logs_of_Annual_Peaks")
  
  results <- data.frame(site_no = as.character(),
                        stats = as.character(),
                        val = as.numeric(),
                        stringsAsFactors = FALSE)
  
  for(i in 1:length(siteIds)) {
    
    N <- length(siteIds)
    station <- siteIds[i]
    url <- paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/", station, ".htm")
    print(paste0(round((i/N)*100,2),"%"," complete"))
    
    hold <- tryCatch(
      url %>%
        read_html() %>%
        html_nodes(xpath = '/html/body/table[3]') %>%
        html_table() %>%
        as.data.frame(),
      error = function(e){"failure"}
    )
    
    if(hold == "failure") {
      
      finDF <- data.frame(site_no = station, 
                          stats = "failed",
                          val = 0,
                          stringsAsFactors = FALSE)
      
    }else if(!(stats %in% hold$X1)[1] == TRUE){
      
      finDF <- data.frame(site_no = station, 
                          stats = "failed",
                          val = 0,
                          stringsAsFactors = FALSE)
      
    }else{
      
      statsNew <- (hold[(hold$X1 %in% stats),])[, 1]
      vals <- dplyr::filter(hold, X1 %in% stats)[, 2]
      finDF <- data.frame(site_no = rep(station, length(vals)), 
                          stats = statsNew, 
                          val = as.numeric(vals),
                          stringsAsFactors = FALSE)
    }
    
    results <- dplyr::bind_rows(results, finDF); rm(finDF)
    
  }
  
  return(results)
}
