scrapeGagePages <- function(siteIds) { 
  
  library(rvest) 
  library(dplyr) 
  library(stringr) 
  
  results <- data.frame(site_no = as.character(), 
                        statGrp = as.character(), 
                        stat = as.character(), 
                        val = as.character(), 
                        unit = as.character(), 
                        stringsAsFactors = FALSE)
  
  for (i in 1:length(siteIds)) {
    
    N <- length(siteIds)
    station <- siteIds[i]
    url <- paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/", station, ".htm")
    print(paste0(round((i / N) * 100, 2), "%", " complete"))
    
    hold <- tryCatch(
      url %>%
        read_html() %>% 
        html_nodes('table'),
      error = function(e) {"failure"}
    )
    
    if (hold == "failure") {
      
      finDF <- data.frame(site_no = station, 
                          statGrp = "failed", 
                          stat = "failed", 
                          val = "failed", 
                          unit = "failed", 
                          stringsAsFactors = FALSE)
      
    } else {
      
      for (j in 2:length(hold)) {
        if (j == 2) {
          holdTables <- hold %>%
            html_nodes(xpath = paste0('/html/body/table[', j, ']')) %>%
            html_table(fill = TRUE) %>%
            as.data.frame()
        } else {
          holdTables_ <- hold %>%
            html_nodes(xpath = paste0('/html/body/table[', j, ']')) %>%
            html_table(fill = TRUE) %>%
            as.data.frame()
          holdTables <- dplyr::bind_rows(holdTables, holdTables_)
        }
      }
      
      holdTables <- holdTables[, c(1:3)]
      holdTables <- dplyr::filter(holdTables, !X1 == "")
      statHdrs <- if_else(holdTables$X1 == holdTables$X2, holdTables$X1, "notAHdr")
      statHdrs <- statHdrs[!grepl("notAHdr", statHdrs)]
      statHdrs <- statHdrs[nchar(statHdrs) > 0]
      
      for (j in 1:length(statHdrs)) {
        if (j < length(statHdrs)) {
          holdRef <- holdTables[c(grep(statHdrs[j], holdTables$X1):grep(statHdrs[j + 1], holdTables$X1)), ]
          holdRef <- holdRef[-c(1, nrow(holdRef)), ]
          finDF <- data.frame(site_no = station, 
                              statGrp = statHdrs[j], 
                              stat = holdRef$X1, 
                              val = holdRef$X2, 
                              unit = holdRef$X3, 
                              stringsAsFactors = FALSE)
        } else {
          holdRef <- holdTables[c(grep(statHdrs[j], holdTables$X1):nrow(holdTables)), ]
          holdRef <- holdRef[-1, ]
          holdRef <- holdRef[!(holdRef$X1 == ""), ]
          finDF <- data.frame(site_no = station, 
                              statGrp = statHdrs[j], 
                              stat = holdRef$X1, 
                              val = holdRef$X2, 
                              unit = holdRef$X3, 
                              stringsAsFactors = FALSE)
        }
        
      }
      
    }
    
    results <- dplyr::bind_rows(results, finDF); rm(finDF)
    
  }
  
  return(results)
}
