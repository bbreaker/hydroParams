scrapeBasinChars <- function(siteIds) { 
  
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
        html_nodes(xpath = '/html/body/table[2]') %>%
        html_table(fill = TRUE) %>%
        as.data.frame(),
      error = function(e) {"failure"}
    )
    
    if (hold == "failure") {
      
      finDF <- data.frame(site_no = station, 
                          statGrp = as.character(), 
                          stat = "failed",
                          val = 0,
                          unit = "failed", 
                          stringsAsFactors = FALSE)
      
    } else {
      
      hold <- hold[-c(1:3, 4), c(1:3)]
      statHdrs <- if_else(hold$X1 == hold$X2, hold$X1, "notAHdr")
      statHdrs <- statHdrs[!grepl("notAHdr", statHdrs)]
      statHdrs <- statHdrs[nchar(statHdrs) > 0]

      for (j in 1:length(statHdrs)) {
        if (j < length(statHdrs)) {
          holdRef <- hold[c(grep(statHdrs[j], hold$X1):grep(statHdrs[j + 1], hold$X1)), ]
          holdRef <- holdRef[-c(1, nrow(holdRef)), ]
          finDF <- data.frame(site_no = station, 
                              statGrp = statHdrs[j], 
                              stat = holdRef$X1, 
                              val = holdRef$X2, 
                              unit = holdRef$X3, 
                              stringsAsFactors = FALSE)
        } else {
          holdRef <- hold[c(grep(statHdrs[j], hold$X1):nrow(hold)), ]
          holdRef <- holdRef[-1, ]
          holdRef <- holdRef[!(holdRef$X1 == ""), ]
          finDF <- data.frame(site_no = station, 
                              statGrp = statHdrs[j], 
                              stat = holdRef$X1, 
                              val = holdRef$X2, 
                              unit = holdRef$X3, 
                              stringsAsFactors = FALSE)
        }
        
        results <- dplyr::bind_rows(results, finDF); rm(finDF)
        
      }
      
    }
    
  }
  
  return(results)
}
