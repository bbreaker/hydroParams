scrapeAHPSmeta <- function(localName, altName = NULL) {
  
  library(rvest) 
  library(dplyr) 
  library(stringr) 
  
  finList <- list(location = data.frame(stringsAsFactors = FALSE), 
                  datumInfo = data.frame(stringsAsFactors = FALSE))
  
  for (i in 1:length(localName)) {
    
    N <- length(localName)
    station <- localName[i]
    altStation <- altName[i]
    url <- paste0("https://water.weather.gov/ahps2/metadata.php?wfo=lzk&gage=", station)
    #print(paste0(round((i / N) * 100, 2), "%", " complete"))
    
    hold <- tryCatch(
      url %>%
        read_html() %>% 
        html_nodes('table'),
      error = function(e) {"failure"}
    )
    
    if (any(hold == "failure")) {
      
      holdInfo <- list(location = data.frame(localName = localName[i], altName = ifelse(is.null(altName), NA, altName[i]), 
                                             dec_lat_va = NA, dec_long_va = NA, stringsAsFactors = FALSE),  
                       datumInfo = data.frame(localName = localName[i], altName = ifelse(is.null(altName), NA, altName[i]), 
                                              desc = NA, datum = NA, flood = NA, 
                                              explanation = NA, stringsAsFactors = FALSE))
      
      finList[[1]] <- dplyr::bind_rows(finList[[1]], holdInfo[[1]])
      finList[[2]] <- dplyr::bind_rows(finList[[2]], holdInfo[[2]])

    } else {
      
      holdTables <- hold %>%
        html_nodes(xpath = paste0('/html/body/table[1]')) %>% 
        html_table(fill = TRUE) %>% 
        as.data.frame() %>% 
        dplyr::select(X1, X2, X3, X4)
      
      loc <- unlist(str_split_fixed(holdTables[2, 1], ",", n = 3))
      
      dec_lat <- unlist(str_split_fixed(loc[1, 1], ": ", n = 2)) [2]
      
      dec_long <- unlist(str_split_fixed(loc[1, 2], ": ", n = 2)) [2]
      
      desc <- holdTables[4:8, 1]
      
      datum <- holdTables[4:8, 2]
      
      flood <- holdTables[4:8, 3]
      
      explanation <- holdTables[4:8, 4]
        
      holdInfo <- list(location = data.frame(localName = localName[i], altName = ifelse(is.null(altName), NA, altName[i]), 
                                             dec_lat = dec_lat, dec_long = dec_long, stringsAsFactors = FALSE), 
                       datumInfo = data.frame(localName = localName[i], altName = ifelse(is.null(altName), NA, altName[i]), 
                                              desc = desc, datum = datum, flood = flood, 
                                              explanation = explanation, stringsAsFactors = FALSE))
      
      finList[[1]] <- dplyr::bind_rows(finList[[1]], holdInfo[[1]])
      finList[[2]] <- dplyr::bind_rows(finList[[2]], holdInfo[[2]])

    }
    
  }
  
  return(finList)
  
}
