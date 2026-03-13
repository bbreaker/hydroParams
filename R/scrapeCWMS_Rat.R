## startDate and/or endDate should be a character object that resembles a POSIXct object... ie "2024-02-01 14:45:00"
## startDate and endDate can be left as NULL.

scrapeCWMS_Rat <- function(rating_id, office) {
  
  rating_id <- stringr::str_replace_all(rating_id, "&", "%26")
  
  url <- paste0("https://cwms-data.usace.army.mil/cwms-data/ratings?name=", 
                rating_id, 
                "&office=", office)
  
  url <- stringr::str_replace_all(url, " ", "%20")
  url <- stringr::str_replace_all(url, ";", "%3B")
  
  library(curl)
  h <- new_handle(ssl_verifypeer = 0L)
  
  req <- curl_fetch_memory(url, handle = h)
  
  parsed_xml <- xml2::read_xml(req$content)
  
  rat <- xml2::xml_text(xml2::xml_find_all(parsed_xml, ".//rating-points"))
  
  ratExp <- unlist(stringr::str_split(rat, pattern = "\n"))
  
  ratTable <- data.frame(DEP = as.numeric(stringr::str_split_i(ratExp, " ", 1)), 
                         INDEP = as.numeric(stringr::str_split_i(ratExp, " ", 2)))
  
  return(ratTable)
}
