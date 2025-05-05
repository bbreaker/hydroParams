## 'basinFile' 
##  - character string specifying file path to .basin file to be updated
## 'paramDF'
##  - data frame containing a columns 
##    - Name = subbasin names in the .basin file for which parameters are to be 
##      updated  
##    - parameters to update in the .basin file, names should match 'paramMap' 
##      defaults or whatever is specified
## 'outputPath' 
##  - character string specifying file path to .basin file to be written out
## 'paramMap'
##  - named character vector containing name from .basin file (make sure to include
##    the ':' after the name) and name from paramDF that is specified to update
##    parameters in the .basin file
##  - the default is;
##                   paramMap <- c("Initial Deficit:" = "initDef",
##                                 "Percolation Rate:" = "percRate",
##                                 "GW-2 Routing Coefficient:" = "gw2Time",
##                                 "GW-2 Initial Flow/Area Ratio:" = "gw2InitFlow")

library(dplyr)
library(stringr)

updateBasinFile <- function(basinFile, paramDF, outputPath, paramMap = NULL) {
  # Read original .basin file
  lines <- readLines(basinFile)
  linesOut <- lines
  
  # Define parameter mapping: .basin file line -> data frame column
  if (is.null(paramMap)) {
    
    paramMap <- c("Initial Deficit:" = "initDef", 
                  "Percolation Rate:" = "percRate", 
                  "GW-2 Routing Coefficient:" = "gw2Time", 
                  "GW-2 Initial Flow/Area Ratio:" = "gw2InitFlow")
  }
  
  else { 
    
    paramMap <- paramMap
  }
  
  
  # Locate Subbasin lines
  subbasinIndices <- grep("Subbasin", lines, ignore.case = TRUE)
  endRf <- c("End:", "End Zone Configuration:", "End Computation Point:")
  endIndices <- grep(paste(endRf, collapse = "|"), lines)
  
  for (i in 1:length(subbasinIndices)) {
    idx <- subbasinIndices[i]
    subName <- stringr::str_remove(lines[idx], "Subbasin: ") 
    #subName <- str_match(lines[idx], 'Subbasin\\s+"([^"]+)"')[,2]
    
    if (is.na(subName) || !(subName %in% paramDF$Name)) next
    
    # Define block range for this subbasin
    end_idx <- endIndices[which(endIndices > idx)][1]
    block <- lines[idx:end_idx]
    df_row <- paramDF %>% filter(Name == subName)
    
    for (basinParam in names(paramMap)) {
      df_col <- paramMap[basinParam]
      value <- df_row[[df_col]]
      
      # Ensure correct whitespace in match
      pattern <- paste0("^\\s*", basinParam, "\\s+")
      replacement <- paste0("     ", basinParam, " ", value)
      
      match_line <- grep(pattern, block)
      
      if (length(match_line) > 0) {
        block[match_line] <- replacement
      } else {
        # Append if not found
        block <- append(block, replacement, after = 1)
      }
    }
    
    # Replace updated block in original lines
    linesOut[idx:end_idx] <- block
  }
  
  # Write updated file
  writeLines(linesOut, outputPath)
  cat("Updated .basin file written to", outputPath, "\n")
}