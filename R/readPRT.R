readPRT <- function(prtFile) { 
  newText <- readLines(prtFile)
  topOut1 <- "                            MEAN     DEVIATION     SKEW "
  newTbl1 <- newText[-c(1:grep(pattern = topOut1, newText))]
  newTbl1 <- newTbl1[2:3]
  newTbl1 <- gsub("EMA W/O REG. INFO", "STATION", newTbl1)
  newTbl1 <- gsub("EMA W/REG. INFO", "REGIONAL", newTbl1)
  newTbl1 <- read.table(text = newTbl1, stringsAsFactors = FALSE)
  names(newTbl1) <- c("type", "mu", "sigma", "gamma")
  topOut2 <- "PROBABILITY ESTIMATE  ESTIMATE     OF EST.       LOWER       UPPER"
  newTbl2 <- newText[-c(1:grep(pattern = topOut2, newText))]
  newTbl2 <- newTbl2[2:16]
  for(i in 1:length(newTbl2)) {
    testTbl2 <- newTbl2[i]
    testVal <- grepl("--", testTbl2)
    if(testVal == TRUE) {
      test1 <- stringr::str_sub(testTbl2, 1, 9)
      test2 <- stringr::str_sub(testTbl2, 10, nchar(testTbl2))
      test1 <- paste0(test1, " 0.")
      newTbl2[i] <- paste0(test1, test2)
    } else {
      newTbl2[i] <- newTbl2[i]
    }
  }
  #newTbl2 <- gsub("--", )
  newTbl2 <- read.table(text = newTbl2, stringsAsFactors = FALSE)
  names(newTbl2) <- c("AEP", "emaStation", "emaRegional", "variance", "lwr", "upr")
  topOut3 <- "    YEAR   DISCHARGE   ESTIMATE      LOW      HIGH"
  newTbl3 <- newText[-c(1:grep(pattern = topOut3, newText))]
  newTbl3 <- newTbl3[c(1:which(newTbl3 == ""))]
  newTbl3 <- newTbl3[-c(length(newTbl3))]
  newTbl3F <- data.frame(read.table(text = newTbl3[1]), stringsAsFactors = FALSE)
  newTbl3F$comment <- "Peak Used"
  for (i in 2:length(newTbl3)) {
    newTbl3_ <- data.frame(read.table(text = newTbl3[i])) 
    if(ncol(newTbl3_) == 4) {
      newTbl3_ <- newTbl3_[, -1]
      names(newTbl3_) <- c("V1", "V2", "V3")
      newTbl3_$comment <- "PILF"
    } else {
      newTbl3_$comment <- "Peak Used"
    }
    newTbl3F <- dplyr::bind_rows(newTbl3F, newTbl3_)
  }
  names(newTbl3F) <- c("watYr", "pkFlow", "hspp", "comment")
  prtList <- list()
  prtList$parms <- newTbl1
  prtList$estimates <- newTbl2
  prtList$plotPositions <- newTbl3F
  return(prtList)
}
