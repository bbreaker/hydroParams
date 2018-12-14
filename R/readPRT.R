readPRT <- function(prtFile) { 
  newText <- readLines(prtFile)
  nPks <- "Number of peaks in record"
  nPks <- newText[grep(pattern = nPks, newText)]
  nPks <- as.numeric(unlist(stringr::str_split(nPks, "="))[2])
  nPksUsed <- "Systematic peaks in analysis"
  nPksUsed <- newText[grep(pattern = nPksUsed, newText)]
  nPksUsed <- as.numeric(unlist(stringr::str_split(nPksUsed, "="))[2])
  skewOpt <- "Skew option"
  skewOpt <- newText[grep(pattern = skewOpt, newText)]
  skewOpt <- as.character(unlist(stringr::str_split(skewOpt, "="))[2])
  newTbl <- data.frame(nPeaks = nPks, nPeaksUsed = nPksUsed, 
                       skewOption = skewOpt, stringsAsFactors = FALSE)
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
      test1 <- paste0(test1, " NA")
      newTbl2[i] <- paste0(test1, test2)
    } else {
      newTbl2[i] <- newTbl2[i]
    }
  }
  #newTbl2 <- gsub("--", )
  newTbl2 <- read.table(text = newTbl2, stringsAsFactors = FALSE)
  newTbl2[, c(4, 5, 6)] <- apply(newTbl2[, c(4, 5, 6)], 2, function(x) as.numeric(x))
  names(newTbl2) <- c("AEP", "emaStation", "emaRegional", "variance", "lwr", "upr")
  topOut3 <- "    YEAR   DISCHARGE   ESTIMATE      LOW      HIGH"
  newTbl3 <- newText[-c(1:grep(pattern = topOut3, newText))]
  newTbl3 <- newTbl3[c(1:which(newTbl3 == ""))]
  newTbl3 <- newTbl3[-c(length(newTbl3))]
  if (nchar(newTbl3[length(newTbl3)]) == 1) {
    newTbl3 <- newTbl3[-c(length(newTbl3))]
  }
  newTbl3F <- data.frame(read.table(text = newTbl3[1]), stringsAsFactors = FALSE)
  if (ncol(newTbl3F) == 3) { 
    names(newTbl3F) <- c("watYr", "pkFlow", "hspp")
  } else {
    names(newTbl3F) <- c("watYr", "pkFlow", "hspp", "lowerLim", "upperLim")
  }
  newTbl3F$comment <- "Peak Used"
  for (i in 2:length(newTbl3)) {
    newTbl3_ <- data.frame(read.table(text = newTbl3[i])) 
    if(grepl("[*]", newTbl3_[1, 1])) {
      newTbl3_ <- newTbl3_[, -1]
      if (ncol(newTbl3_) == 3) { 
        names(newTbl3_) <- c("watYr", "pkFlow", "hspp")
      } else {
        names(newTbl3_) <- c("watYr", "pkFlow", "hspp", "lowerLim", "upperLim")
      }
      newTbl3_$comment <- "PILF"
    } else {
      if (ncol(newTbl3_) == 3) { 
        names(newTbl3_) <- c("watYr", "pkFlow", "hspp")
      } else {
        names(newTbl3_) <- c("watYr", "pkFlow", "hspp", "lowerLim", "upperLim")
      }
      newTbl3_$comment <- "Peak Used"
    }
    newTbl3F <- dplyr::bind_rows(newTbl3F, newTbl3_)
  }
  #names(newTbl3F) <- c("watYr", "pkFlow", "hspp", "comment")
  prtList <- list()
  prtList$info <- newTbl
  prtList$parms <- newTbl1
  prtList$estimates <- newTbl2
  prtList$plotPositions <- newTbl3F
  return(prtList)
}
