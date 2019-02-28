readPeakFQUSACEOut <- function(outFile) {
  newText <- readLines(outFile)
  nPks <- "Total No. of Observations" 
  nPks <- newText[grep(pattern = nPks, newText)]
  spltPks <- length(unlist(stringr::str_split(nPks, " ")))
  nPks <- as.numeric(unlist(stringr::str_split(nPks, " "))[spltPks])
  nPksUsed <- "Number of Peaks"
  nPksUsed <- newText[grep(pattern = nPksUsed, newText)]
  spltNPksUsed <- length(unlist(stringr::str_split(nPksUsed, " ")))
  nPksUsed <- as.numeric(unlist(stringr::str_split(nPksUsed, " "))[spltNPksUsed])
  skewOpt <- "Skew Option"
  skewOpt <- newText[grep(pattern = skewOpt, newText)]
  spltSkewOpt <- length(unlist(stringr::str_split(skewOpt, " "))) - 4
  skewOpt <- as.character(unlist(stringr::str_split(skewOpt, " "))[spltSkewOpt])
  newTbl <- data.frame(nPeaks = nPks, nPeaksUsed = nPksUsed, 
                       skewOption = skewOpt, stringsAsFactors = FALSE)
  topOut1 <- "Fitted Log10 Moments"
  newTbl1 <- newText[-c(1:grep(pattern = topOut1, newText))]
  newTbl1 <- newTbl1[2:4]
  newTbl1[1] <- gsub("EMA, At-Site Data, w/o Reg. Info", "STATION", newTbl1[1])
  newTbl1[2] <- gsub("EMA w/ Reg. Info & B17B MSE", "GENERAL_B17B", newTbl1[2])
  newTbl1[3] <- gsub("EMA w/Reg. Info & Spec. MSE", "GENERAL_EMA", newTbl1[3])
  newTbl1 <- read.table(text = newTbl1, stringsAsFactors = FALSE)
  names(newTbl1) <- c("type", "mu", "sigma", "gamma")
  topOut2 <- "EMA FREQUENCY ESTIMATES"
  newTbl2 <- newText[-c(1:grep(pattern = topOut2, newText))]
  newTbl2 <- newTbl2[6:46]
  for(i in 1:length(newTbl2)) {
    testTbl2 <- newTbl2[i]
    testVal <- grepl("[/*************]", testTbl2)
    if(testVal == TRUE) {
      testTbl2Sub1 <- stringr::str_sub(testTbl2, 1, 28)
      testTbl2Sub2 <- stringr::str_sub(testTbl2, 45, nchar(testTbl2))
      
      testTbl2Sub1 <- paste0(testTbl2Sub1, " NA ")
      newTbl2[i] <- paste0(testTbl2Sub1, testTbl2Sub2)
    } else {
      newTbl2[i] <- newTbl2[i]
    }
  }
  newTbl2 <- read.table(text = newTbl2, stringsAsFactors = FALSE)
  newTbl2 <- newTbl2[, -1]
  names(newTbl2) <- c("AEP", "emaRegional", "variance", "emaStation", "lwr", "upr")
  topOut3 <- "Year Plot Pos     Obs. Q      Fit Value Q      % Diff"
  newTbl3 <- newText[-c(1:grep(pattern = topOut3, newText))]
  newTbl3 <- newTbl3[-1]
  newTbl3 <- read.table(text = newTbl3, stringsAsFactors = FALSE)
  names(newTbl3) <- c("watYr", "hspp", "obsPkFlow", "fitPkFlow", "percDiff")
  newTbl3$comment <- "Peak Used"
  topOut4 <- "Number of Low Outliers"
  PILFs <- newText[grep(pattern = topOut4, newText)]
  spltPILFs <- length(unlist(stringr::str_split(PILFs, " ")))
  nPILFs <- as.numeric(unlist(stringr::str_split(PILFs, " "))[spltPILFs])
  if(nPILFs > 0) {
    PILFs <- newText[-c(1:grep(pattern = topOut4, newText))]
    PILFs <- PILFs[1:nPILFs]
    PILFs <- gsub("[/*]", "", PILFs)
    PILFs <- read.table(text = PILFs, stringsAsFactors = FALSE)
    PILFs[, 5] <- gsub("[/)]", "", PILFs[, 5])
    PILFs[, 6] <- gsub("[/p=]", "", PILFs[, 6])
    PILFs <- PILFs %>% 
      dplyr::select(V5, V6) %>% 
      dplyr::rename(obsPkFlow = V5, hspp = V6) %>% 
      dplyr::mutate(comment = "PILF") %>% 
      dplyr::mutate(obsPkFlow = as.numeric(obsPkFlow)) %>% 
      dplyr::mutate(hspp = as.numeric(hspp)) %>% 
      data.frame()
    newTbl3 <- dplyr::bind_rows(newTbl3, PILFs)
  }
  newList <- list()
  newList$info <- newTbl
  newList$parms <- newTbl1
  newList$estimates <- newTbl2
  newList$plotPositions <- newTbl3
  return(newList)
}
