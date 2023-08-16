readPeakFQUSACEOut <- function(outFile) {
  newText <- readLines(outFile)
  ## get data for inputs
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
  ## get fitted moments - mu, sigma, and gamma
  topOut1 <- "Fitted Log10 Moments"
  newTbl1 <- newText[-c(1:grep(pattern = topOut1, newText))]
  newTbl1 <- newTbl1[2:4]
  newTbl1[1] <- gsub("EMA, At-Site Data, w/o Reg. Info", "STATION", newTbl1[1])
  newTbl1[2] <- gsub("EMA w/ Reg. Info & B17B MSE", "GENERAL_B17B", newTbl1[2])
  newTbl1[3] <- gsub("EMA w/Reg. Info & Spec. MSE", "GENERAL_EMA", newTbl1[3])
  newTbl1 <- read.table(text = newTbl1, stringsAsFactors = FALSE)
  names(newTbl1) <- c("type", "mu", "sigma", "gamma")
  ## get MSE and skew info
  topOut1_2 <- "Estimates & MSEs Corr. to Skew"
  newTbl1_2 <- newText[-c(1:grep(pattern = topOut1_2, newText))]
  newTbl1_2 <- newTbl1_2[1:9]
  col2Name <- gsub("Option", "", newTbl1_2[1])
  newTbl1_2[2] <- gsub("EMA Est. of G_atsite", "gamma_Est_EMA", newTbl1_2[2])
  newTbl1_2[3] <- gsub("B17B Est. of MSE[G_atsite]", "MSE_Est_B17B", newTbl1_2[3], fixed = TRUE)
  stringr::str_sub(newTbl1_2[4], start = 16, end = 48) <- "EMA_Adje_Est_MSE"
  newTbl1_2[4] <- gsub("EMA ADJE Est. of MSEG_atsite", "EMA_Adje_Est_MSE", newTbl1_2[4])
  newTbl1_2[5] <- gsub("Regional G (G_reg)", "regional_gamma", newTbl1_2[5], fixed = TRUE)
  newTbl1_2[6] <- gsub("MSE[G_reg]", "MSE_regional_gamma", newTbl1_2[6], fixed = TRUE)
  newTbl1_2[7] <- gsub("Weighted G", "weighted_gamma", newTbl1_2[7])
  newTbl1_2[8] <- gsub("MSE[G_atsite_systematic]", "MSE_atSite_Systematic", newTbl1_2[8], fixed = TRUE)
  newTbl1_2[9] <- gsub("ERL[G_atsite]", "erl_gamma_atSite", newTbl1_2[9], fixed = TRUE)
  newTbl1_2 <- read.table(text = newTbl1_2[2:9], stringsAsFactors = FALSE)
  names(newTbl1_2) <- c("estimate_type", col2Name)
  ## get frequency estimates
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
    } else if (ncol(read.table(text = testTbl2)) == 6) {
      testTbl2 <- paste(stringr::str_sub(testTbl2, 1, 67), stringr::str_sub(testTbl2, 68, 80))
      newTbl2[i] <- testTbl2
    } else {
      newTbl2[i] <- newTbl2[i]
    }
  }
  newTbl2 <- read.table(text = newTbl2, stringsAsFactors = FALSE)
  newTbl2 <- newTbl2[, -1] %>% 
    dplyr::rename(AEP = 1, emaRegional = 2, variance = 3, emaStation = 4, lwr = 5, upr = 6) %>% 
    dplyr::mutate(expected = signif((upr + lwr) / 2, 3))
  ## get EMA estimates of data
  topOut3 <- "Year Plot Pos     Obs. Q      Fit Value Q      % Diff"
  newTbl3 <- newText[-c(1:grep(pattern = topOut3, newText))]
  newTbl3 <- newTbl3[-1]
  newTbl3 <- read.table(text = newTbl3, stringsAsFactors = FALSE)
  names(newTbl3) <- c("watYr", "hspp", "obsPkFlow", "fitPkFlow", "percDiff")
  newTbl3$comment <- "Peak Used"
  ## get outlier plotting positions
  topOut4 <- "Number of Low Outliers"
  PILFs <- newText[grep(pattern = topOut4, newText)]
  spltPILFs <- length(unlist(stringr::str_split(PILFs, " ")))
  nPILFs <- as.numeric(unlist(stringr::str_split(PILFs, " "))[spltPILFs])
  if (length(nPILFs) == 0) {
    newTbl3 <- newTbl3
  } else if (nPILFs > 0) {
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
  } else {
    newTbl3 <- newTbl3
  }
  newList <- list()
  newList$info <- newTbl
  newList$parms <- newTbl1
  newList$mse <- newTbl1_2
  newList$estimates <- newTbl2
  newList$plotPositions <- newTbl3
  return(newList)
}
