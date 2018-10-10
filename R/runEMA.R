runEMA <- function(pathFQ, date, flow, interval, beginYr = NULL, endYr = NULL, 
                   skewOpt = "STATION", genSkew = NULL, skewSE = NULL, killOut = TRUE) {
  
  pkFile <- data.frame(pkDates = date, pkFlows = round(flow, 0), 
                       pkCodes = rep(NA, length(flow)))
  
  hdr <- c(paste0("I pkInput.spc"),
           "0 something.out",
           "LOTHRESH         0.0",
           paste0("SKEWOPT          ", skewOpt),
           paste0("GENSKEW          ", genSkew),
           paste0("SKEWSD           ", skewSE),
           paste0("BEGYEAR          ", beginYr),
           paste0("ENDYEAR          ", as.character(endYr)),
           "GAGEBASE            0",
           paste0("THRESHOLD        ", as.character(beginYr), " ", 
                  as.character(endYr), " 0.000 1.d99"))
  
  pkVec <- c(paste0("Q    ", waterYear(pkFile$pkDates), sprintf("%7d", pkFile$pkFlows), 
                    sprintf("%4d", pkFile$pkCodes)))
  
  pkFileTxt <- c(hdr, pkVec)
  writeLines(pkFileTxt, paste0(pathFQ, "/pkInput.spc"))
  command <- paste0(pathFQ, "/PeakfqSA_USACE_win.exe")
  args <- paste0("pkInput.spc")
  system2(command, args)
  newText <- readLines(paste0(pathFQ, "/pkInput.out"))
  newText <- str_replace(newText, pattern = fixed("*************"), "          INF")
  newText <- str_replace(newText, pattern = fixed("     Infinity"), "          INF")
  if (killOut == TRUE) {
    file.remove(paste0(pathFQ, "/pkInput.out"))
  }
  topOut <- "                                  EMA FREQUENCY ESTIMATES"
  newTbl <- newText[-c(1:grep(pattern = topOut, newText))]
  newTbl <- newTbl[6:46]
  newTbl <- read.table(text = newTbl)
  names(newTbl) <- c("type", "AEP", "estEMA", "varLogEMA", "estAtSiteEMA", "CILow", "CIHigh")
  newTbl$NEP <- round(1 - newTbl$AEP, 8)
  topOut2 <- "            Year Plot Pos     Obs. Q      Fit Value Q      % Diff"
  newTbl2 <- newText[-c(1:grep(pattern = topOut2, newText))]
  newTbl2 <- newTbl2[2:length(newTbl2)]
  newTbl2 <- read.table(text = newTbl2)
  names(newTbl2) <- c("Year", "plotPos", "obsQ", "fitQ", "prcntDiff")
  newTbl2$NEP <- round(1 - newTbl2$plotPos, 8)
  topOut3 <- "         Fitted Log10 Moments                    M         S         G"
  newTbl3 <- newText[-c(1:grep(pattern = topOut3, newText))]
  newTbl3 <- newTbl3[2:length(newTbl3)]
  newTbl3 <- newTbl3[1:3]
  for (i in 1:length(newTbl3)) {
    newTbl3[i] <- stringr::str_sub(newTbl3[i], 45, nchar(newTbl3[i]))
  }
  newTbl3 <- read.table(text = newTbl3)
  names(newTbl3) <- c("mu", "sigma", "gamma")
  newTbl3$description <- c("atSite", "weighted", "specWeighted")
  allDat <- list(newTbl3, newTbl2, newTbl)
  return(allDat)
}