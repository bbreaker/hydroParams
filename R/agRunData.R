agRunData <- function(dssFile, makeList = FALSE) {
  
  paths <- getAllPaths(dssFile)
  
  theTSC <- data.frame(getFullDT(dssFile, paths))
  
  allPaths <- data.frame(feature = as.character(), val = as.character(), 
                         date = as.character(), timeStep = as.character(), 
                         run = as.character())
  
  for(i in 1:length(paths)) { 
    newMat <- stringr::str_split_fixed(paths[i], "/", n = 8) 
    newVec <- data.frame(t(newMat[1, which(nchar(newMat) > 0)])) 
    names(newVec) <- names(allPaths) 
    allPaths <- dplyr::bind_rows(allPaths, newVec) 
  }
  
  numInts <- as.numeric((difftime(max(theTSC$datetime), min(theTSC$datetime), 
                                  units = "mins") / 15) + 1)
  
  allPaths$qualDF <- paste(allPaths$feature, allPaths$val, sep = ":")
  
  qual <- unique(allPaths$qualDF)
  
  partVec <- c()
  
  paramVec <- c()
  
  for(j in seq(1, length(qual), 1)) { 
    qualVal <- qual[j] 
    allPathsSub <- dplyr::filter(allPaths, qualDF == qualVal) 
    if(nrow(allPathsSub) > 1) { 
      partVec2 <- rep(allPathsSub[1, 1], numInts) 
      paramVec2 <- rep(allPathsSub[1, 2], numInts)
    } else { 
      partVec2 <- rep(allPathsSub[1, 1], numInts - 1)
      paramVec2 <- rep(allPathsSub[1, 2], numInts - 1)
    }
    partVec <- c(partVec, partVec2) 
    paramVec <- c(paramVec, paramVec2)
  }
  
  theTSC$feature <- partVec
  
  theTSC$param <- paramVec
  
  theTSC$run <- unique(allPaths$run)
  
  if(makeList == TRUE) {
    theTSC$listVal <- paste(theTSC$feature, theTSC$param, sep = ":")
    listQual <- unique(theTSC$listVal)
    newList <- list()
    for(k in seq(1, length(listQual), 1)) {
      forListQual <- listQual[k]
      subTSC <- dplyr::filter(theTSC, listVal == forListQual)
      newList[[k]] <- subTSC
    }
    theTSC <- newList
    names(theTSC) <- listQual
  }
  
  return(theTSC)
  
}
