agRunData2 <- function(dssFile) {
  
  paths <- getAllPaths(dssFile)
  
  allPaths <- data.frame(system = as.character(), feature = as.character(), 
                         param = as.character(), date = as.character(), 
                         timeStep = as.character(), source = as.character(),
                         fullPath = as.character(), stringsAsFactors = FALSE)
  
  for(i in 1:length(paths)) { 
    newMat <- stringr::str_split_fixed(paths[i], "/", n = 8) 
    newVec <- data.frame(t(newMat[1, which(nchar(newMat) > 0)])) 
    newVec[7] <- paths[i]
    names(newVec) <- names(allPaths) 
    allPaths <- dplyr::bind_rows(allPaths, newVec) 
  }
  
  allPaths$listVal <- paste(allPaths$feature, allPaths$param, 
                            allPaths$source, sep = ":")
  
  allPathsTest <- data.frame(listVal = unique(allPaths$listVal), 
                             ref = seq(1, length(unique(allPaths$listVal)), 1),
                             stringsAsFactors = FALSE)
  
  allPaths <- dplyr::left_join(allPaths, allPathsTest, "listVal")
  
  newList <- list()
  
  for (i in seq(1, length(unique(allPaths$listVal)), 1)) {
    allPathsSub <- dplyr::filter(allPaths, ref == i)
    if (nrow(allPathsSub) == 1) {
      newDf <- data.frame(getFullDT(dssFile, allPathsSub[1, 7]))
    } else if (nrow(allPathsSub) > 1) {
      newDf <- data.frame(getFullDT(dssFile, allPathsSub[1, 7]))
      for (j in seq(2, nrow(allPathsSub), 1)) {
        newerDf <- data.frame(getFullDT(dssFile, allPathsSub[j, 7]))
        newDf <- dplyr::bind_rows(newDf, newerDf)
      }
    } else {
      newDf <- data.frame(datetime = NA, value = NA, units = NA,
                          stringsAsFactors = FALSE)
    }
    newList[[i]] <- newDf
  }
  
  names(newList) <- allPathsTest$listVal
  
  return(newList)
  
}
