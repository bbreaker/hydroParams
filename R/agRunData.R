agRunData <- function(dssFile, pathKeep = NULL, pathDrop = NULL, makeList = FALSE) {
  
  paths <- getAllPaths(dssFile)
  
  if(is.null(pathKeep) & is.null(pathDrop)) {
    paths <- paths
  } else if (is.null(pathDrop) & !is.null(pathKeep)) {
    pathFilter <- paste(pathKeep, collapse = "|")
    paths <- paths[grepl(pathFilter, paths)]
  } else if (!is.null(pathDrop) & is.null(pathKeep)) {
    pathFilter <- paste(pathDrop, collapse = "|")
    paths <- paths[!grepl(pathFilter, paths)]
  } else {
    pathFilter <- paste(pathKeep, collapse = "|")
    paths <- paths[grepl(pathFilter, paths)]
    pathFilter <- paste(pathDrop, collapse = "|")
    paths <- paths[!grepl(pathFilter, paths)]
  }
  
  pathsDF <- data.frame(unlist(stringr::str_split_fixed(paths, "/", n = 8))) %>% 
    dplyr::group_by(X3, X4, X6, X7) %>% 
    dplyr::slice(1) %>% 
    data.frame()
  
  paths <- c()
  
  for (i in 1:nrow(pathsDF)) {
    paths_ <- pathsDF[i, ]
    paths_ <- paste(paths_, collapse = '/')
    paths <- c(paths, paths_)
    rm(paths_)
  }
  
  paths <- paths[!grepl("FLOW-UNIT GRAPH", paths)]
  
  theTSC <- data.frame()
  
  for (i in 1:length(paths)) {
    theTSC_ <- getTimeSeriesAsDataFrame(dssFile, paths[i])
    thePathSub <- data.frame(unlist(stringr::str_split_fixed(paths[i], "/", n = 8)))
    theTSC_ <- dplyr::bind_cols(theTSC_, thePathSub)
    theTSC <- dplyr::bind_rows(theTSC, theTSC_)
  }
  
  theTSC <- theTSC %>% 
    dplyr::select(X3, value, index, X6, X7, X4)
  
  names(theTSC) <- c("feature", "val", "date", "timeStep", "run", "param")
  
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
