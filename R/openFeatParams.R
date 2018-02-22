openFeatParams <- function(basinFile) {
  featFile <- readLines(basinFile)
  types <- c("Basin", "Subbasin", "Reservoir", "Sink", 
             "Junction", "Reach", "Schematic")
  featList <- rep(list(list()), length(types))
  names(featList) <- types
  endCount <- sum(featFile == "End:")
  endPos <- which(featFile == "End:")
  for(i in seq(1, length(endCount), 1)) {
    if (i == 1) {
      featSub <- featFile[1:16]
    } else {
      featSub <- featFile[(endPos[i - 1] + 2):(endPos[i])]
    }
    featClass <- unlist(stringr::str_split(featSub[1], ": "))[1]
    featName <- unlist(stringr::str_split(featSub[1], ": "))[2]
    if (featClass == "Basin") {
      featList$Basin <- table(featSub)
    } else if (featClass == "Subbasin") {
      featNames <- unlist(stringr::str_split(featSub, ": "))
    } else if (featClass == "Reservoir") {
      
    } else if (featClass == "Sink") {
      
    } else if (featClass == "Junction") {
      
    } else if (featClass == "Reach") {
      
    } else if (featClass == "Schematic") {
      
    }
  }
}