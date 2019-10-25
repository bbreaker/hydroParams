getHMSParams <- function(basinFile, type) {
  endRf <- "End:"
  typeRf <- paste0(type, ": ")
  findType <- stringr::str_detect(basinFile, typeRf)
  findEndRf <- stringr::str_detect(basinFile, endRf)
  bsnFlType <- which(findType == TRUE)
  bsnFlEndRf <- which(findEndRf == TRUE)
  refDF <- data.frame()
  for (i in 1:length(bsnFlType)) {
    bsnFlTypeTst <- bsnFlType[i] 
    bsnFlEndRfSub <- bsnFlEndRf[which(bsnFlEndRf > bsnFlTypeTst)]
    bsnFlEndRfSub <- bsnFlEndRfSub[1]
    refDFSub <- basinFile[bsnFlTypeTst:bsnFlEndRfSub]
    refDFWide <- data.frame(fileN = 1)
    for (j in 1:length(refDFSub)) {
      refDFWide_ <- data.frame(val = (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[2]), 
                               stringsAsFactors = FALSE)
      names(refDFWide_) <- (unlist(stringr::str_split(refDFSub[j], pattern = ": "))[1])
      refDFWide <- dplyr::bind_cols(refDFWide, refDFWide_)
    }
    refDFWide <- refDFWide[, -1]
    refDFWide <- refDFWide[, colSums(is.na(refDFWide)) < nrow(refDFWide)]
    refDF <- dplyr::bind_rows(refDF, refDFWide)
  }
  return(refDF)
}
