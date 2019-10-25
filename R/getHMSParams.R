getHMSParams <- function(basinFile, type) {
  endRf <- "End:"
  typeRf <- paste0(type, ": ")
  findSbBsn <- stringr::str_detect(basinFile, sbBsnRf)
  findEndRf <- stringr::str_detect(basinFile, endRf)
  bsnFlSbBsn <- which(findSbBsn == TRUE)
  bsnFlEndRf <- which(findEndRf == TRUE)
  refDF <- data.frame()
  for (i in 1:length(bsnFlSbBsn)) {
    bsnFlSbBsnTst <- bsnFlSbBsn[i] 
    bsnFlEndRfSub <- bsnFlEndRf[which(bsnFlEndRf > bsnFlSbBsnTst)]
    bsnFlEndRfSub <- bsnFlEndRfSub[1]
    refDFSub <- basinFile[bsnFlSbBsnTst:bsnFlEndRfSub]
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