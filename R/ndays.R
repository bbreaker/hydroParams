ndays <- function(dates) {
  library(dplyr, quietly = TRUE)
  testDF <- data.frame(dates)
  testType <- class(testDF$dates)
  if (any(testType == "POSIXt")) {
    testDFnDays <- length(unique(as.Date(testDF$dates, format = "%Y-%m-%d $H:%M:%S")))
  } else if (any(testType == "Date")) {
    testDFnDays <- length(unique(testDF$dates))
  } else {
    stop("Please use objects of class 'Date' or 'POSIXt'")
  }
  return(testDFnDays)
}
