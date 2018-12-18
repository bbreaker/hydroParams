spreadSeq <- function(inputTimes, inputFlows, outputTimes, outPkTime, tStep = "15 mins") {
  
  inputDF <- data.frame(times = inputTimes, flows = inputFlows)
  
  maxInput <- max(inputDF$flows)
  
  beginInput <- inputDF[1, 2]
  
  endInput <- inputDF[nrow(inputDF), 2]
  
  maxPos <- which.max(inputDF$flows)
  
  afterMax <- inputDF[maxPos + 1, 2]
  
  timeVecA <- seq(from = outputTimes[1], 
                  to = outPkTime, 
                  by = tStep)
  
  timeVecB <- seq(from = outPkTime + lubridate::minutes(15), 
                  to = outputTimes[length(outputTimes)], 
                  by = tStep)
  
  flowVecA <- seq(from = beginInput, to = maxInput, 
                  length.out = length(timeVecA))
  
  flowVecB <- seq(from = afterMax, to = endInput, 
                  length.out = length(timeVecB))
  
  timeVec <- c(timeVecA, timeVecB)
  
  flowVec <- c(flowVecA, flowVecB)
  
  percDiffVec <- (sum(flowVec) - sum(inputDF$flows)) / sum(flowVec)
  
  flowVec <- flowVec - (flowVec * percDiffVec)
  
  newDF <- data.frame(dateTime = timeVec, flow = flowVec)
  
  return(newDF)
  
}
