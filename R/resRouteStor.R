resRouteStor <- function(inflow, geometry, initStor = NULL, initDisch = NULL, sim) {
  
  inflow[which(inflow < 0)] <- 0
  
  if (is.null(initDisch)) {
    initDisch <- inflow[1]
  }
  
  if (is.null(initStor)) {
    initStor <- geometry$capacity
  }
  
  geometry$resCurves <- geometry$resCurves %>% 
    dplyr::mutate(storCubicFt = storage * (1 / 43560)) %>% 
    dplyr::mutate(coeff1 = ((2 * storCubicFt) * sim$tStep) + discharge)
  
  retDF <- data.frame(date = sim$date, inflow = inflow) %>% 
    dplyr::mutate(inflow2 = inflow + dplyr::lag(inflow, 1), 
                  coeff1 = rep(NA, length(inflow)), 
                  outflow = c(initDisch, rep(NA, length(inflow) - 1)), 
                  coeff2 = rep(NA, length(inflow)), 
                  elev = rep(NA, length(inflow)))
  
  storVal1 <- geometry$resCurves %>% 
    dplyr::mutate(storDiff = abs(initStor - storage)) %>% 
    dplyr::arrange(storDiff) %>% 
    dplyr::slice(1)
  
  for (i in 1:nrow(retDF)) {
    
    testRow <- retDF[i, ]
    
    if (i < length(inflow)) {
      
      testCurve <- geometry$resCurves %>% 
        dplyr::mutate(outDiff = abs(discharge - testRow$outflow)) %>% 
        dplyr::arrange(outDiff) %>% 
        dplyr::slice(1) %>% 
        data.frame()
      
      retDF[i, 4] <- ifelse(i == 1, storVal1$coeff1, testCurve$coeff1)
      
      retDF[i, 6] <- retDF[i, 4] - (2 * retDF[i, 5])
      
      retDF[i, 7] <- testCurve$elevation
      
      retDF[i + 1, 4] <- retDF[i, 6] + retDF[i + 1, 3]
      
      testCurve2 <- geometry$resCurves %>% 
        dplyr::mutate(coeff1Diff = abs(coeff1 - retDF[i + 1, 4])) %>% 
        dplyr::arrange(coeff1Diff) %>% 
        dplyr::slice(1) %>% 
        data.frame()
      
      retDF[i + 1, 5] <- testCurve2$discharge
      
    } else {
      
      testCurve <- geometry$resCurves %>% 
        dplyr::mutate(outDiff = abs(discharge - testRow$outflow)) %>% 
        dplyr::arrange(outDiff) %>% 
        dplyr::slice(1) %>% 
        data.frame()
      
      retDF[i, 4] <- testCurve$coeff1
      
      retDF[i, 6] <- retDF[i, 4] - (2 * retDF[i, 5])
      
      retDF[i, 7] <- testCurve$elevation
      
    }
    
  }
  
  return(retDF)
  
}
