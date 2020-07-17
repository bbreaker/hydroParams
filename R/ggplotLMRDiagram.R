ggplotLMRDiagram <- function() {
  
  if (("lmomco" %in% rownames(installed.packages())) != TRUE) {
    
    return(print("Please install the lmomco package"))
    
  }
  
  testDat <- lmomco::lmrdia()
  
  for(i in 1:length(testDat)) {
    testDat[[i]] <- data.frame(testDat[[i]])
    names(testDat[[i]]) <- c("x", "y")
  }
  
  testDat$kap <- data.frame(x = testDat$gpa$x, ymin = testDat$gpa$y, ymax = testDat$glo$y)
  
  lmrPlot <- ggplot() + 
    geom_ribbon(data = testDat$kap, aes(x = x, ymin = ymin, ymax = ymax), fill = "cornflowerblue", color = NA, alpha = 0.2) +
    geom_line(data = testDat$limits, aes(x = x, y = y), size = 2, color = "grey80") + 
    geom_line(data = testDat$aep4, aes(x = x, y = y), size = 1, linetype = "dotted", color = "red") + 
    geom_line(data = testDat$gev, aes(x = x, y = y), size = 1, linetype = "dashed", color = "red") + 
    geom_line(data = testDat$glo, aes(x = x, y = y), size = 1, color = "green") + 
    geom_line(data = testDat$gno, aes(x = x, y = y), size = 1, linetype = "dashed", color = "blue") + 
    geom_line(data = testDat$gov, aes(x = x, y = y), size = 1, linetype = "dashed", color = "purple") + 
    geom_line(data = testDat$gpa, aes(x = x, y = y), size = 1, color = "blue") + 
    geom_line(data = testDat$pe3, aes(x = x, y = y), size = 1, color = "purple") + 
    geom_point(data = testDat$exp, aes(x = x, y = y), shape = 16, color = "red", size = 4) + 
    geom_point(data = testDat$nor, aes(x = x, y = y), shape = 15, color = "red", size = 3) + 
    geom_point(data = testDat$gum, aes(x = x, y = y), shape = 17, color = "red", size = 3) + 
    geom_point(data = testDat$ray, aes(x = x, y = y), shape = 18, color = "red", size = 4) + 
    geom_point(data = testDat$uniform, aes(x = x, y = y), shape = 12, color = "red", size = 3) + 
    geom_point(data = testDat$cau, aes(x = x, y = y), shape = 13, color = "green", size = 3) + 
    geom_point(data = testDat$slash, aes(x = x, y = y), shape = 10, color = "green", size = 3) + 
    labs(x = "L-Skew", y = "L-Kurtosis")
  
  return(lmrPlot)
  
}
