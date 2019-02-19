getSHGFromDSSPath <- function(dssPath, dssOpen, convProj = FALSE, newCRS) {
  
  data <- dssOpen$get(dssPath)
  
  dat <- data$gridData
  
  gridDat <- dat$getData()
  
  spatDat <- dat$getGridInfo()
  
  cellSize <- spatDat$getCellSize()
  
  llCellY <- spatDat$getLowerLeftCellY()
  
  nCellY <- spatDat$getNumberOfCellsY()
  
  minCellY <- llCellY * cellSize
  
  maxCellY <- (llCellY + nCellY) * cellSize
  
  llCellX <- spatDat$getLowerLeftCellX()
  
  nCellX <- spatDat$getNumberOfCellsX()
  
  minCellX <- llCellX * cellSize
  
  maxCellX <- (llCellX + nCellX) * cellSize
  
  newRas <- raster(nrows = nCellY, ncols = nCellX, xmn = minCellX, ymn = minCellY, xmx = maxCellX, ymx = maxCellY)
  
  gridDatMat <- matrix(nrow = nCellY, ncol = nCellX)
  
  gridVec <- seq(length(gridDat), 0, -nCellX)
  
  for (i in 1:(length(gridVec) - 1)) { 
    
    gridDatMat[i, ] <- gridDat[(gridVec[i + 1] + 1):(gridVec[i])]
    
  }
  
  values(newRas) <- gridDatMat
  
  projection(newRas) <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
  
  if (convProj == TRUE) { 
    
    projRas <- projectRaster(newRas, crs = CRS(newCRS))
    
    newRas <- projRas
    
  }
  
  return(newRas)
  
}
