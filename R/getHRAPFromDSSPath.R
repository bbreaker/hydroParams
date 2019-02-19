getHRAPFromDSSPath <- function(dssPath, convProj = TRUE, newCRS) {
  
  data <- dssOpen$get(dssPath)
  
  dat <- data$gridData
  
  gridDat <- dat$getData()
  
  spatDat <- dat$getGridInfo()
  
  llCellY <- spatDat$getLowerLeftCellY()
  
  nCellY <- spatDat$getNumberOfCellsY()
  
  minCellY <- llCellY * 4762.5 - 1601.0 * 4762.5
  
  maxCellY <- (llCellY + nCellY)  * 4762.5 - 1601.0 * 4762.5
  
  llCellX <- spatDat$getLowerLeftCellX()
  
  nCellX <- spatDat$getNumberOfCellsX()
  
  minCellX <- llCellX * 4762.5 - 401.0 * 4762.5
  
  maxCellX <- (llCellX + nCellX) * 4762.5 - 401.0 * 4762.5
  
  cellSize <- spatDat$getCellSize()
  
  newRas <- raster(nrows = nCellY, ncols = nCellX, xmn = minCellX, ymn = minCellY, xmx = maxCellX, ymx = maxCellY)
  
  gridDatMat <- matrix(nrow = nCellY, ncol = nCellX)
  
  gridVec <- seq(length(gridDat), 0, -nCellX)
  
  for (i in 1:(length(gridVec) - 1)) { 
    
    gridDatMat[i, ] <- gridDat[(gridVec[i + 1] + 1):(gridVec[i])]
    
  }
  
  values(newRas) <- gridDatMat
  
  projection(newRas) <- '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
  
  if (convProj == TRUE) { 
    
    projRas <- projectRaster(newRas, crs = CRS(newCRS))
    
    newRas <- projRas
    
  }
  
  return(newRas)
  
}
