resRoute <- function (inflow, geometry, initStor, initDisch = NA, sim, unitMmt = "si") {
  
  storElevCurve <- geometry$storElevCurve
  
  if (unitMmt == "si") {
    storElevCurve$s <- storElevCurve$s * 1233.48
  } else if (unitMmt == "metric") {
    storElevCurve$s <- storElevCurve$s
  }
  
  dischElevCurve <- geometry$dischElevCurve
  
  if (unitMmt == "si") {
    dischElevCurve$q <- dischElevCurve$q * 0.0283168
  } else if (unitMmt == "metric") {
    dischElevCurve$q <- dischElevCurve$q
  }
  
  if (unitMmt == "si") {
    capacity <- geometry$capacity * 1233.48
  } else if (unitMmt == "metric") {
    capacity <- geometry$capacity
  }
  
  inflow[which(inflow < 0)] <- 0
  
  if (unitMmt == "si") {
    inflow <- inflow * 0.0283168
  } else if (unitMmt == "metric") {
    inflow <- inflow
  }
  
  start <- as.numeric(strsplit(sim$start, "-")[[1]])
  
  end <- as.numeric(strsplit(sim$end, "-")[[1]])
  
  start <- ISOdate(start[1], start[2], start[3])
  
  end <- ISOdate(end[1], end[2], end[3])
  
  sim$simSteps <- seq(start, end, sim$by)
  
  if (is.na(initDisch)) {
    initDisch <- 0
  } else {
    if (unitMmt == "si") {
      initDisch <- initDisch * 0.0283168
    } else if (unitMmt == "metric") {
      initDisch <- initDisch
    }
  }
  
  if (is.na(initStor)) {
    initStor <- capacity
  }
    
  if (initStor < capacity) {
    id <- which(((cumsum(inflow * sim$by/1000000) + 
                    initStor) > capacity) == TRUE)[1]
    if (is.na(id)) {
      mat <- as.data.frame(matrix(0, length(sim$simSteps), 5))
      colnames(mat) <- c("I", "Im", "O", "Im-O", "G")
      rownames(mat) <- sim$simSteps
      #return(mat)
    } else {
      inflow[id] <- sum(inflow[1:id] * sim$by/1000000) - 
        (capacity - initStor)
      inflow[1:(id - 1)] <- 0
    }
  }
  
  SH <- function(H) {
    return(approxExtrap(x = storElevCurve[, 2], 
                        y = storElevCurve[, 1], xout = H)$y - capacity)
  }
  
  OH <- function(H) {
    return(approxExtrap(x = dischElevCurve[, 2], 
                        y = dischElevCurve[, 1], xout = H)$y)
  }
  
  G <- function(H) {
    SH(H) * 1000000/sim$by + OH(H)/2
  }
  
  ma <- function(x, n = 2) {
    stats::filter(x, rep(1/n, n), sides = 2)
  }
  
  g <- c(0, sapply(seq(min(dischElevCurve[, 2]), 
                       max(dischElevCurve[, 2]), 
                       length.out = 100), G))
  
  o <- c(0, sapply(seq(min(dischElevCurve[, 2]), 
                       max(dischElevCurve[, 2]), 
                       length.out = 100), OH))
  
  OG <- function(G) {
    approxExtrap(x = g, y = o, xout = G)$y
  }
  
  mat <- as.data.frame(matrix(0, length(sim$simSteps), 5))
  
  colnames(mat) <- c("I", "Im", "O", "Im-O", "G")
  
  rownames(mat) <- sim$simSteps
  
  mat[1:length(inflow), 1] <- inflow
  
  mat[1:(length(inflow)), 2] <- c(NA, ma(inflow, 2)[1:(length(inflow) - 1)])
  
  mat[1, 3] <- initDisch
  
  mat[2, 4] <- mat[2, 2] - mat[1, 3]
  
  mat[1, 5] <- 0
  
  mat[2, 5] <- mat[1, 5] + mat[2, 4]
  
  for (t in 2:(nrow(mat) - 1)) {
    mat[t, 3] <- OG(mat[t, 5])
    mat[t + 1, 4] <- mat[t + 1, 2] - mat[t, 3]
    mat[t + 1, 5] <- mat[t, 5] + mat[t + 1, 4]
  }
  
  mat[t + 1, 3] <- OG(mat[t + 1, 5])
  
  mat[which(mat[, 3] < 0), 3] <- 0
  
  elev <- as.numeric()
  
  cap <- as.numeric()
  
  for (j in 1:nrow(mat)) {
    subDischElev <- dischElevCurve %>% 
      dplyr::mutate(diff = abs(mat[j, 3] - q)) %>% 
      dplyr::filter(diff == min(diff)) %>% 
      dplyr::arrange(desc(h)) %>% 
      dplyr::slice(1) %>% 
      data.frame()
    subStorElev <- storElevCurve %>% 
      dplyr::mutate(diff = abs(subDischElev$h - h)) %>% 
      dplyr::filter(diff == min(diff)) %>% 
      dplyr::arrange(desc(h)) %>% 
      dplyr::slice(1) %>% 
      data.frame()
    elev <- c(elev, subStorElev$h)
    cap <- c(cap, subStorElev$s)
  }
  
  if (unitMmt == "si") {
    mat <- mat * 35.3147
  } else if (unitMmt == "metric") {
    mat <- mat
  }
  
  mat$ts <- sim$simSteps
  
  if (unitMmt == "si") {
    cap <- cap * (1 / 1233.48)
  } else if (unitMmt == "metric") {
    cap <- cap
  }
  
  mat$C <- cap
  
  mat$h <- elev
  
  #mat$C <- mat$O
  
  return(mat)
  
}