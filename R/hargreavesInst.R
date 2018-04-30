hargreavesInst <- function (tmin, tmax, times, pre, lat) { 
  
  n <- length(tmin) 
  
  ET0 <- tmin * NA 
  
  T <- (tmin + tmax)/2 
  
  Tr <- tmax - tmin 
  
  Tr <- ifelse(Tr < 0, 0, Tr) 
  
  J <- lubridate::yday(times) + 1 
  
  delta <- 0.409 * sin(0.0172 * J - 1.39) 
  
  dr <- 1 + 0.033 * cos(0.0172 * J) 
  
  latr <- lat/57.2957795 
  
  sset <- -tan(latr) * tan(delta) 
  
  omegas <- sset * 0 
  
  omegas[sset >= {-1} & sset <= 1] <- acos(sset[sset >= {-1} & sset <= 1])
  
  omegas[sset < {-1}] <- max(omegas)
  
  Ra <- 37.6 * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
    
  Ra <- ifelse(Ra < 0, 0, Ra) 
  
  ab <- Tr - 0.0123 * pre
  
  ET0 <- 0.0013 * 0.408 * Ra * (T + 17.8) * ab^0.76 
  
  ET0[is.nan(ab^0.76)] <- 0
  
  ET0 <- ifelse(ET0 < 0, 0, ET0) 
  
  ET0 <- ET0 * Hmisc::monthDays(times)
  
  return(ET0) 
  
}
