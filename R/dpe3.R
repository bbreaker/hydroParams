dpe3 <- function(x, shape, scale, location) {
  SMALL <- sqrt(.Machine$double.eps)
  MU <- location
  SIGMA <- scale
  GAMMA <- shape
  if (abs(GAMMA) <= SMALL) 
    return(pnorm((x - MU)/SIGMA))
  ALPHA <- 4/GAMMA^2
  BETA <- 0.5 * SIGMA * abs(GAMMA)
  XI <- MU - 2 * SIGMA/GAMMA
  if (GAMMA > 0) 
    return(pgamma((x - XI)/BETA, ALPHA))
  return(1 - pgamma((XI - x)/BETA, ALPHA))
}
