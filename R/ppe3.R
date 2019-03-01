ppe3 <- function(x, shape, scale, location) {
  MU <- location
  SIGMA <- scale
  GAMMA <- shape
  SMALL <- sqrt(.Machine$double.eps)
  if (abs(GAMMA) <= SMALL) 
    return(dnorm(x, mean = MU, sd = SIGMA))
  ALPHA <- 4/GAMMA^2
  BETA <- (1/2) * SIGMA * abs(GAMMA)
  XI <- MU - 2 * SIGMA/GAMMA
  Y <- sign(GAMMA) * (x - XI)
  f <- dgamma(Y/BETA, ALPHA)/BETA
  names(f) <- NULL
  f[!is.finite(f)] <- NA
  f[is.na(f)] <- 0
  return(f)
}
