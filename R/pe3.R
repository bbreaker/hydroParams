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

qpe3 <- function(f, shape, scale, location) { 
  SMALL <- sqrt(.Machine$double.eps)
  U <- location
  A <- scale
  G <- shape
  x <- vector(mode = "numeric", length = length(f))
  if (abs(G) <= SMALL) {
    x <- U + A * qnorm(f)
  }
  else {
    ALPHA <- 4/G^2
    BETA <- abs(0.5 * A * G)
    if (G > 0) {
      x <- U - ALPHA * BETA + qgamma(f, ALPHA, scale = BETA)
    }
    else {
      x <- U + ALPHA * BETA - qgamma(1 - f, ALPHA, scale = BETA)
    }
  }
  x[f == 0 & G > 0] <- U - 2 * A/G
  x[f == 1 & G < 0] <- U - 2 * A/G
  names(x) <- NULL
  return(x)
}