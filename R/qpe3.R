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
