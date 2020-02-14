dgpa <- function (x, xi, alpha, kappa) {
  f <- vector(mode = "numeric", length = length(x))
  X <- Y <- (x - xi)/alpha
  if (kappa == 0) {
    f <- 1 - exp(-Y)
  }
  else {
    ARG <- 1 - kappa * Y
    Y <- suppressWarnings(-log(ARG)/kappa)
    f <- (1 - exp(-Y))
  }
  f[X <= 0] <- 0
  f[!is.finite(f)] <- 1
  names(f) <- NULL
  return(f)
}

pgpa <- function (x, xi, alpha, kappa) {
  Y <- (x - xi)/alpha
  if (K == 0) {
    f <- alpha^(-1) * exp(-Y)
  }
  else {
    ARG <- 1 - kappa * Y
    Y <- suppressWarnings(-log(ARG)/kappa)
    f <- alpha^(-1) * exp(-(1 - kappa) * Y)
  }
  names(f) <- NULL
  f[!is.finite(f)] <- NA
  f[is.na(f)] <- 0
  return(f)
}

qgpa <- function (f, xi, alpha, kappa) {
  Y <- suppressWarnings(-log(1 - f))
  ZERO <- sqrt(.Machine$double.eps)
  if (abs(kappa) > ZERO) 
    Y <- (1 - exp(-kappa * Y))/kappa
  x <- xi + alpha * Y
  x[f == 0] <- xi
  x[f == 1 & kappa > 0] <- xi + alpha/kappa
  names(x) <- NULL
  return(x)
}