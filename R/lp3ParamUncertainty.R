# Approximate parameter uncertainty from LP3 parameters + effective record length
# Based on Bulletin 17B/17C approximations
# Inputs: meanlog, sdlog, skew (of log10 flows), Neff = effective record length
# Output: list with Sigma covariance matrix
lp3ParamUncertainty <- function(meanlog, sdlog, skew, Neff) {
  if (Neff <= 1) stop("Effective record length must be > 1.")
  if (sdlog <= 0) stop("sdlog must be > 0.")
  
  # Variance of mean (classic)
  var_mean <- sdlog^2 / Neff
  
  # Variance of sd (approx)
  var_sd <- (sdlog^2 / (2 * Neff))  # delta-method on variance ~ chi-square
  
  # Variance of skew (Bulletin 17B formula)
  var_skew <- 6 / Neff
  
  # Approx covariances (often assumed 0 in Bulletin 17 practice)
  cov_ms <- 0
  cov_mg <- 0
  cov_sg <- 0
  
  Sigma <- matrix(c(var_mean, cov_ms,  cov_mg,
                    cov_ms,  var_sd,  cov_sg,
                    cov_mg,  cov_sg,  var_skew),
                  nrow = 3, byrow = TRUE)
  colnames(Sigma) <- rownames(Sigma) <- c("meanlog","sdlog","skew")
  
  list(
    meanlog = meanlog,
    sdlog   = sdlog,
    skew    = skew,
    Neff    = Neff,
    Sigma   = Sigma
  )
}
