#' Expected Frequency Curve (Log-Pearson Type III)
#'
#' @param meanlog Mean of log10 flows
#' @param sdlog   Standard deviation of log10 flows
#' @param skew    Skew (moment skewness) of log10 flows
#' @param p_exceed Vector of exceedance probabilities
#' @param log_base Base of logarithm (default 10)
#' @param Sigma   Optional covariance matrix of (meanlog, sdlog, skew)
#'                If supplied, function simulates parameter uncertainty
#'                to compute the expected frequency curve.
#' @param nsim    Number of simulations if Sigma is provided
#'
#' @return Data frame with exceedance probabilities and expected quantiles
lp3ExpectedCurve <- function(meanlog, sdlog, skew, 
                             p_exceed = c(0.5,0.2,0.1,0.04,0.02,0.01), 
                             log_base = 10, Sigma = NULL, nsim = 5000) {
  if (sdlog <= 0) stop("sdlog must be > 0")
  
  # Internal LP3 quantile fn
  lp3_quantile <- function(p_nonexceed, mu, sigma, G) {
    if (abs(G) < 1e-6) {
      z <- qnorm(p_nonexceed)
    } else {
      alpha <- (2/abs(G))^2
      if (G > 0) {
        w <- qgamma(p_nonexceed, shape = alpha, scale = 1)
        z <- (w - alpha) / sqrt(alpha)
      } else {
        w <- qgamma(1 - p_nonexceed, shape = alpha, scale = 1)
        z <- - (w - alpha) / sqrt(alpha)
      }
    }
    mu + sigma * z
  }
  
  p_nonexceed <- 1 - p_exceed
  
  if (is.null(Sigma)) {
    # No parameter uncertainty: median frequency curve
    y <- sapply(p_nonexceed, lp3_quantile, mu = meanlog, sigma = sdlog, G = skew)
    q <- log_base ^ y
    return(data.frame(p_exceed = p_exceed, q_expected = q, type = "median"))
  } else {
    # Simulate parameter sets from MVN
    if (!requireNamespace("MASS", quietly = TRUE))
      stop("Package MASS required for multivariate normal sampling")
    sims <- MASS::mvrnorm(nsim, mu = c(meanlog, sdlog, skew), Sigma = Sigma)
    
    qmat <- sapply(p_nonexceed, function(pn) {
      apply(sims, 1, function(pr) {
        yq <- lp3_quantile(pn, pr[1], pr[2], pr[3])
        log_base ^ yq
      })
    })
    
    qmean <- colMeans(qmat)
    return(data.frame(p_exceed = p_exceed, q_expected = qmean, type = "expected"))
  }
}
