# Compute LP3 parameter uncertainty (Sigma) from data via bootstrap
# - x: positive flows
# - method: "nonparametric" (resample observed data) or "parametric" (simulate from fitted LP3)
# - B: number of bootstrap replicates
# - Returns: list(meanlog, sdlog, skew, Sigma, draws), where draws is a Bx3 matrix of bootstrap params
lp3ParamUncertainty <- function(x, log_base = 10, method = c("nonparametric", "parametric"), 
                                B = 5000, seed = NULL) {
  method <- match.arg(method)
  if (any(!is.finite(x)) || any(x <= 0)) stop("All flows x must be positive and finite.")
  if (!is.null(seed)) set.seed(seed)
  
  # log10 (or other base) transform
  y <- log(x, base = log_base)
  n <- length(y)
  
  # helpers
  skewness <- function(v) {
    m <- mean(v); s <- sd(v)
    if (s == 0) return(0)
    mean((v - m)^3) / (s^3)
  }
  
  # point estimates from sample logs
  mu_hat <- mean(y)
  sd_hat <- sd(y)
  G_hat  <- skewness(y)
  
  # generator for LP3 in log-space (returns a vector on log scale)
  rlp3_log <- function(n, mu, sigma, G) {
    if (abs(G) < 1e-8) {
      # approx log-normal when skew ~ 0
      z <- rnorm(n)
    } else {
      alpha <- (2 / abs(G))^2
      if (G > 0) {
        w <- rgamma(n, shape = alpha, scale = 1)
        z <- (w - alpha) / sqrt(alpha)
      } else {
        w <- rgamma(n, shape = alpha, scale = 1)
        z <- - (w - alpha) / sqrt(alpha)
      }
    }
    mu + sigma * z
  }
  
  # collect bootstrap parameter draws
  draws <- matrix(NA_real_, nrow = B, ncol = 3)
  colnames(draws) <- c("meanlog", "sdlog", "skew")
  
  if (method == "nonparametric") {
    for (b in seq_len(B)) {
      yb <- sample(y, size = n, replace = TRUE)
      draws[b, ] <- c(mean(yb), sd(yb), skewness(yb))
    }
  } else { # parametric
    for (b in seq_len(B)) {
      yb <- rlp3_log(n, mu = mu_hat, sigma = sd_hat, G = G_hat)
      draws[b, ] <- c(mean(yb), sd(yb), skewness(yb))
    }
  }
  
  # covariance matrix of (meanlog, sdlog, skew)
  Sigma <- stats::cov(draws)
  
  list(
    meanlog = mu_hat,
    sdlog   = sd_hat,
    skew    = G_hat,
    Sigma   = Sigma,
    draws   = draws
  )
}
