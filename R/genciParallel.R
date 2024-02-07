# define the function to generate the CIs
genciParallel <- function (para, n, f = NULL, level = 0.9, edist = "gno", nsim = 1000, 
                            expand = FALSE, verbose = FALSE, showpar = FALSE, quiet = FALSE) {
  forParCIs <- function(f) {
    test <- qua2ci.simple(f, para, n, level = level, edist = edist, 
                          nsim = nsim, verbose = verbose, showpar = showpar, 
                          empdist = TRUE)
    if (test$ifail > 0) {
      ci_low <- ci_tru <- ci_hi <- NA
      ci_l1 <- ci_l2 <- ci_t3 <- ci_t4 <- ci_t5 <- NA
      ci_md <- ci_mu <- ci_var <- ci_skw <- NA
      next
    }
    ci_low <- test$lwr
    ci_tru <- test$true
    ci_hi <- test$upr
    ci_l1 <- test$elmoms$lambdas[1]
    ci_l2 <- test$elmoms$lambdas[2]
    ci_t3 <- test$elmoms$ratios[3]
    ci_t4 <- test$elmoms$ratios[4]
    ci_t5 <- test$elmoms$ratios[5]
    ci_md <- test$empdist$median
    ci_mu <- test$empdist$epmoms$moments[1]
    ci_var <- test$empdist$epmoms$moments[2]^2
    ci_skw <- test$empdist$epmoms$ratios[3]
    cis <- data.frame(nonexceed = f, lwr = ci_low, true = ci_tru, 
                      upr = ci_hi, qua_med = ci_md, qua_mean = ci_mu, qua_var = ci_var, 
                      qua_lam2 = ci_l2)
    return(cis)
  }
  if (is.null(f)) 
    f <- nonexceeds()
  if (!check.fs(f)) {
    warning("The provided nonexceedance probabilities are invalid")
    return()
  }
  if (!are.par.valid(para)) {
    warning("The distribution parameters are invalid")
    return()
  }
  if (!check.fs(level) | level == 1) {
    warning("argument 'ci' is not in [0,1)")
    return()
  }
  cis <- foreach(i = 1:length(f), .combine = dplyr::bind_rows, .packages = c("lmomco")) %dopar% 
    (forParCIs(f[i]))
  return(cis)
}
