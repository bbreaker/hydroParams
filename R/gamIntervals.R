# function written by Will Asquith to compute intervals for gam regressions

gamIntervals <- function(gam_predicts_wsefit, gam=NULL, interval=c("none", "confidence", "prediction"), level=0.95, ...) {
  
  if(class(gam)[1] != "gam") {
    warning("need the actual GAM model too via the 'gam' argument")
    return()
  }
  
  z <- as.data.frame(gam_predicts_wsefit)
  
  if(! any(names(z) == "se.fit")) {
    warning("need gam predictions with se.fit=TRUE passed for 'z'")
    return()
  }
  
  interval <- match.arg(interval)
  
  sum.gam <- summary(gam); n <- sum.gam$n # summary.gam() and the sample size
  
  z$residual.scale <- sigma <- sqrt(sum.gam$scale) # residual standard error
  
  df <- n-sum(gam$edf) # total degrees of freedom
  
  QT <- abs(qt((1-level)/2, df)) # will do the +/- separately
  
  if(interval == "none") {
    z$lwr <- z$upr <- NA
  } else {
    one <- ifelse(interval == "confidence", 0, 1)
    tmp <- sqrt(one+(z$se.fit/sigma)^2)
    z$lwr <- z$fit - sigma*QT*tmp
    z$upr <- z$fit + sigma*QT*tmp
  }
  
  z <- z[,c(4,1,5,2,3)]
  
  attr(z, "interval") <- interval
  
  attr(z, "level") <- level
  
  attr(z, "t-dist_degrees_of_freedom") <- df
  
  return(z)
  
}
