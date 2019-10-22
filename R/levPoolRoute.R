#' level_pool_routing
#' @param lt data.frame with time and inflow columns
#' @param qh data.frame with elevation and discharge columns.
#'  Storage column optional.
#' @param area numeric reservoir area
#' @param tStep numeric time step interval in seconds.
#' @param initFlow numeric
#' @param initStor numeric
#' @param fitType logical operator specifying a linear
#'  relationship between outflow and reservoir-change-in-storage
#' @importFrom mgcv gam

levelPoolRoute <- function(lt, qh, area, tStep, initFlow, initStor, fitType){
  
  lagpad <- function(x, k) {
    c(rep(NA, k), x)[1 : length(x)] 
  }
  
  lt$ii <- apply(cbind(lagpad(lt$inflow, 1), lt$inflow), 1, sum)
  
  if (is.null(qh$storage)){
    qh$storage <- area * qh$elevation
  }
  
  qh$stq       <- ((2 * qh$storage) / (tStep)) + qh$discharge
  
  lt$sjtminq   <- NA
  lt$sj1tplusq <- NA
  lt$outflow   <- NA
  lt[1, c("sj1tplusq")] <- c(NA)
  lt[1, c("sjtminq")] <- ((2 * initStor) / tStep) -
    initFlow
  
  lt[1, "outflow"] <- initFlow
  
  if (fitType == "linear"){
    
    fit <- lm(discharge ~ stq, data = qh)
    
  } else if (fitType == "gam") {
    
    fit <- mgcv::gam(discharge ~ s(stq, k = 3), data = qh)
    
    plot(qh$stq, qh$discharge, xlab = "Change-in-storage-with-time",
         ylab = "Discharge")
    
    lines(qh$stq, predict(fit))
    
  }
  
  for(i in seq_len(nrow(lt))[-1]){
    lt[i, "sj1tplusq"] <- lt[i-1, "sjtminq"] + lt[i, "ii"]
    lt[i, "outflow"]   <- predict(fit,
                                  data.frame(stq = lt[i, "sj1tplusq"]))
    lt[i, "sjtminq"]   <- lt[i, "sj1tplusq"] -
      (lt[i, "outflow"] * 2)
  }
  
  lt
}
