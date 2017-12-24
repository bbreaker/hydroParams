shiftData <- function (x, k = 1, fill = NA, circular = FALSE) {
  fill.temp <- fill
  fill <- x[1L]
  fill[1L] <- fill.temp
  ckfact <- inherits(x, "factor")
  if (ckfact) {
    xlevs <- levels(x)
    xclass <- class(x)[1L]
    x <- as.character(x)
    fill <- as.character(fill)
  }
  k <- as.integer(k)
  if (k == 0L) 
    return(x)
  N <- length(x)
  if (k > 0L) {
    skip <- seq(k - 1L, 0L) - N
    if (circular) 
      x <- c(x[-skip], x[skip])
    else x <- c(rep(fill, k), x[skip])
  }
  else {
    skip <- seq(-1L, k)
    if (circular) 
      x <- c(x[skip], x[-skip])
    else x <- c(x[skip], rep(fill, -k))
  }
  if (ckfact) {
    if (xclass == "factor") {
      x <- factor(x, levels = xlevs)
    }
    else {
      x <- ordered(x, levels = xlevs)
    }
  }
  return(x)
}

na2miss <- function (x, to = -99999) {
  if (inherits(x, "factor")) {
    levs <- c(levels(x), as.character(to))
    x <- as.vector(x)
    x[is.na(x)] <- to
    return(factor(x, levels = levs))
  }
  x[is.na(x)] <- to
  return(x)
}

eventNum <- function (event, reset = FALSE, na.fix = FALSE) {
  event[is.na(event)] <- na.fix
  event.rle <- rle(c(event, FALSE))
  number <- 0L
  ret.val <- rep(0L, length(event) + 1L)
  i <- 1L
  beg = 1L
  while (i < length(event.rle$values)) {
    if (event.rle$values[i]) {
      number <- number + 1L
      end <- beg + event.rle$lengths[i] + event.rle$lengths[i + 1] - 1L
      ret.val[beg:end] <- number
      beg <- end + 1L
      i <- i + 2L
    }
    else {
      beg <- event.rle$lengths[i] + 1L
      i <- i + 1L
    }
  }
  ret.val <- ret.val[seq(along = event)]
  if (reset) 
    ret.val <- ifelse(event, ret.val, 0L)
  return(ret.val)
}

runPART <- function (flow, dates, drnArea) {
  flow <- pmax(flow, 1e-99)
  if (any(is.na(flow))) {
    retVec <- NA
  } else {
    Nact <- max(drnArea^0.2, 1)
    N <- as.integer(ceiling(Nact))
    NF <- max(N - 1L, 1L)
    NC <- max(N, 2L)
    NC1 <- NC + 1L
    ALLGWF <- ALLGWC <- ALLGWC1 <- rep(FALSE, length(flow))
    BaseQF <- BaseQC <- BaseQC1 <- rep(NA_real_, length(flow))
    DiffQ <- c(0, diff(flow))
    AnteF <- na2miss(stats::filter(DiffQ <= 0, rep(1, NF), sides = 1), 
                     0)
    AnteC <- na2miss(stats::filter(DiffQ <= 0, rep(1, NC), sides = 1), 
                     0)
    AnteC1 <- na2miss(stats::filter(DiffQ <= 0, rep(1, NC1), sides = 1), 
                      0)
    ALLGWF <- ifelse(AnteF == NF, TRUE, ALLGWF)
    BaseQF <- ifelse(ALLGWF, flow, BaseQF)
    ALLGWC <- ifelse(AnteC == NC, TRUE, ALLGWC)
    BaseQC <- ifelse(ALLGWC, flow, BaseQC)
    ALLGWC1 <- ifelse(AnteC1 == NC1, TRUE, ALLGWC1)
    BaseQC1 <- ifelse(ALLGWC1, flow, BaseQC1)
    CkQ <- (flow > 1e-09) & (flow/shiftData(flow, k = -1, fill = 1) > 
                               1.258925)
    ALLGWF <- ifelse(ALLGWF & CkQ, FALSE, ALLGWF)
    ALLGWC <- ifelse(ALLGWC & CkQ, FALSE, ALLGWC)
    ALLGWC1 <- ifelse(ALLGWC1 & CkQ, FALSE, ALLGWC1)
    Seq <- seq(length(flow))
    BaseQF <- exp(approx(Seq[ALLGWF], log(flow[ALLGWF]), xout = Seq, 
                         rule = 2)$y)
    BaseQC <- exp(approx(Seq[ALLGWC], log(flow[ALLGWC]), xout = Seq, 
                         rule = 2)$y)
    BaseQC1 <- exp(approx(Seq[ALLGWC1], log(flow[ALLGWC1]), xout = Seq, 
                          rule = 2)$y)
    while (any(CkQ <- (BaseQF > flow + 1e-06))) {
      CkQ <- CkQ & !ALLGWF
      Ck0 <- eventNum(!ALLGWF, reset = TRUE)
      CkE <- unique(Ck0[CkQ])
      for (i in CkE) {
        Sel <- which(Ck0 == i)
        MaxR <- BaseQF[Sel]/flow[Sel]
        Pck <- which.max(MaxR)
        ALLGWF[Sel[Pck]] <- TRUE
        BaseQF[Sel[Pck]] <- flow[Sel[Pck]]
      }
      BaseQF <- exp(approx(Seq[ALLGWF], log(flow[ALLGWF]), 
                           xout = Seq, rule = 2)$y)
      BaseQF <- ifelse(BaseQF < 1e-06, 0, BaseQF)
    }
    while (any(CkQ <- (BaseQC > flow + 1e-06))) {
      CkQ <- CkQ & !ALLGWC
      Ck0 <- eventNum(!ALLGWC, reset = TRUE)
      CkE <- unique(Ck0[CkQ])
      for (i in CkE) {
        Sel <- which(Ck0 == i)
        MaxR <- BaseQC[Sel]/flow[Sel]
        Pck <- which.max(MaxR)
        ALLGWC[Sel[Pck]] <- TRUE
        BaseQC[Sel[Pck]] <- flow[Sel[Pck]]
      }
      BaseQC <- exp(approx(Seq[ALLGWC], log(flow[ALLGWC]), 
                           xout = Seq, rule = 2)$y)
      BaseQC <- ifelse(BaseQC < 1e-06, 0, BaseQC)
    }
    while (any(CkQ <- (BaseQC1 > flow + 1e-06))) {
      CkQ <- CkQ & !ALLGWC1
      Ck0 <- eventNum(!ALLGWC1, reset = TRUE)
      CkE <- unique(Ck0[CkQ])
      for (i in CkE) {
        Sel <- which(Ck0 == i)
        MaxR <- BaseQC1[Sel]/flow[Sel]
        Pck <- which.max(MaxR)
        ALLGWC1[Sel[Pck]] <- TRUE
        BaseQC1[Sel[Pck]] <- flow[Sel[Pck]]
      }
      BaseQC1 <- exp(approx(Seq[ALLGWC1], log(flow[ALLGWC1]), 
                            xout = Seq, rule = 2)$y)
      BaseQC1 <- ifelse(BaseQC1 < 1e-06, 0, BaseQC1)
    }
    Ffact <- NC - Nact
    BaseQ <- BaseQF * Ffact + BaseQC * (1 - Ffact)
    retVec <- signif(BaseQ, 5)
  }
  return(retVec)
}
