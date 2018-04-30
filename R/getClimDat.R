getClimDat <- function (siteID, lat, long, minDate, maxDate, keepFile = FALSE) {
  
  library(dplyr); library(Hmisc)
  
  if (as.numeric(stringr::str_sub(minDate, 1, 4) < 1980)) {
    
    stop("Data not avaliable prior to 1980")
    
  }
  
  if (length(lat) != length(long)) {
    
    stop("lat and long are not pairs")
    
  }
  
  yrRng <- paste(seq(as.numeric(stringr::str_sub(maxDate, 1, 4)), 
                     as.numeric(stringr::str_sub(minDate, 1, 4))), 
                 collapse = ",")
  
  if (length(lat) == 1) {
    
    dload <- sprintf("https://daymet.ornl.gov/data/send/saveData?lat=%s&lon=%s&measuredParams=tmax,tmin,dayl,prcp,srad,swe,vp&year=%s", 
                     lat, long, yrRng)
    
    x <- try(RCurl::getURL(dload, ssl.verifypeer = FALSE))
    
    if (!inherits(x, "try-error")) {
      
      dat <- read.csv(textConnection(x), skip = 7)
      
      dat$yday <- dat$yday - 1
      
      dat$date <- as.Date(dat$yday, origin = as.Date(paste0(dat$year, "-01-01")))
      
      dat <- mutate(dat, rad = (srad..W.m.2. * dayl..s.) / 1000000)
      
      dat$harET0 <- hargreavesInst(tmin = dat$tmin, tmax = dat$tmax, 
                                   times = as.POSIXlt(dat$date, format = "%Y-%m-%d"),
                                   pre = dat$prcp, lat = lat)
      
      names(dat) <- c("year", "julian", "dayl", "prcp", "srad", 
                      "swe", "tmax", "tmin", "vp", "date", "rad", "harET0")
      
      dat <- dat[,c(10, 1:9, 11, 12)]
      
      if (!is.null(siteID)) 
        
        dat <- data.frame(siteID = siteID, dat, stringsAsFactors = FALSE)
      
    }
    
    else {
      
      stop("There was an error retrieving climate data")
      
    }
    
  }
  
  else if (length(lat) > 1) {
    
    dat <- data.frame(matrix(ncol = 13, nrow = 0))
    
    names(dat) <- c("siteID", "date", "year", "julian", "dayl", "prcp", 
                    "srad", "swe", "tmax", "tmin", "vp", "rad", "harET0")
    
    for(i in seq(1, length(lat), 1)) {
      
      dload <- sprintf("https://daymet.ornl.gov/data/send/saveData?lat=%s&lon=%s&measuredParams=tmax,tmin,dayl,prcp,srad,swe,vp&year=%s", 
                       lat[i], long[i], yrRng)
      
      x <- try(RCurl::getURL(dload, ssl.verifypeer = FALSE))
      
      if (!inherits(x, "try-error")) {
        
        datN <- read.csv(textConnection(x), skip = 7)
        
        datN$yday <- datN$yday - 1
        
        datN$date <- as.Date(datN$yday, origin = as.Date(paste0(datN$year, "-01-01")))
        
        datN <- mutate(datN, rad = (srad..W.m.2. * dayl..s.) / 1000000)
        
        datN$harET0 <- hargreavesInst(tmin = datN$tmin, tmax = datN$tmax, 
                                      times = as.POSIXlt(datN$date, format = "%Y-%m-%d"), 
                                      pre = datN$prcp, lat = lat[i])
        
        names(datN) <- c("year", "julian", "dayl", "prcp", "srad",
                         "swe", "tmax", "tmin", "vp", "date", "rad", "harET0")
        
        datN <- datN[,c(10, 1:9, 11, 12)]
        
        if (!is.null(siteID)) 
          
          datN <- data.frame(siteID = siteID, datN, stringsAsFactors = FALSE)
        
      }
      
      else {
        
        stop("There was an error retrieving climate data")
        
      }
      
      dat <- dplyr::bind_rows(dat, datN)
      
    }
    
    dat <- dat %>%
      group_by(siteID, date, year, julian) %>%
      summarize_all(.funs = mean) %>%
      data.frame()
    
  }
                 
  if (keepFile == TRUE) {
    
    if (is.null(siteID))  {
      
      siteID <- paste(paste("lat", lat, sep = "_"), paste("long", long, sep = "_"), sep = ".")
      
    }
    
    write.csv(dat, paste(siteID, "_", yrRng[1], "_", yrRng[length(yrRng)], ".csv"))
    
  }
  
  return(dat)
  
}
