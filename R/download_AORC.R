rfc <- "LMRFC"

theTimeVec <- seq(as.Date("1979-02-01"), as.Date("2022-07-01"), by = "1 months")

theTimeVec <- format(theTimeVec, "%Y%m")

dest <- "C:\\Path\\to\\write\\data"

library(RCurl)
options(timeout = 900)

for (i in 1:length(theYrVec)) {
  
  download.file(paste0("https://hydrology.nws.noaa.gov/aorc-historic/AORC_", rfc, "_4km/", rfc, "_precip_partition/AORC_APCP_4KM_", rfc, "_", theTimeVec[i], ".zip"), 
                destfile = paste0(dest, "\\AORC_APCP_4KM_", rfc, "_", theTimeVec[i], ".zip"), 
                mode = "wb", quiet = TRUE)
  
  cat(paste("\nDone with precip at for", theTimeVec[i], "at", Sys.time(), "\n"))
  
  download.file(paste0("https://hydrology.nws.noaa.gov/aorc-historic/AORC_", rfc, "_4km/AORC_TMPR_4KM_", rfc, "_", theTimeVec[i], ".zip"), 
                destfile = paste0(dest, "\\AORC_TMPR_4KM_", rfc, "_", theTimeVec[i], ".zip"), 
                mode = "wb", quiet = TRUE)
  
  cat(paste("\nDone with temp for", theYrVec[i], "at", Sys.time(), "\n"))
  
}
