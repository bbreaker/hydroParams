library(dataRetrieval)

testDat <- readNWISuv("07068000", parameterCd = "00060", startDate = "2010-10-01", endDate = "2018-09-30", tz = "UTC")

testDF <- data.frame(dates = testDat$dateTime, flow = testDat$X_00060_00000)

#testDF <- dplyr::filter(testDF, dates >= as.POSIXct("2010-10-01 00:00:01"))
#
#testDF <- dplyr::filter(testDF, dates <= as.POSIXct("2018-10-01 00:00:01"))

source("https://raw.githubusercontent.com/bbreaker/hydroParams/master/R/runBFI.R")

source("https://raw.githubusercontent.com/bbreaker/hydroParams/master/R/runPART.R")

testDates <- seq(from = testDF[1, 1], to = testDF[nrow(testDF), 1], by = "15 mins")

testDatesDF <- data.frame(dates = testDates, val = "complete", stringsAsFactors = FALSE)

library(dplyr)

testDatesDF <- testDatesDF %>% 
  dplyr::left_join(testDF, "dates") %>% 
  dplyr::mutate(val = ifelse(is.na(val), "missing", val)) %>% 
  dplyr::mutate(flow = zoo::na.spline(flow)) %>% 
  data.frame()

testDF <- data.frame(dates = testDatesDF$dates, flow = testDatesDF$flow)

library(ggplot2)

ggplot(testDatesDF, aes(x = dates, y = flow, color = val)) + 
  geom_point(alpha = 0.8) + 
  scale_fill_manual(values = c("missing" = "red", "complete" = "olivedrab"))

library(plotly)

ggplotly(ggplot() + 
  geom_line(data = fallChunk, aes(x = dates, y = flow), color = "blue") + 
  geom_line(data = fallChunk, aes(x = dates, y = bFlow), color = "red") + 
  geom_line(data = fallChunk, aes(x = dates, y = runOff), color = "green3") + 
  geom_point(data = breaksDF, aes(x = dates, y = flow), color = "black", size = 3) + 
  geom_point(data = runOffBPDF, aes(x = dates, y = runOff), color = "black", size = 3) + 
  scale_y_log10(labels = scales::comma, minor_breaks = c(-1:10 %o% 10^(-1:10))) + 
  theme_bw())

ggplot() + 
  geom_line(data = fallChunk, aes(x = dates, y = flow), color = "blue") + 
  #geom_line(data = fallChunk, aes(x = dates, y = bFlow), color = "red") + 
  #geom_line(data = fallChunk, aes(x = dates, y = partQ), color = "green3") + 
  geom_point(data = test, aes(x = dates, y = flow), color = "red", size = 3) + 
  scale_y_log10(labels = scales::comma, minor_breaks = c(-1:10 %o% 10^(-1:10))) + 
  theme_bw()

ggplot(fallChunk, aes(x = dates, y = slopeAve)) + 
  geom_line() + 
  theme_bw()
  
