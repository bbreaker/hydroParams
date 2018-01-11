library(ggplot2)
library(gridExtra)

p1 <- ggplot(testDF, aes(x = dates, y = flow, color = qual)) + 
  geom_point() + 
  geom_line(data = testDF, aes(x = dates, y = aveMove), color = "black", size = 1) +
  scale_y_log10(minor_breaks = c(-3:10 %o% 10^(-3:10))) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-03-01 00:00:00", "2017-05-01 00:00:00"), 
  #origin = "1970-01-01 00:00:00", tz = "UTC")) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.2),
        legend.background = element_blank())

p2 <- ggplot() + 
  #geom_line(data = testDFnon, aes(x = dates, y = slope), color = "grey50", size = 1) +
  #geom_line(data = testDF, aes(x = dates, y = absSlope), color = "grey80", size = 1) +
  geom_line(data = testDFnon, aes(x = dates, y = muP), color = "red", size = 1) +
  #geom_line(data = testDFnon, aes(x = dates, y = nuP), color = "blue", size = 1) +
  geom_line(data = testDFnon, aes(x = dates, y = sigP), color = "green", size = 1) +
  #scale_y_log10(minor_breaks = c(-10:10 %o% 10^(-10:10))) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-03-01 00:00:00", "2017-05-01 00:00:00"), 
  #origin = "1970-01-01 00:00:00", tz = "UTC")) +
  theme_bw() 

p3 <- ggplot() + 
  geom_line(data = testDFnon, aes(x = dates, y = slope), color = "grey50", size = 1) +
  geom_hline(yintercept = slpThrshldHgh, color = "red", linetype = "dashed") +
  geom_hline(yintercept = slpThrshldLow, color = "red", linetype = "dashed") +
  #geom_line(data = testDF, aes(x = dates, y = absSlope), color = "grey80", size = 1) +
  #scale_y_log10(minor_breaks = c(-10:10 %o% 10^(-10:10)), limits = c(0.00001, 1)) +
  scale_y_continuous(limits = c(-0.02, 0.02)) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-03-01 00:00:00", "2017-05-01 00:00:00"), 
  #origin = "1970-01-01 00:00:00", tz = "UTC")) +
  theme_bw()

grid.arrange(p1, p2, p3, ncol = 1)
