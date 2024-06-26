library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)

testDFPlot <- dplyr::filter(testDF, dates >= as.POSIXct("2017-03-01 00:00:00", tz = "UTC") & 
                              dates <= as.POSIXct("2017-07-01 00:00:00", tz = "UTC"))

colors <- grDevices::colors()[grep(c('white'), grDevices::colors(), invert = T)]

testEventVals <- unique(testDFPlot$eventVal)

p1 <- ggplot(testDFPlot, aes(x = dates, y = flow, color = as.character(eventVal))) + 
  geom_line(size = 1) + 
  #geom_line(data = testDF, aes(x = dates, y = aveMove), color = "black", size = 1) +
  #geom_line(data = testDFPlot, aes(x = as.POSIXct(dateNew, format = "%Y-%m-%d"), y = baseQ), 
            #color = "red", size = 0.5, linetype = "dashed") +
  #geom_line(data = testDFPlot, aes(x = as.POSIXct(dateNew, format = "%Y-%m-%d"), y = dailyQ), 
            #color = "green", size = 0.5, linetype = "dashed") +
  #scale_y_log10(minor_breaks = c(-3:10 %o% 10^(-3:10))) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-03-01 00:00:00", "2017-07-01 00:00:00"), 
                                       #origin = "1970-01-01 00:00:00", tz = "UTC")) +
  labs(y = "flow") +
  scale_color_manual(values = sample(colors, length(testEventVals))) + 
  theme_linedraw() +
  theme(legend.position = "right",
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank())

p2 <- ggplot() + 
  #geom_line(data = testDFnon, aes(x = dates, y = slope), color = "grey50", size = 1) +
  #geom_line(data = testDF, aes(x = dates, y = absSlope), color = "grey80", size = 1) +
  geom_point(data = testVals, aes(x = dates, y = muP, color = "mu")) +
  #geom_line(data = testDFnon, aes(x = dates, y = nuP), color = "blue", size = 1) +
  geom_point(data = testVals, aes(x = dates, y = sigP, color = "sigma")) +
  #scale_y_log10(minor_breaks = c(-10:10 %o% 10^(-10:10))) +
  #scale_y_continuous(limits = c(-0.01, 0.01)) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-01-03 00:00:00", "2017-06-01 00:00:00"), 
  #                                     origin = "1970-01-01 00:00:00", tz = "UTC")) +
  labs(y = "moments \n") +
  theme_linedraw() +
  theme(legend.position = c(0.99, 0.5),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank())

p3 <- ggplot() + 
  geom_point(data = testVals, aes(x = dates, y = slope), color = "grey50", size = 1) +
  geom_line(data = testVals, aes(x = dates, y = slpThrshldHgh), color = "red", linetype = "dashed") +
  geom_line(data = testVals, aes(x = dates, y = slpThrshldLow), color = "red", linetype = "dashed") +
  #geom_line(data = testDF, aes(x = dates, y = absSlope), color = "grey80", size = 1) +
  #scale_y_log10(minor_breaks = c(-10:10 %o% 10^(-10:10)), limits = c(0.00001, 1)) +
  #scale_y_continuous(limits = c(-0.01, 0.01)) +
  #scale_x_datetime(limits = as.POSIXct(c("2017-01-03 00:00:00", "2017-06-01 00:00:00"), 
  #                                     origin = "1970-01-01 00:00:00", tz = "UTC")) +
  labs(y = "slope \n") +
  theme_linedraw()

grid.arrange(p1, p2, p3, ncol = 1)

p4 <- ggplot(testDF, aes(x = dates, y = flow, color = qual)) + 
  geom_point() + 
  geom_line(data = testDF, aes(x = dates, y = aveMove), color = "black", size = 1) +
  scale_y_log10(minor_breaks = c(-3:10 %o% 10^(-3:10))) +
  scale_x_datetime(limits = as.POSIXct(c("2017-01-03 00:00:00", "2017-06-01 00:00:00"), 
                                       origin = "1970-01-01 00:00:00", tz = "UTC")) +
  labs(y = "flow \n \n \n") +
  theme_linedraw() +
  theme(legend.position = c(0.99, 0.5),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank())

p5 <- ggplot(testVals, aes(x = dates, y = eventNum)) +
  geom_point() +
  scale_x_datetime(limits = as.POSIXct(c("2017-01-03 00:00:00", "2017-06-01 00:00:00"), 
                                       origin = "1970-01-01 00:00:00", tz = "UTC")) +
  #scale_y_continuous(limits = c(2325, 2400), breaks = seq(2325, 2400, 5)) +
  labs(y = "eventNum \n") +
  theme_linedraw()

grid.arrange(p1, p5, ncol = 1)
