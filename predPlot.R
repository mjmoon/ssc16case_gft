source("importData.R")
source("graphicSetting.R")
library(dynlm)
library(dyn)

minYr <- as.numeric(format(minDate, '%Y'))
minWk <- as.numeric(format(minDate + 3, '%U'))
ts.flupos <- ts(can$FluPos, frequency = 52, start = c(minYr, minWk))

ts.can.learn <- window(ts.flupos, start = c(minYr, minWk), 
                   end = c(2012, 52))
ts.can.valid <- window(ts.flupos, start = c(2013, 1))

# SARIMA prediction
ar.model <- arima(ts.can.learn, c(2,1,0), list(order = c(1,1,0)))
summary(ar.model)
ar.pred <- NULL
lsize <- length(ts.can.learn)
for(k in 1:(length(ts.can.valid))){
  ar.pred.loop <- forecast(Arima(
    c(ts.can.learn[min(0,lsize - k): lsize], ts.can.valid[1:k]), model = ar.model), h = 3)
  ar.pred <- rbind(ar.pred, 
                   c(fit = unlist(ar.pred.loop$mean[3]),
                     LL = unlist(ar.pred.loop$lower[3,2]), 
                     UL = unlist(ar.pred.loop$upper[3,2])))
}

ar.pred <- as.data.frame(ar.pred)
ar.pred$Date <- can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)]
ar.pred$FluPos <- can$FluPos[(length(can$FluPos)-length(ts.can.valid)+1) : length(can$FluPos)]
names(ar.pred)[2:3] <- c("LL", "UL")

# SARIMA forecast
ar.forec <- forecast(ar.model, h = length(ts.can.valid))
ar.forec.df <- NULL
ar.forec.df$LL <- ar.forec$lower[,2]
ar.forec.df$UL <- ar.forec$upper[,2]
ar.forec.df$fit <- ar.forec$mean
ar.forec.df$Date <- ar.pred$Date
ar.forec.df$FluPos <- ar.pred$FluPos
ar.forec.df <- as.data.frame(ar.forec.df)

# dynamic regression prediction
ts.gft <- ts(can$gft, frequency = 52, start = c(minYr, minWk))
ts.gft.can.learn <- window(ts.gft, start = c(minYr, minWk), 
                       end = c(2012, 52))
ts.gft.can.valid <- window(ts.gft, start = c(2013, 1))

dr.learn.dt <- data.frame(gft = ts.gft.can.learn, FluPos = ts.can.learn)
dr.valid.dt <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid)

dr.model <- dynlm(FluPos ~ L(FluPos, 3) + L(gft, 3) + L(FluPos, 52), data = dr.learn.dt)

dr.pred <- predict(dr.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                       fit = dr.pred$fit[,"fit"], 
                       UL = dr.pred$fit[,"upr"], 
                       LL = dr.pred$fit[,"lwr"], 
                       Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

# plots
png("Img/arForec.png", width = pltWidth, height = pltHeight)  
ggplot(data = ar.forec.df, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000)", title = "Two-year forecasts with SARIMA") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% confidence interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Forecasted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 25000)) +
  scale_y_continuous(labels = format1000)
dev.off()

png("Img/arPred.png", width = pltWidth, height = pltHeight)  
ggplot(data = ar.pred, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000)", title = "Two-year prediction from AR3") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% confidence interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)
dev.off()

png("Img/drPred.png", width = pltWidth, height = pltHeight)  
ggplot(data = dr.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000)", title = "Two-year prediction from DR33") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% confidence interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
       fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)
dev.off()