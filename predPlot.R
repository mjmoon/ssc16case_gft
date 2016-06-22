source("importData.R")
source("graphicSetting.R")
library(dynlm)
library(dyn)
library(forecast)

minYr <- as.numeric(format(minDate, '%Y'))
minWk <- as.numeric(format(minDate + 3, '%U'))
ts.flupos <- ts(can$FluPos, frequency = 52, start = c(minYr, minWk))

ts.can.learn <- window(ts.flupos, start = c(minYr, minWk), 
                   end = c(2012, 52))
ts.can.valid <- window(ts.flupos, start = c(2013, 1))

# SARIMA prediction
ar.model <- arima(ts.can.learn, c(2,1,0), list(order = c(1,1,0)))
# ar.model <- arima(ts.can.learn, c(1,0,0), list(order = c(0,0,0)))
summary(ar.model)
ar.pred <- NULL
lsize <- length(ts.can.learn)
for(k in 1:(length(ts.can.valid))){
  ar.pred.loop <- forecast(Arima(
    c(ts.can.learn[min(0,lsize - k): lsize], ts.can.valid[1:k]), model = ar.model), h = 3)
  ar.pred <- rbind(ar.pred, 
                   c(unlist(ar.pred.loop$mean[3]),
                     unlist(ar.pred.loop$lower[3,2]), 
                     unlist(ar.pred.loop$upper[3,2])))
}

ar.pred <- as.data.frame(ar.pred)
names(ar.pred) <- c("fit", "LL", "UL")
ar.pred$Date <- can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)]
ar.pred$FluPos <- can$FluPos[(length(can$FluPos)-length(ts.can.valid)+1) : length(can$FluPos)]

# SARIMA forecast
ar.forec <- forecast(ar.model, h = length(ts.can.valid))
ar.forec.df <- NULL
ar.forec.df$LL <- ar.forec$lower[,2]
ar.forec.df$UL <- ar.forec$upper[,2]
ar.forec.df$fit <- ar.forec$mean
ar.forec.df$Date <- ar.pred$Date
ar.forec.df$FluPos <- ar.pred$FluPos
ar.forec.df <- as.data.frame(ar.forec.df)

# plots
png("Img/arForec.png", width = pltWidth, height = pltHeight)  
ggplot(data = ar.forec.df, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "Two-year forecasts with SARIMA") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
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
  labs(x = "Year", y = "Frequency (1,000s)", title = "Two-year prediction from AR3") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)
dev.off()

# dynamic regression prediction
ts.gft <- ts(can$gft, frequency = 52, start = c(minYr, minWk))
ts.gft.can.learn <- window(ts.gft, start = c(minYr, minWk), 
                           end = c(2012, 52))
ts.gft.can.valid <- window(ts.gft, start = c(2013, 1))

dr.learn.dt <- data.frame(gft = ts.gft.can.learn, FluPos = ts.can.learn)
dr.valid.dt <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid)

dr33.model <- dynlm(FluPos ~ L(FluPos, 3) + L(gft, 3) + L(FluPos, 52), data = dr.learn.dt)
dr33.pred <- predict(dr33.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr33.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                       fit = dr33.pred$fit[,"fit"], 
                       UL = dr33.pred$fit[,"upr"], 
                       LL = dr33.pred$fit[,"lwr"], 
                       Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

dr20.model <- dynlm(FluPos ~ L(FluPos, 2) + L(FluPos, 52), data = dr.learn.dt)
dr20.pred <- predict(dr20.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr20.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr20.pred$fit[,"fit"], 
                         UL = dr20.pred$fit[,"upr"], 
                         LL = dr20.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

dr30.model <- dynlm(FluPos ~ L(FluPos, 3) + L(FluPos, 52), data = dr.learn.dt)
dr30.pred <- predict(dr30.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr30.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr30.pred$fit[,"fit"], 
                         UL = dr30.pred$fit[,"upr"], 
                         LL = dr30.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

dr40.model <- dynlm(FluPos ~ L(FluPos, 4) + L(FluPos, 52), data = dr.learn.dt)
dr40.pred <- predict(dr40.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr40.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr40.pred$fit[,"fit"], 
                         UL = dr40.pred$fit[,"upr"], 
                         LL = dr40.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

dr.33plt <- ggplot(data = dr33.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR33") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)

plt.legend <- get_legend(dr.33plt)

dr.20plt <- ggplot(data = dr20.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR20") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000) + theme(legend.position = "none")

dr.30plt <- ggplot(data = dr30.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR30") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000) + theme(legend.position = "none")

dr.40plt <- ggplot(data = dr40.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR40") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000) + theme(legend.position = "none")

png("Img/dr4Preds.png", width = 1.2*pltWidth, height = 1.2*pltHeight)
grid.arrange(plt.legend, dr.33plt + theme(legend.position = "none"), 
             dr.20plt, dr.30plt, dr.40plt, 
             ncol = 2, nrow = 3, layout_matrix = rbind(c(1,1), c(2,3), c(4,5)), 
             heights = c(0.1, 0.45, 0.45),
             top = textGrob("Predictions from dynamic regression models", gp=gpar(fontsize=25)))
dev.off()


dr.learn.dt <- data.frame(gft = ts.gft.can.learn, FluPos = ts.can.learn)
dr.valid.dt <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid)

splitVar <- function(var, name, df.learn, df.valid){
  tsvar <- ts(var, frequency = 52, start = c(minYr, minWk))
  
  tsvar.learn <- window(tsvar, start = c(minYr, minWk), 
                             end = c(2012, 52))
  tsvar.valid <- window(tsvar, start = c(2013, 1))
  
  df.learn$new <- tsvar.learn
  df.valid$new <- tsvar.valid
  
  names(df.learn)[ncol(df.learn)] <- name
  names(df.valid)[ncol(df.valid)] <- name
  
  return(list(df.learn, df.valid))
}

tmp <- splitVar(can$FluTest, "FluTest", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$RSVtest, "RSVtest", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$adenot, "adenot", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$parat, "parat", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$RSVpos, "RSVpos", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$adeno, "adeno", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

tmp <- splitVar(can$para, "para", dr.learn.dt, dr.valid.dt)
dr.learn.dt <- tmp[[1]]
dr.valid.dt <- tmp[[2]]

dr3a.model <- dynlm(FluPos ~ L(FluPos, 3) + L(FluTest, 3) + L(RSVtest, 3) 
                    + L(adenot, 3) + L(parat, 3) + L(FluPos, 52), data = dr.learn.dt)
dr3a.pred <- predict(dr3a.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr3a.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr3a.pred$fit[,"fit"], 
                         UL = dr3a.pred$fit[,"upr"], 
                         LL = dr3a.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

ggplot(data = dr3a.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR with multiple regressors 1") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)

dr3b.model <- dynlm(FluPos ~ L(FluPos, 3) + L(RSVpos, 3) 
                    + L(adeno, 3) + L(para, 3) + L(FluPos, 52), data = dr.learn.dt)
dr3b.pred <- predict(dr3b.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr3b.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr3b.pred$fit[,"fit"], 
                         UL = dr3b.pred$fit[,"upr"], 
                         LL = dr3b.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

ggplot(data = dr3b.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR with multiple regressors 2") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)

dr2b.model <- dynlm(FluPos ~ L(FluPos, 2) + L(RSVpos, 2) 
                    + L(adeno, 2) + L(para, 2) + L(FluPos, 52), data = dr.learn.dt)
dr2b.pred <- predict(dr2b.model, newdata = dr.valid.dt, interval = "predict", se.fit = TRUE)
dr2b.valid <- data.frame(gft = ts.gft.can.valid, FluPos = ts.can.valid,
                         fit = dr2b.pred$fit[,"fit"], 
                         UL = dr2b.pred$fit[,"upr"], 
                         LL = dr2b.pred$fit[,"lwr"], 
                         Date = can$Date[(length(can$Date)-length(ts.can.valid)+1) : length(can$Date)])

ggplot(data = dr2b.valid, aes(x = Date, y = fit)) + 
  mytheme +
  labs(x = "Year", y = "Frequency (1,000s)", title = "DR with multiple regressors 2") +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "95% prediction interval")) +
  geom_line(aes(y = FluPos, col = "Actual")) +
  geom_line(aes(col = "Predicted"))  + 
  scale_fill_brewer(name = "") + 
  scale_color_discrete(name = "") +
  coord_cartesian(ylim = c(-5, 5000)) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  scale_y_continuous(labels = format1000)
