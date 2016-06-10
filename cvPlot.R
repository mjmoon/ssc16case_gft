source("importData.R")
source("cvloop.R")
source("graphicSetting.R")

can.res <- rbind(cvloop(can, type = "AR", arpdq = c(2,1,0), arpdqs = c(1,1,0)),
                 cvloop(can, type = "DR"))
on.res <- rbind(cvloop(on, type = "AR", arpdq = c(2,1,0), arpdqs = c(1,1,0)),
                cvloop(on, type = "DR"))
qc.res <- rbind(cvloop(qc, type = "AR", arpdq = c(2,1,0), arpdqs = c(1,1,0)),
                cvloop(qc, type = "DR"))
bc.res <- rbind(cvloop(bc, type = "AR", arpdq = c(2,1,0), arpdqs = c(1,1,0)),
                cvloop(bc, type = "DR"))
ab.res <- rbind(cvloop(ab, type = "AR", arpdq = c(2,1,0), arpdqs = c(1,1,0)),
                cvloop(ab, type = "DR"))

armethods <- c("AR2", "AR3", "AR4")
drmethods <- c("DR20", "DR22", "DR30", "DR33", "DR40", "DR44")

png("Img/arCanada_bar.png", width = pltWidth, height = pltHeight)
ggplot(data = subset(can.res, index == 1 & method %in% armethods), 
       aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Cross-validation MSE\n\nCanada") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0))) +
  scale_y_continuous(labels = format1000)
dev.off()

png("Img/drCanada_bar.png", width = pltWidth, height = pltHeight)
ggplot(data = subset(can.res, index == 1 & method %in% drmethods), 
       aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Cross-validation MSE\n\nCanada") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0)))+
  scale_y_continuous(labels = format1000)
dev.off()

plt.dr.on <- ggplot(data = subset(on.res, index == 1 & method %in% drmethods), 
                 aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Ontario") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0))) + 
  scale_fill_discrete(guide = guide_legend(ncol = 1, title = ""),
                      labels = c("DR20: Dynamic regression with 2-week lead",
                                 "DR22: Dynamic regression with 2-week lead/GFT",
                                 "DR30: Dynamic regression with 3-week lead",
                                 "DR33: Dynamic regression with 3-week lead/GFT",
                                 "DR40: Dynamic regression with 4-week lead",
                                 "DR44: Dynamic regression with 4-week lead/GFT")) +
  scale_y_continuous(labels = format1000)

plt.dr.legend <- get_legend(plt.dr.on)

plt.dr.bc <- ggplot(data = subset(bc.res, index == 1 & method %in% drmethods), 
                 aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "British Columbia") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0))) +
  scale_y_continuous(labels = format1000)

plt.dr.ab <- ggplot(data = subset(ab.res, index == 1 & method %in% drmethods), 
                 aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Alberta") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0))) +
  scale_y_continuous(labels = format1000)

png("Img/drProvinces_bar.png", width = pltWidth, height = pltWidth)
grid.arrange(plt.dr.on + theme(legend.position = "none"), plt.dr.bc, plt.dr.ab, plt.dr.legend,
             ncol = 2, nrow = 2)
dev.off()

plt.ar.on <- ggplot(data = subset(on.res, index == 1 & method %in% armethods), 
                    aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Ontario") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0))) + 
  scale_fill_discrete(guide = guide_legend(ncol = 1, title = ""),
                      labels = c("AR2: SARIMA 2-week forecast",
                                 "AR3: SARIMA 3-week forecast",
                                 "AR4: SARIMA 4-week forecast")) +
  scale_y_continuous(labels = format1000)

plt.ar.legend <- get_legend(plt.ar.on)

plt.ar.bc <- ggplot(data = subset(bc.res, index == 1 & method %in% armethods), 
                    aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "British Columbia") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0))) +
  scale_y_continuous(labels = format1000)

plt.ar.ab <- ggplot(data = subset(ab.res, index == 1 & method %in% armethods), 
                    aes(x = method, y = meanMSE, fill = method)) + mytheme +
  labs(x = "", y = "Cross-validation MSE (1,000)", title = "Alberta") +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position = "none", axis.title.y=element_text(margin=margin(0,20,0,0))) +
  scale_y_continuous(labels = format1000)

png("Img/arProvinces_bar.png", width = pltWidth, height = pltWidth)
grid.arrange(plt.ar.on + theme(legend.position = "none"), plt.ar.bc, plt.ar.ab, plt.ar.legend,
             ncol = 2, nrow = 2)
dev.off()

save(can.res, on.res, bc.res, ab.res, file = "Res.RData")
# load("Res/Res.RData")
