source("importData.R")
source("graphicSetting.R")

corrres <- read.csv("Res/corr.csv")

corrres$illness <- factor(corrres$illness, 
                          levels = c("Influenza", "RSV", "Adenovirus", 
                                     "Parainfluenza","Rhinovirus", "hMPV", "Coronavirus"))

png("Img/corrSpAnnual.png", width = pltWidth, height = pltHeight)  
ggplot(data = subset(corrres, illness == "Influenza" & 
                       type == "Spearman" & year > 0)) + 
  mytheme +
  labs(x = "", y ="Correlation", title = "Spearman's correlation between GFT and influenza by year") +
  geom_line(aes(x = year + 0.5, y = corr, color = var, group = var), size = 1.2) +
  scale_x_continuous(breaks = c(2003:2015), 
                     labels = c("Sep-03", "Sep-04", "Sep-05", "Sep-06", "Sep-07",
                                "Sep-08", "Sep-09", "Sep-10", "Sep-11", "Sep-12",
                                "Sep-13", "Sep-14", "Sep-15")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), 
                     labels = c(0, 0.5, 1),
                     limits = c(0,1)) +
  scale_color_discrete(name = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png("Img/corrSpIllness.png", width = pltWidth, height = pltHeight)  
ggplot(data = subset(corrres, type =="Spearman"  & year == -1
                     & var != "Positive test percent")) +
  mytheme + labs(x = "", y = "Correlation", 
                 title = "Spearman's correlation between GFT and respiratory illnesses") +
  geom_bar(aes(x = illness, y = corr, fill = var), 
           position = "dodge", stat = "identity", width = 0.2) +
  scale_fill_discrete(name = "") +
  scale_y_continuous(breaks = c(0, 0.5, 1), 
                     labels = c(0, 0.5, 1),
                     lim = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.position = "right")
dev.off()

png("Img/corrLead.png", width = pltWidth, height = pltHeight)  
ggplot(data = subset(corrres, type =="Cross-correlation"  & year == -1
                     & var != "Positive test percent")) +
  mytheme + labs(x = "", y = "Weeks",
                 title = "Number of weeks GFT lead resipatory illnesses") +
  geom_bar(aes(x = illness, y = -lag, fill = var, alpha = as.numeric(corr)), 
           position = "dodge", stat = "identity", width = 0.2) +
  scale_fill_discrete(name = "") +
  scale_alpha_continuous(name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.position = "right") +
  geom_hline(yintercept = 0) 
dev.off()
