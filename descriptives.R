source("importData.R")
source("graphicSetting.R")

png("Img/des.Can1.png", width = pltWidth, height = pltHeight)
ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "Frequency (10,000)\nand Percentage", title = "GFT vs influenza test frequencies in Canada") +
  geom_line(aes(y = FluPos/10000, color = "Positive test")) +
  geom_line(aes(y = FluTest/10000, color = "Total test")) +
  geom_line(aes(y = FluPos/FluTest, color = "Positive test percentage"), linetype = 2) +
  geom_line(aes(y = gft/10000, color = "Google Flu Trend")) +
  scale_color_brewer(palette = "Set1", 
                     guide = guide_legend(nrow = 2, title = ""))
dev.off()


plt.total <- ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "", title = "Total frequency") +
  geom_line(aes(y = FluTest/10000, color = "Influenza tests")) +
  geom_line(aes(y = gft/10000, color = "Google Flu Trend")) +
  scale_color_brewer(palette = "Set1", name = "") +
  theme(plot.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(0.8)))

plt.legend <- get_legend(plt.total)

plt.posfr <- ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "", title = "Positive frequency") +
  geom_line(aes(y = FluPos/10000, color = "Positive test (10,000)")) +
  geom_line(aes(y = gft/10000, color = "Google Flu Trend per (10,000)")) +
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  theme(plot.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(0.8)))

plt.pospc <- ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "", title = "Positive percentage") +
  geom_line(aes(y = FluPos/FluTest, color = "Positive test percentage")) +
  geom_line(aes(y = gft/10000, color = "Google Flu Trend (10,000)")) +
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  theme(plot.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(0.8)))

png("Img/des.Can2.png", width = pltWidth, height = pltHeight)
grid.arrange(plt.total + theme(legend.position = "none"), plt.posfr, plt.pospc, plt.legend, 
             nrow = 2, ncol = 3, layout_matrix = rbind(c(1,2,3),c(4,4,4)), heights = c(0.9,0.1))
dev.off()

plt.total.other <- ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "Frequency (10,000)", title = "") +
  geom_line(aes(y = RSVtest/10000, color = "RSV")) +
  geom_line(aes(y = adenot/10000, color = "Adenovirus")) +
  geom_line(aes(y = parat/10000, color = "Parainfluenza")) +
  geom_line(aes(y = Rhinot/10000, color = "Rhinovirus")) +
  geom_line(aes(y = hMPVt/10000, color = "hMPV")) +
  geom_line(aes(y = Coronat/10000, color = "Coronavirus")) +
  geom_line(aes(y = gft/10000, color = "Google Flu Trend")) +
  scale_color_brewer(palette = "Set1", 
                     guide = guide_legend(nrow = 2, title = "", byrow = TRUE),
                     breaks = c("Google Flu Trend", "Adenovirus", "RSV", "Parainfluenza", 
                                "hMPV", "Rhinovirus", "Coronavirus"))

plt.legend <- get_legend(plt.total.other)

plt.pospc.other <- ggplot(data = can, aes(x = Date)) + mytheme +
  labs(x = "", y = "Percentages", title = "") +
  geom_line(aes(y = RSVpos/RSVtest, color = "RSV")) +
  geom_line(aes(y = adeno/adenot, color = "Adenovirus")) +
  geom_line(aes(y = para/parat, color = "Parainfluenza")) +
  geom_line(aes(y = Rhino/Rhinot, color = "Rhinovirus")) +
  geom_line(aes(y = hMPV/hMPVt, color = "hMPV")) +
  geom_line(aes(y = Corona/Coronat, color = "Coronavirus")) +
  geom_line(aes(y = -1, color = "Goolge Flu Trend")) +
  scale_color_brewer(palette = "Set1", 
                     guide = FALSE) +
  ylim(c(0, 0.5))


png("Img/des.Can.other.png", width = pltWidth, height = pltHeight)
grid.arrange(plt.total.other + theme(legend.position = "none"), plt.pospc.other, plt.legend, 
             nrow = 2, ncol = 2, layout_matrix = rbind(c(1,2),c(3,3)), heights = c(0.9,0.1),
             top = textGrob("GFT vs other respiratory illness test frequencies in Canada",
                            gp=gpar(fontsize=22)))
dev.off()