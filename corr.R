source("importData.R")
source("graphicSetting.R")

### Spearman's correlations ###
sp <- cor.test(can$gft, can$FluTest, method = "spearman")
corrRes <- data.frame(year = -1, illness = "Influenza", 
                      var = "Total test frequency", type = "Spearman", 
                      corr = sp$estimate, p.val = sp$p.value, lag = 0,
                      stringsAsFactors = FALSE, row.names = NULL)
sp <- cor.test(can$gft, can$FluPos, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Influenza", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
# sp <- cor.test(can$gft, can$FluPos/can$FluTest, method = "spearman")
# corrRes <- rbind(corrRes, c(-1, "Influenza", "Postive test percentage", "Spearman",
#                             sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$RSVtest, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "RSV", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$RSVpos, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "RSV", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$adenot, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Adenovirus", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$adeno, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Adenovirus", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$parat, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Parainfluenza", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$para, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Parainfluenza", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$Rhinot, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Rhinovirus", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$Rhino, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Rhinovirus", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$hMPVt, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "hMPV", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$hMPV, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "hMPV", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

sp <- cor.test(can$gft, can$Coronat, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Coronavirus", "Total test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))
sp <- cor.test(can$gft, can$Corona, method = "spearman")
corrRes <- rbind(corrRes, c(-1, "Coronavirus", "Postive test frequency", "Spearman",
                            sp$estimate, sp$p.value, 0))

### Spearman's correlations by year ###
sp.yr <- cbind(by(can, can$yr, FUN = function(x) cor(x$gft, x$FluTest, method = "spearman")),
               by(can, can$yr, FUN = function(x) cor(x$gft, x$FluPos, method = "spearman")),
               by(can, can$yr, FUN = function(x) cor(x$gft, x$FluPos/x$FluTest, method = "spearman"))
               )

sp.yr.df <- rbind(data.frame(year = row.names(sp.yr), illness = "Influenza", 
                             var = "Total test frequency", type = "Spearman", 
                             corr = sp.yr[,1], p.val = NA, lag = 0, 
                             row.names = NULL),
                  data.frame(year = row.names(sp.yr), illness = "Influenza", 
                             var = "Positive test frequency", type = "Spearman", 
                             corr = sp.yr[,2], p.val = NA, lag = 0, 
                             row.names = NULL),
                  data.frame(year = row.names(sp.yr), illness = "Influenza", 
                             var = "Positive test percentage", type = "Spearman", 
                             corr = sp.yr[,3], p.val = NA, lag = 0, 
                             row.names = NULL))
corrRes <- rbind(corrRes, sp.yr.df)

### cross-correlation with strongest association ###
findMaXCCF <- function(x, y, illness = "Influenza", var = "Total test frequency", year = -1){
  x <- x[!is.na(x) & !is.na(y)]
  y <- y[!is.na(x) & !is.na(y)]
  c <- ccf(x, y, plot = FALSE)
  df <- data.frame(year, illness, var, type = "Cross-correlation", 
                   corr = c$acf[,,1], p.val = NA, 
                   lag = c$lag[,,1], row.names = NULL)
  return(df[which.max(df$corr), ])
}

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$FluTest))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$FluPos, var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$RSVtest, illness = "RSV"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$RSVpos, 
                                     illness = "RSV", var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$adenot, illness = "Adenovirus"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$adeno, 
                                     illness = "Adenovirus", var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$parat, illness = "Parainfluenza"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$para, 
                                     illness = "Parainfluenza", var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$Rhinot, illness = "Rhinovirus"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$Rhino, 
                                     illness = "Rhinovirus", var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$hMPVt, illness = "hMPV"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$hMPV, 
                                     illness = "hMPV", var = "Positive test frequency"))

corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$Coronat, illness = "Coronavirus"))
corrRes <- rbind(corrRes, findMaXCCF(can$gft, can$Corona, 
                                     illness = "Coronavirus", var = "Positive test frequency"))

write.csv(corrRes, file = "./Res/corr.csv")
