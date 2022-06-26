library(dplyr)
library(ggplot2)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

#table with absolut dispersals (from script 14)
absolut

#table with the ltt (from script 11)
df5
meanltt <- as.data.frame(df5)

relative <- inner_join(absolut, meanltt, by="time")
relative2 <- relative %>% mutate(relative= absolut/mean, time = time*-1)

#fazer gráfico absolut com os dados da tabela (tipo grafico em linha, e nao histograma)
png("Figure_relative_dispersal2.png", width = 480, height = 480)

plot(relative2$time, relative2$relative, type = "l", lwd=2, xlim = c(50,0),
     main = "Dispersal events / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#barplot
#bar_relative <- ggplot(relative2, aes(x = time, y = relative)) + 
#  geom_bar(stat="identity", width = 1) +
 # xlab("Time") +
  #ylab("Relative dispersal events") + xlim(50,0)
#bar_relative  

#export figure

#ggsave(filename = "Figure_relative_dispersal_events_all.png",  plot = last_plot(),  device = "png",  path = NULL,
 #      scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)

### WAM para CAR A -> J time_AtoJsr2
htime_AtoJsr2 <- hist(time_AtoJsr2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoJsr3 <- htime_AtoJsr2$counts
htime_AtoJsr3 <- as.data.frame(htime_AtoJsr3)
htime_AtoJsr3$time <- seq(0, 49, 1)

meanltt$time <- meanltt$time*-1
meanltt

rel_AtoJsr <- inner_join(htime_AtoJsr3, meanltt, by="time")
rel_AtoJsr2 <- rel_AtoJsr %>% mutate(relative= htime_AtoJsr3/mean)

png("Figure_relative_switch_AtoJ.png", width = 480, height = 480)
plot(rel_AtoJsr2$time, rel_AtoJsr2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Switch-range WAM -> CAR / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> WAM + CHO (A -> AG) time_AtoG2
htimeAtoG2 <- hist(time_AtoG2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoG3 <- htimeAtoG2$counts
htimeAtoG3 <- as.data.frame(htimeAtoG3)
htimeAtoG3$time <- seq(0, 49, 1)

rel_AtoG <- inner_join(htimeAtoG3, meanltt, by="time")
rel_AtoG2 <- rel_AtoG %>% mutate(relative= htimeAtoG3/mean)

png("Figure_relative_disp_AtoG.png", width = 480, height = 480)
plot(rel_AtoG2$time, rel_AtoG2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> WAM + CHO / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> WAM + EAM (A -> AD) time_AtoD2
htimeAtoD <- hist(time_AtoD2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoD2 <- htimeAtoD$counts
htimeAtoD2 <- as.data.frame(htimeAtoD2)
htimeAtoD2$time <- seq(0, 49, 1)

rel_AtoD <- inner_join(htimeAtoD2, meanltt, by="time")
rel_AtoD2 <- rel_AtoD %>% mutate(relative= htimeAtoD2/mean)

png("Figure_relative_disp_AtoD.png", width = 480, height = 480)
plot(rel_AtoD2$time, rel_AtoD2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> WAM + EAM / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

rel_AtoD3 <- as.data.frame(rel_AtoD2$relative)
ts_rel_AtoD3 <- ts(rel_AtoD3)
plot(ts_rel_AtoD3, xlim=c(50,0))


### WAM -> EAM (A -> D) switch-range time_AtoDsr2
htimeAtoDsr <- hist(time_AtoDsr2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoDsr2 <- htimeAtoDsr$counts
htimeAtoDsr2 <- as.data.frame(htimeAtoDsr2)
htimeAtoDsr2$time <- seq(0, 49, 1)

rel_AtoDsr <- inner_join(htimeAtoDsr2, meanltt, by="time")
rel_AtoDsr2 <- rel_AtoDsr %>% mutate(relative= htimeAtoDsr2/mean)

png("Figure_relative_switch_range_AtoD.png", width = 480, height = 480)
plot(rel_AtoDsr2$time, rel_AtoDsr2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Switch-Range WAM -> EAM / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> WAM + PAT (A -> AI) time_AtoI2
htimeAtoI <- hist(time_AtoI2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoI2 <- htimeAtoI$counts
htimeAtoI2 <- as.data.frame(htimeAtoI2)
htimeAtoI2$time <- seq(0, 49, 1)

rel_AtoI <- inner_join(htimeAtoI2, meanltt, by="time")
rel_AtoI2 <- rel_AtoI %>% mutate(relative= htimeAtoI2/mean)

png("Figure_relative_dispersal_AtoI.png", width = 480, height = 480)
plot(rel_AtoI2$time, rel_AtoI2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> WAM + PAT / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> SAF (A -> C) time_AtoCsr2
htimeAtoCsr <- hist(time_AtoCsr2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoCsr2 <- htimeAtoCsr$counts
htimeAtoCsr2 <- as.data.frame(htimeAtoCsr2)
htimeAtoCsr2$time <- seq(0, 49, 1)

rel_AtoCsr <- inner_join(htimeAtoCsr2, meanltt, by="time")
rel_AtoCsr2 <- rel_AtoCsr %>% mutate(relative= htimeAtoCsr2/mean)

png("Figure_relative_switch_range_AtoC.png", width = 480, height = 480)
plot(rel_AtoCsr2$time, rel_AtoCsr2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Switch-range WAM -> SAF / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> CHA (A -> B) time_AtoB2
htime_AtoB2 <- hist(time_AtoB2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoB2 <- htime_AtoB2$counts
htime_AtoB2 <- as.data.frame(htime_AtoB2)
htime_AtoB2$time <- seq(0, 49, 1)

rel_AtoB <- inner_join(htime_AtoB2, meanltt, by="time")
rel_AtoB2 <- rel_AtoB %>% mutate(relative= htime_AtoB2/mean)

png("Figure_relative_dispersal_AtoB.png", width = 480, height = 480)
plot(rel_AtoB2$time, rel_AtoB2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> WAM + CHA / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### SAF -> CHA (C -> BC) time_CtoB2
htime_CtoB2 <- hist(time_CtoB2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_CtoB2 <- htime_CtoB2$counts
htime_CtoB2 <- as.data.frame(htime_CtoB2)
htime_CtoB2$time <- seq(0, 49, 1)

rel_CtoB <- inner_join(htime_CtoB2, meanltt, by="time")
rel_CtoB2 <- rel_CtoB %>% mutate(relative= htime_CtoB2/mean)

png("Figure_relative_dispersal_CtoB.png", width = 480, height = 480)
plot(rel_CtoB2$time, rel_CtoB2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal SAF -> SAF + CHA / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


### WAM -> SAF (A -> AC) time_AtoC2
htimeAtoC <- hist(time_AtoC2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoC2 <- htimeAtoC$counts
htimeAtoC2 <- as.data.frame(htimeAtoC2)
htimeAtoC2$time <- seq(0, 49, 1)

meanltt$time <- meanltt$time*-1
meanltt

rel_AtoC <- inner_join(htimeAtoC2, meanltt, by="time")
rel_AtoC2 <- rel_AtoC %>% mutate(relative= htimeAtoC2/mean)

png("Figure_relative_dispersal_AtoC.png", width = 480, height = 480)
plot(rel_AtoC2$time, rel_AtoC2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> WAM + SAF / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


### WAM para CAR A -> AJ time_AtoJ2
htime_AtoJ2 <- hist(time_AtoJ2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoJ3 <- htime_AtoJ2$counts
htime_AtoJ3 <- as.data.frame(htime_AtoJ3)
htime_AtoJ3$time <- seq(0, 49, 1)

rel_AtoJ <- inner_join(htime_AtoJ3, meanltt, by="time")
rel_AtoJ2 <- rel_AtoJ %>% mutate(relative= htime_AtoJ3/mean)

png("Figure_relative_dispersal_AtoJ2.png", width = 480, height = 480)
plot(rel_AtoJ2$time, rel_AtoJ2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal WAM -> (WAM + CAR) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()
