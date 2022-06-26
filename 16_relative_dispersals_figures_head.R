library(dplyr)
library(ggplot2)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head")

#table with absolut dispersals (from script 14)
absolut

#table with the ltt (from script 11)
df5
meanltt <- as.data.frame(df5)
meanltt2 <- meanltt %>% mutate(time = time*-1)

relative <- inner_join(absolut, meanltt2, by="time")
relative2 <- relative %>% mutate(relative= absolut/mean)

#fazer gráfico absolut com os dados da tabela (tipo grafico em linha, e nao histograma)
png("Figure_relative_dispersal_plus_Smoothed.png", width = 480, height = 480)
plot(relative2$time, relative2$relative, type = "l", lwd=2, xlim = c(50,0),
     main = "Dispersal events / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
lines(disSMA)
dev.off()

####LTT,

png("Figure_absolut_LTT.png", width = 480, height = 480)
plot(relative2$time, relative2$mean, type = "l", lwd=2, xlim = c(50,0),
     main = "Lineage through time", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


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
     main = "Dispersal (WAM -> WAM + CHO) / LTT", xlab = "Time", ylab = NA,
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

png("Figure_relative_disp_WAM_to_EAM.png", width = 480, height = 480)
plot(rel_AtoD2$time, rel_AtoD2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + EAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


### WAM -> EAM (A -> D) switch-range time_AtoDsr2
htimeAtoDsr <- hist(time_AtoDsr2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoDsr2 <- htimeAtoDsr$counts
htimeAtoDsr2 <- as.data.frame(htimeAtoDsr2)
htimeAtoDsr2$time <- seq(0, 49, 1)

rel_AtoDsr <- inner_join(htimeAtoDsr2, meanltt, by="time")
rel_AtoDsr2 <- rel_AtoDsr %>% mutate(relative= htimeAtoDsr2/mean)

png("Figure_relative_switch_range_WAM_to_EAM.png", width = 480, height = 480)
plot(rel_AtoDsr2$time, rel_AtoDsr2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Switch-Range (WAM -> EAM) / LTT", xlab = "Time", ylab = NA,
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

png("Figure_relative_dispersal_WAMtoPAT.png", width = 480, height = 480)
plot(rel_AtoI2$time, rel_AtoI2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + PAT) / LTT", xlab = "Time", ylab = NA,
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
     main = "(WAM -> WAM + CHA) / LTT", xlab = "Time", ylab = NA,
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
     main = "(SAF -> SAF + CHA) / LTT", xlab = "Time", ylab = NA,
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

png("Figure_relative_dispersal_WAMtoSAF.png", width = 480, height = 480)
plot(rel_AtoC2$time, rel_AtoC2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + SAF) / LTT", xlab = "Time", ylab = NA,
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
     main = "(WAM -> WAM + CAR) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM para MES A -> AF time_AtoF2
htime_AtoF2 <- hist(time_AtoF2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoF3 <- htime_AtoF2$counts
htime_AtoF3 <- as.data.frame(htime_AtoF3)
htime_AtoF3$time <- seq(0, 49, 1)

rel_AtoF <- inner_join(htime_AtoF3, meanltt, by="time")
rel_AtoF2 <- rel_AtoF %>% mutate(relative= htime_AtoF3/mean)

png("Figure_relative_dispersal_AtoF.png", width = 480, height = 480)
plot(rel_AtoF2$time, rel_AtoF2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(WAM -> WAM + MES) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM para DNO A -> AH time_AtoH2
htime_AtoH2 <- hist(time_AtoH2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoH3 <- htime_AtoH2$counts
htime_AtoH3 <- as.data.frame(htime_AtoH3)
htime_AtoH3$time <- seq(0, 49, 1)

rel_AtoH <- inner_join(htime_AtoH3, meanltt, by="time")
rel_AtoH2 <- rel_AtoH %>% mutate(relative= htime_AtoH3/mean)

png("Figure_relative_dispersal_AtoH.png", width = 480, height = 480)
plot(rel_AtoH2$time, rel_AtoH2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(WAM -> WAM + DNO) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### SAF -> NAF (C -> CE) time_CtoE2
htime_CtoE2 <- hist(time_CtoE2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_CtoE2 <- htime_CtoE2$counts
htime_CtoE2 <- as.data.frame(htime_CtoE2)
htime_CtoE2$time <- seq(0, 49, 1)

rel_CtoE <- inner_join(htime_CtoE2, meanltt, by="time")
rel_CtoE2 <- rel_CtoE %>% mutate(relative= htime_CtoE2/mean)

png("Figure_relative_dispersal_SAF_to_NAF.png", width = 480, height = 480)
plot(rel_CtoE2$time, rel_CtoE2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (SAF -> SAF + NAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### NAF -> SAF (E -> CE) time_EtoC2
htime_EtoC2 <- hist(time_EtoC2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_EtoC2 <- htime_EtoC2$counts
htime_EtoC2 <- as.data.frame(htime_EtoC2)
htime_EtoC2$time <- seq(0, 49, 1)

rel_EtoC <- inner_join(htime_EtoC2, meanltt, by="time")
rel_EtoC2 <- rel_EtoC %>% mutate(relative= htime_EtoC2/mean)

png("Figure_relative_dispersal_NAF_to_SAF.png", width = 480, height = 480)
plot(rel_EtoC2$time, rel_EtoC2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (NAF -> NAF + SAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### MES -> CHO (F -> FG) time_FtoG2
htime_FtoG2 <- hist(time_FtoG2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_FtoG2 <- htime_FtoG2$counts
htime_FtoG2 <- as.data.frame(htime_FtoG2)
htime_FtoG2$time <- seq(0, 49, 1)

rel_FtoG <- inner_join(htime_FtoG2, meanltt, by="time")
rel_FtoG2 <- rel_FtoG %>% mutate(relative= htime_FtoG2/mean)

png("Figure_relative_dispersal_MES_to_CHO.png", width = 480, height = 480)
plot(rel_FtoG2$time, rel_FtoG2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(MES -> MES + CHO / LTT)", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### CHO -> MES (G -> FG) time_GtoF2
htime_GtoF2 <- hist(time_GtoF2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_GtoF2 <- htime_GtoF2$counts
htime_GtoF2 <- as.data.frame(htime_GtoF2)
htime_GtoF2$time <- seq(0, 49, 1)

rel_GtoF <- inner_join(htime_GtoF2, meanltt, by="time")
rel_GtoF2 <- rel_GtoF %>% mutate(relative= htime_GtoF2/mean)

png("Figure_relative_dispersal_CHO_to_MES.png", width = 480, height = 480)
plot(rel_GtoF2$time, rel_GtoF2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(CHO -> CHO + MES) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### PAT -> WAM (I -> AI) time_ItoA2
htime_ItoA2 <- hist(time_ItoA2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_ItoA2 <- htime_ItoA2$counts
htime_ItoA2 <- as.data.frame(htime_ItoA2)
htime_ItoA2$time <- seq(0, 49, 1)

rel_ItoA <- inner_join(htime_ItoA2, meanltt, by="time")
rel_ItoA2 <- rel_ItoA %>% mutate(relative= htime_ItoA2/mean)

png("Figure_relative_dispersal_PAT_to_WAM.png", width = 480, height = 480)
plot(rel_ItoA2$time, rel_ItoA2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (PAT -> PAT + WAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### EAM -> WAM (D -> AD) time_DtoA2
htime_DtoA2 <- hist(time_DtoA2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_DtoA2 <- htime_DtoA2$counts
htime_DtoA2 <- as.data.frame(htime_DtoA2)
htime_DtoA2$time <- seq(0, 49, 1)

rel_DtoA <- inner_join(htime_DtoA2, meanltt, by="time")
rel_DtoA2 <- rel_DtoA %>% mutate(relative= htime_DtoA2/mean)

png("Figure_relative_dispersal_EAM_to_WAM.png", width = 480, height = 480)
plot(rel_DtoA2$time, rel_DtoA2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (EAM -> EAM + WAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM -> NAF (A -> E) time_AtoE2
htime_AtoE2 <- hist(time_AtoE2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AtoE2 <- htime_AtoE2$counts
htime_AtoE2 <- as.data.frame(htime_AtoE2)
htime_AtoE2$time <- seq(0, 49, 1)

rel_AtoE <- inner_join(htime_AtoE2, meanltt, by="time")
rel_AtoE2 <- rel_AtoE %>% mutate(relative= htime_AtoE2/mean)

png("Figure_relative_dispersal_WAM_to_NAF.png", width = 480, height = 480)
plot(rel_AtoE2$time, rel_AtoE2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(WAM -> WAM + NAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### EAM -> NAF (D -> DE) time_DtoE2
htime_DtoE2 <- hist(time_DtoE2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_DtoE2 <- htime_DtoE2$counts
htime_DtoE2 <- as.data.frame(htime_DtoE2)
htime_DtoE2$time <- seq(0, 49, 1)

rel_DtoE <- inner_join(htime_DtoE2, meanltt, by="time")
rel_DtoE2 <- rel_DtoE %>% mutate(relative= htime_DtoE2/mean)

png("Figure_relative_dispersal_EAM_to_NAF.png", width = 480, height = 480)
plot(rel_DtoE2$time, rel_DtoE2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "(EAM -> EAM + NAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### WAM | EAM (A | D) time_AD2 (from script 17)

htime_AD2 <- hist(time_AD2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AD2 <- htime_AD2$counts
htime_AD2 <- as.data.frame(htime_AD2)
htime_AD2$time <- seq(0, 49, 1)

rel_AD <- inner_join(htime_AD2, meanltt, by="time")
rel_AD2 <- rel_AD %>% mutate(relative= htime_AD2/mean)

png("Figure_relative_allopatry_WAM_&_EAM.png", width = 480, height = 480)
plot(rel_AD2$time, rel_AD2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Allopatry (WAM + EAM -> WAM | EAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


###WAM | PAT (A | I) time_AI2 (from script 17)
htime_AI2 <- hist(time_AI2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AI2 <- htime_AI2$counts
htime_AI2 <- as.data.frame(htime_AI2)
htime_AI2$time <- seq(0, 49, 1)

rel_AI <- inner_join(htime_AI2, meanltt, by="time")
rel_AI2 <- rel_AI %>% mutate(relative= htime_AI2/mean)

png("Figure_relative_allopatry_WAM_&_PAT.png", width = 480, height = 480)
plot(rel_AI2$time, rel_AI2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Allopatry (WAM + PAT -> WAM | PAT) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

### CHO to WAM (G -> A) time_GtoA2 (from script 15)
htime_GtoA2 <- hist(time_GtoA2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_GtoA2 <- htime_GtoA2$counts
htime_GtoA2 <- as.data.frame(htime_GtoA2)
htime_GtoA2$time <- seq(0, 49, 1)

rel_GtoA <- inner_join(htime_GtoA2, meanltt, by="time")
rel_GtoA2 <- rel_GtoA %>% mutate(relative= htime_GtoA2/mean)

png("Figure_relative_dispersal_CHO_to_WAM.png", width = 480, height = 480)
plot(rel_GtoA2$time, rel_GtoA2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (CHO -> CHO + WAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


### WAM | CHO (A|G) time_AtoGvic2 (from script 17)
time_AG2 <- time_AtoGvic2

htime_AG2 <- hist(time_AG2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htime_AG2 <- htime_AG2$counts
htime_AG2 <- as.data.frame(htime_AG2)
htime_AG2$time <- seq(0, 49, 1)

rel_AG <- inner_join(htime_AG2, meanltt, by="time")
rel_AG2 <- rel_AG %>% mutate(relative= htime_AG2/mean)

png("Figure_relative_allopatry_WAM_&_CHO.png", width = 480, height = 480)
plot(rel_AG2$time, rel_AG2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Allopatry (WAM + CHO -> WAM | CHO) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()