###Figures of the routes (REVIEW process) April 2023

library(dplyr)
library(ggplot2)
library(strucchange)

#folder to export figures
setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/doc/2023 - JBI Full Review/new_figures")

#open table with results
tb_time <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",", dec = ".")

#table with absolut dispersals (from script 14)
absolut

#table with the ltt (from script 11)
df5
meanltt <- as.data.frame(df5)
meanltt2 <- meanltt %>% mutate(time = time*-1)

relative <- inner_join(absolut, meanltt2, by="time")
relative2 <- relative %>% mutate(relative= absolut/mean)


#Amazonia
## WAM -> WAM + EAM

#absolute number of dispersals
#Filter table for dispersal from A -> D
time_AtoD <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AD")

time_AtoD2 <- transform(time_AtoD, transitionTime = as.numeric(transitionTime))

#histogram
png("WAM_to_EAM_absolute_dispersal.png", width = 480, height = 480)
hist(time_AtoD2$transitionTime, main = "Dispersal WAM -> WAM + EAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()



#Rate graph
htimeAtoD <- hist(time_AtoD2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoD2 <- htimeAtoD$counts
htimeAtoD2 <- as.data.frame(htimeAtoD2)
htimeAtoD2$time <- seq(0, 49, 1)

rel_AtoD <- inner_join(htimeAtoD2, meanltt2, by="time")
rel_AtoD2 <- rel_AtoD %>% mutate(relative= htimeAtoD2/mean)


## Breking points for the routes.
### WAM -> WAM + EAM (A -> AD) rel_AtoD2 (script 16)
#result: 2 breaks (12 e 18)

rel_AtoD3 <- as.data.frame(rel_AtoD2$relative)
ts_rel_AtoD3 <- ts(rel_AtoD3)
plot(ts_rel_AtoD3, xlim=c(50,0))

bps_AtoD <-breakpoints(ts_rel_AtoD3~1)
summary(bps_AtoD)

mbp0_AtoD <- lm(rel_AtoD2$relative~1)
mbp1_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=1))
mbp2_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=2))
mbp3_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=3))
mbp4_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=4))
mbp5_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=5))
mbp6_AtoD <- lm(rel_AtoD2$relative~breakfactor(bps_AtoD,breaks=6))
anova(mbp0_AtoD, mbp1_AtoD)
anova(mbp1_AtoD,mbp2_AtoD)
anova(mbp2_AtoD,mbp3_AtoD)
anova(mbp2_AtoD,mbp4_AtoD)
anova(mbp2_AtoD,mbp5_AtoD)
anova(mbp2_AtoD,mbp6_AtoD)

png("WAM_to_EAM_rate_break.png", width = 480, height = 480)
plot(rel_AtoD2$time, rel_AtoD2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + EAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(12,18), lwd=0.5,lty=1)
lines(rel_AtoD2$time, ts(fitted(mbp2_AtoD),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

###Boxplot
summary(time_AD2$transitionTime)
boxplot(time_AD2$transitionTime, horizontal = TRUE)
time_AtoD2 %>% 
  ggplot(aes(x = transitionAnag, y = transitionTime)) + geom_boxplot() + coord_flip() + 
  theme_classic() + scale_y_reverse() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank()) +
  ylim(50,0)

#Amazonia
## EAM -> EAM + WAM

#absolute number of dispersals
#Filter table for dispersal from D -> AD
time_DtoA <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "D -> AD")

time_DtoA2 <- transform(time_DtoA, transitionTime = as.numeric(transitionTime))
summary(time_DtoA2)
#histogram
png("EAM_to_WAM_absolute_dispersal.png", width = 480, height = 480)
hist(time_DtoA2$transitionTime, main = "Dispersal EAM -> EAM + WAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeDtoA <- hist(time_DtoA2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeDtoA2 <- htimeDtoA$counts
htimeDtoA2 <- as.data.frame(htimeDtoA2)
htimeDtoA2$time <- seq(0, 49, 1)

rel_DtoA <- inner_join(htimeDtoA2, meanltt2, by="time")
rel_DtoA2 <- rel_DtoA %>% mutate(relative= htimeDtoA2/mean)


## Breaking points for the routes.
### EAM -> EAM + WAM + EAM (D -> AD) rel_AtoD2 (script 16)
#result: 

rel_DtoA3 <- as.data.frame(rel_DtoA2$relative)
ts_rel_DtoA3 <- ts(rel_DtoA3)
plot(ts_rel_DtoA3, xlim=c(50,0))

bps_DtoA <-breakpoints(ts_rel_DtoA3~1)
summary(bps_DtoA)

mbp0_DtoA <- lm(rel_DtoA2$relative~1)
mbp1_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=1))
mbp2_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=2))
mbp3_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=3))
mbp4_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=4))
mbp5_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=5))
mbp6_DtoA <- lm(rel_DtoA2$relative~breakfactor(bps_DtoA,breaks=6))
anova(mbp0_DtoA, mbp1_DtoA)
anova(mbp1_DtoA,mbp2_DtoA)
anova(mbp2_DtoA,mbp3_DtoA)
anova(mbp2_DtoA,mbp4_DtoA)
anova(mbp2_DtoA,mbp5_DtoA)
anova(mbp2_DtoA,mbp6_DtoA)

png("EAM_to_WAM_rate_break.png", width = 480, height = 480)
plot(rel_DtoA2$time, rel_DtoA2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (EAM -> EAM + WAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(9), lwd=0.5,lty=1)
lines(rel_DtoA2$time, ts(fitted(mbp1_DtoA),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

#Caribbean
## WAM -> WAM + CAR

#absolute number of dispersals
#Filter table for dispersal from A -> J
time_AtoJ <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AJ")

time_AtoJ2 <- transform(time_AtoJ, transitionTime = as.numeric(transitionTime))
summary(time_AtoJ2)
#histogram
png("WAM_to_CAR_absolute_dispersal.png", width = 480, height = 480)
hist(time_AtoJ2$transitionTime, main = "Dispersal WAM -> WAM + CAR", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeAtoJ <- hist(time_AtoJ2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoJ2 <- htimeAtoJ$counts
htimeAtoJ2 <- as.data.frame(htimeAtoJ2)
htimeAtoJ2$time <- seq(0, 49, 1)

rel_AtoJ <- inner_join(htimeAtoJ2, meanltt2, by="time")
rel_AtoJ2 <- rel_AtoJ %>% mutate(relative= htimeAtoJ2/mean)


## Breking points for the routes.

rel_AtoJ3 <- as.data.frame(rel_AtoJ2$relative)
ts_rel_AtoJ3 <- ts(rel_AtoJ3)
plot(ts_rel_AtoJ3, xlim=c(50,0))

bps_AtoJ <-breakpoints(ts_rel_AtoJ3~1)
summary(bps_AtoJ)

mbp0_AtoJ <- lm(rel_AtoJ2$relative~1)
mbp1_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=1))
mbp2_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=2))
mbp3_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=3))
mbp4_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=4))
mbp5_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=5))
mbp6_AtoJ <- lm(rel_AtoJ2$relative~breakfactor(bps_AtoJ,breaks=6))
anova(mbp0_AtoJ, mbp1_AtoJ)
anova(mbp1_AtoJ,mbp2_AtoJ)
anova(mbp2_AtoJ,mbp3_AtoJ)
anova(mbp2_AtoJ,mbp4_AtoJ)
anova(mbp2_AtoJ,mbp5_AtoJ)
anova(mbp2_AtoJ,mbp6_AtoJ)

png("WAM_to_CAR_rate_break.png", width = 480, height = 480)
plot(rel_AtoJ2$time, rel_AtoJ2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + CAR) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(12,18), lwd=0.5,lty=1)
#lines(rel_AtoD2$time, ts(fitted(mbp2_AtoD),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

#Chacoan
## WAM -> WAM + CHA

#absolute number of dispersals
#Filter table for dispersal from A -> B
time_AtoB <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AB")

time_AtoB2 <- transform(time_AtoB, transitionTime = as.numeric(transitionTime))
summary(time_AtoB2)
#histogram
png("WAM_to_CHA_absolute_dispersal.png", width = 480, height = 480)
hist(time_AtoB2$transitionTime, main = "Dispersal WAM -> WAM + CHA", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeAtoB <- hist(time_AtoB2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoB2 <- htimeAtoB$counts
htimeAtoB2 <- as.data.frame(htimeAtoB2)
htimeAtoB2$time <- seq(0, 49, 1)

rel_AtoB <- inner_join(htimeAtoB2, meanltt2, by="time")
rel_AtoB2 <- rel_AtoB %>% mutate(relative= htimeAtoB2/mean)


## Breking points for the routes.

rel_AtoB3 <- as.data.frame(rel_AtoB2$relative)
ts_rel_AtoB3 <- ts(rel_AtoB3)
plot(ts_rel_AtoB3, xlim=c(50,0))

bps_AtoB <-breakpoints(ts_rel_AtoB3~1)
summary(bps_AtoB)

mbp0_AtoB <- lm(rel_AtoB2$relative~1)
mbp1_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=1))
mbp2_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=2))
mbp3_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=3))
mbp4_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=4))
mbp5_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=5))
mbp6_AtoB <- lm(rel_AtoB2$relative~breakfactor(bps_AtoB,breaks=6))
anova(mbp0_AtoB, mbp1_AtoB)
anova(mbp1_AtoB,mbp2_AtoB)
anova(mbp2_AtoB,mbp3_AtoB)
anova(mbp3_AtoB,mbp4_AtoB)
anova(mbp3_AtoB,mbp5_AtoB)
anova(mbp3_AtoB,mbp6_AtoB)

png("WAM_to_CHA_rate_break.png", width = 480, height = 480)
plot(rel_AtoB2$time, rel_AtoB2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + CHA) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(6,13,20), lwd=0.5,lty=1)
lines(rel_AtoB2$time, ts(fitted(mbp3_AtoB),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

#Chacoan
## SAF -> SAF + CHA

#absolute number of dispersals
#Filter table for dispersal from C -> BC
time_CtoB <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> BC")

time_CtoB2 <- transform(time_CtoB, transitionTime = as.numeric(transitionTime))
summary(time_CtoB2)
#histogram
png("SAF_to_CHA_absolute_dispersal.png", width = 480, height = 480)
hist(time_CtoB2$transitionTime, main = "Dispersal SAF -> SAF + CHA", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeCtoB <- hist(time_CtoB2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeCtoB2 <- htimeCtoB$counts
htimeCtoB2 <- as.data.frame(htimeCtoB2)
htimeCtoB2$time <- seq(0, 49, 1)

rel_CtoB <- inner_join(htimeCtoB2, meanltt2, by="time")
rel_CtoB2 <- rel_CtoB %>% mutate(relative= htimeCtoB2/mean)


## Breking points for the routes.

rel_CtoB3 <- as.data.frame(rel_CtoB2$relative)
ts_rel_CtoB3 <- ts(rel_CtoB3)
plot(ts_rel_CtoB3, xlim=c(50,0))

bps_CtoB <-breakpoints(ts_rel_CtoB3~1)
summary(bps_CtoB)

mbp0_CtoB <- lm(rel_CtoB2$relative~1)
mbp1_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=1))
mbp2_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=2))
mbp3_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=3))
mbp4_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=4))
mbp5_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=5))
mbp6_CtoB <- lm(rel_CtoB2$relative~breakfactor(bps_CtoB,breaks=6))
anova(mbp0_CtoB, mbp1_CtoB)
anova(mbp1_CtoB,mbp2_CtoB)
anova(mbp2_CtoB,mbp3_CtoB)
anova(mbp3_CtoB,mbp4_CtoB)
anova(mbp3_CtoB,mbp5_CtoB)
anova(mbp3_CtoB,mbp6_CtoB)

png("SAF_to_CHA_rate_break.png", width = 480, height = 480)
plot(rel_CtoB2$time, rel_CtoB2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (SAF -> SAF + CHA) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(6), lwd=0.5,lty=1)
lines(rel_CtoB2$time, ts(fitted(mbp1_CtoB),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

#Mesoamerica
## WAM -> WAM + MES

#absolute number of dispersals
#Filter table for dispersal from A -> F
time_AtoF <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AF")

time_AtoF2 <- transform(time_AtoF, transitionTime = as.numeric(transitionTime))
summary(time_AtoF2)

#histogram
png("WAM_to_MES_absolute_dispersal.png", width = 480, height = 480)
hist(time_AtoF2$transitionTime, main = "Dispersal WAM -> WAM + MES", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeAtoF <- hist(time_AtoF2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeAtoF2 <- htimeAtoF$counts
htimeAtoF2 <- as.data.frame(htimeAtoF2)
htimeAtoF2$time <- seq(0, 49, 1)

rel_AtoF <- inner_join(htimeAtoF2, meanltt2, by="time")
rel_AtoF2 <- rel_AtoF %>% mutate(relative= htimeAtoF2/mean)


## Breking points for the routes.

rel_AtoF3 <- as.data.frame(rel_AtoF2$relative)
ts_rel_AtoF3 <- ts(rel_AtoF3)
plot(ts_rel_AtoF3, xlim=c(50,0))

bps_AtoF <-breakpoints(ts_rel_AtoF3~1)
summary(bps_AtoF)

mbp0_AtoF <- lm(rel_AtoF2$relative~1)
mbp1_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=1))
mbp2_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=2))
mbp3_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=3))
mbp4_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=4))
mbp5_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=5))
mbp6_AtoF <- lm(rel_AtoF2$relative~breakfactor(bps_AtoF,breaks=6))
anova(mbp0_AtoF, mbp1_AtoF)
anova(mbp1_AtoF,mbp2_AtoF)
anova(mbp2_AtoF,mbp3_AtoF)
anova(mbp3_AtoF,mbp4_AtoF)
anova(mbp3_AtoF,mbp5_AtoF)
anova(mbp3_AtoF,mbp6_AtoF)

png("WAM_to_MES_rate_break.png", width = 480, height = 480)
plot(rel_AtoF2$time, rel_AtoF2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + MES) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(10,24), lwd=0.5,lty=1)
lines(rel_AtoF2$time, ts(fitted(mbp2_AtoF),start=1),lwd=2,lty=4, col = "gray50")
dev.off()

#Atlantic Forest
## SAF -> SAF + NAF

#absolute number of dispersals
#Filter table for dispersal from C -> CE
time_CtoE <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> CE")

time_CtoE2 <- transform(time_CtoE, transitionTime = as.numeric(transitionTime))
summary(time_CtoE2)
#histogram
png("SAF_to_NAF_absolute_dispersal.png", width = 480, height = 480)
hist(time_CtoE2$transitionTime, main = "Dispersal SAF -> SAF + NAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Rate graph
htimeCtoE <- hist(time_CtoE2$transitionTime, breaks = seq(50,0,-1), xlim = c(50,0))
htimeCtoE2 <- htimeCtoE$counts
htimeCtoE2 <- as.data.frame(htimeCtoE2)
htimeCtoE2$time <- seq(0, 49, 1)

rel_CtoE <- inner_join(htimeCtoE2, meanltt2, by="time")
rel_CtoE2 <- rel_CtoE %>% mutate(relative= htimeCtoE2/mean)


## Breking points for the routes.

rel_CtoE3 <- as.data.frame(rel_CtoE2$relative)
ts_rel_CtoE3 <- ts(rel_CtoE3)
plot(ts_rel_CtoE3, xlim=c(50,0))

bps_CtoE <-breakpoints(ts_rel_CtoE3~1)
summary(bps_CtoE)

mbp0_CtoE <- lm(rel_CtoE2$relative~1)
mbp1_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=1))
mbp2_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=2))
mbp3_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=3))
mbp4_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=4))
mbp5_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=5))
mbp6_CtoE <- lm(rel_CtoE2$relative~breakfactor(bps_CtoE,breaks=6))
anova(mbp0_CtoE, mbp1_CtoE)
anova(mbp1_CtoE,mbp2_CtoE)
anova(mbp2_CtoE,mbp3_CtoE)
anova(mbp3_CtoE,mbp4_CtoE)
anova(mbp3_CtoE,mbp5_CtoE)
anova(mbp3_CtoE,mbp6_CtoE)

png("SAF_to_NAF_rate_break.png", width = 480, height = 480)
plot(rel_CtoE2$time, rel_CtoE2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (SAF -> SAF + NAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(6), lwd=0.5,lty=1)
lines(rel_CtoE2$time, ts(fitted(mbp1_CtoE),start=1),lwd=2,lty=4, col = "gray50")
dev.off()
