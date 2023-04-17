library(strucchange)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")


## Breaking points for the routes.

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

png("Figure_relative_disp_AtoD_BP.png", width = 480, height = 480)
plot(rel_AtoD2$time, rel_AtoD2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + EAM) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
abline(v = c(12,18), lwd=0.5,lty=1)
lines(rel_AtoD2$time, ts(fitted(mbp2_AtoD),start=1),lwd=2,lty=4, col = "gray50")
#lines(loess.smooth(rel_AtoD2$time, rel_AtoD2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()


### WAM -> WAM + PAT (A -> AI) rel_AtoI2 (script 16)
#result: 2 breaks (20 and 28)

rel_AtoI3 <- as.data.frame(rel_AtoI2$relative)
ts_rel_AtoI3 <- ts(rel_AtoI3)
plot(ts_rel_AtoI3, xlim=c(50,0))

bps_AtoI <-breakpoints(ts_rel_AtoI3~1)
summary(bps_AtoI)

mbp0_AtoI <- lm(rel_AtoI2$relative~1)
mbp1_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=1))
mbp2_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=2))
mbp3_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=3))
mbp4_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=4))
mbp5_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=5))
mbp6_AtoI <- lm(rel_AtoI2$relative~breakfactor(bps_AtoI,breaks=6))
anova(mbp0_AtoI, mbp1_AtoI)
anova(mbp0_AtoI,mbp2_AtoI)
anova(mbp2_AtoI,mbp3_AtoI)
anova(mbp2_AtoI,mbp4_AtoI)
anova(mbp2_AtoI,mbp5_AtoI)
anova(mbp2_AtoI,mbp6_AtoI)

png("Figure_relative_disp_AtoI_BP.png", width = 480, height = 480)
plot(rel_AtoI2$time, rel_AtoI2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + PAT) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
abline(v = c(20,28), lwd=0.5,lty=1)
lines(rel_AtoI2$time, ts(fitted(mbp2_AtoI),start=1),lwd=2,lty=4, col = "gray50")
#lines(loess.smooth(rel_AtoI2$time, rel_AtoI2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()


### WAM -> WAM + CHO (A -> AG) rel_AtoG2 (script 16)
#result: 3 breaks (6, 21 and 27)

rel_AtoG3 <- as.data.frame(rel_AtoG2$relative)
ts_rel_AtoG3 <- ts(rel_AtoG3)
plot(ts_rel_AtoG3, xlim=c(50,0))

bps_AtoG <-breakpoints(ts_rel_AtoG3~1)
summary(bps_AtoG)

mbp0_AtoG <- lm(rel_AtoG2$relative~1)
mbp1_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=1))
mbp2_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=2))
mbp3_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=3))
mbp4_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=4))
mbp5_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=5))
mbp6_AtoG <- lm(rel_AtoG2$relative~breakfactor(bps_AtoG,breaks=6))
anova(mbp0_AtoG,mbp1_AtoG)
anova(mbp1_AtoG,mbp2_AtoG)
anova(mbp2_AtoG,mbp3_AtoG)
anova(mbp3_AtoG,mbp4_AtoG)
anova(mbp3_AtoG,mbp5_AtoG)
anova(mbp2_AtoG,mbp6_AtoG)

png("Figure_relative_disp_AtoG_BP.png", width = 480, height = 480)
plot(rel_AtoG2$time, rel_AtoG2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + CHO) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
abline(v = c(6,21,27), lwd=0.5,lty=1)
lines(rel_AtoG2$time, ts(fitted(mbp3_AtoG),start=1),lwd=2,lty=4, col = "gray50")
#lines(loess.smooth(rel_AtoI2$time, rel_AtoI2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()

### WAM -> WAM + SAF (A -> AC) rel_AtoC2 (script 16)
#result: 3 breaks (7, 13, 19)

rel_AtoC3 <- as.data.frame(rel_AtoC2$relative)
ts_rel_AtoC3 <- ts(rel_AtoC3)
plot(ts_rel_AtoC3, xlim=c(50,0))

bps_AtoC <-breakpoints(ts_rel_AtoC3~1)
summary(bps_AtoC)

mbp0_AtoC <- lm(rel_AtoC2$relative~1)
mbp1_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=1))
mbp2_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=2))
mbp3_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=3))
mbp4_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=4))
mbp5_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=5))
mbp6_AtoC <- lm(rel_AtoC2$relative~breakfactor(bps_AtoC,breaks=6))
anova(mbp0_AtoC,mbp1_AtoC)
anova(mbp1_AtoC,mbp2_AtoC)
anova(mbp2_AtoC,mbp3_AtoC)
anova(mbp3_AtoC,mbp4_AtoC)
anova(mbp3_AtoC,mbp5_AtoC)
anova(mbp2_AtoC,mbp6_AtoC)

png("Figure_relative_disp_AtoC_BP_vert.png", width = 480, height = 480)
plot(rel_AtoC2$time, rel_AtoC2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (WAM -> WAM + SAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
abline(v = c(7,13,19), lwd=0.5,lty=1)
lines(rel_AtoC2$time, ts(fitted(mbp3_AtoC),start=1),lwd=2,lty=4, col = "gray50")
#lines(loess.smooth(rel_AtoI2$time, rel_AtoI2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()

### SAF -> SAF + NAF (C -> CE) rel_CtoE2 (script 16)
#result: 1 break (6)

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
anova(mbp0_CtoE,mbp1_CtoE)
anova(mbp1_CtoE,mbp2_CtoE)

png("Figure_relative_disp_CtoE_BP.png", width = 480, height = 480)
plot(rel_CtoE2$time, rel_CtoE2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,8),
     main = "Dispersal (SAF -> SAF + NAF) / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
abline(v = 6, lwd=0.5,lty=1)
lines(rel_CtoE2$time, ts(fitted(mbp3_CtoE),start=1),lwd=2,lty=4, col = "gray50")
#lines(loess.smooth(rel_AtoI2$time, rel_AtoI2$relative, span = 1/6),lty=1, lwd=3, col = "gray70")
dev.off()