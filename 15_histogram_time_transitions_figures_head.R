library(dplyr)
library(ggplot2)

#Dispersal and Switch-ranges through time absolute values

#Histogram dispersal through time from WAM to EAM (code A -> AD)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")

#open table with results
tb_time <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",", dec = ".")

#Filter table for dispersal from A -> D
time_AtoD <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AD")

time_AtoD2 <- transform(time_AtoD, transitionTime = as.numeric(transitionTime))

#histogram (ggplot2 histogram showed a bin location issue)
#ggplot(time_AtoD2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 1) +
#  theme_minimal()+
# xlab("Time") +
#  ylab("Dispersals from WAM to EAM") +
#  scale_x_reverse()+ xlim(c(50,0)) +
#  ylim(c(0,125))

#histogram
png("Figure_freq_dispersal_events_from_WAM_to_EAM.png", width = 480, height = 480)
hist(time_AtoD2$transitionTime, main = "Dispersal WAM -> WAM + EAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA, 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_EAM.png",  
 #      plot = last_plot(),  device = "png",  path = NULL,
  #     scale = 1,  width = 1000,  height = 1000,  units = "px",  
   #    dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to EAM (code A -> D)

#Filter table for switch-range from A -> D
time_AtoDsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> D")

time_AtoDsr2 <- transform(time_AtoDsr, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_switch_range_WAM_to_EAM.png", width = 480, height = 480)
hist(time_AtoDsr2$transitionTime, main = "Switch-range WAM -> EAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram
#ggplot(time_AtoDsr2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 0.5, boundary = 0) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Switch-range from WAM to EAM") +
#  scale_x_reverse() + xlim(c(50,0)) +
#  ylim(c(0,100))
  
#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_switch_range_from_WAM_to_EAM.png",  
#       plot = last_plot(),  device = "png",  path = NULL,
#       scale = 1,  width = 1000,  height = 1000,  units = "px",  
#       dpi = 320,  limitsize = TRUE,  bg = NULL)


#Histogram dispersal through time from WAM to PAT (code A -> AI)

#Filter table for dispersal from A -> AI
time_AtoI <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AI")

time_AtoI2 <- transform(time_AtoI, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_PAT.png", width = 480, height = 480)
hist(time_AtoI2$transitionTime, main = "Dispersal WAM -> WAM + PAT", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#histogram
#ggplot(time_AtoI2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 0.5, boundary = 0) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Dispersals from WAM to PAT") +
#  scale_x_reverse()+ xlim(c(50,0)) +
#  ylim(c(0,100))

#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_PAT.png",  
#       plot = last_plot(),  device = "png",  path = NULL,
#       scale = 1,  width = 1000,  height = 1000,  units = "px",  
#       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to PAT (code A -> AI)
#no events

#Histogram dispersal through time from WAM to CHO (code A -> AG)

#Filter table for dispersal from A -> AG
time_AtoG <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AG")

time_AtoG2 <- transform(time_AtoG, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_CHO.png", width = 480, height = 480)
hist(time_AtoG2$transitionTime, main = "Dispersal WAM -> WAM + CHO", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#histogram
#ggplot(time_AtoG2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 0.5, boundary = 0) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Dispersals from WAM to CHO") +
#  scale_x_reverse()+ xlim(c(50,0)) +
#  ylim(c(0,100))

#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_CHO.png",  
#       plot = last_plot(),  device = "png",  path = NULL,
#       scale = 1,  width = 1000,  height = 1000,  units = "px",  
#       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to SAF (code A -> C)

#Filter table for switch-range from A -> C
time_AtoCsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> C")

time_AtoCsr2 <- transform(time_AtoCsr, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_switch_range_from_WAM_to_SAF2.png", width = 480, height = 480)
hist(time_AtoCsr2$transitionTime, main = "Switch-range WAM -> SAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#histogram
#ggplot(time_AtoCsr2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 1, center = -0.5) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Switch-range from WAM to SAF") +
#  scale_x_reverse() + xlim(c(50,0)) +
#  ylim(c(0,100))

#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_switch_range_from_WAM_to_SAF.png",  
#       plot = last_plot(),  device = "png",  path = NULL,
#       scale = 1,  width = 1000,  height = 1000,  units = "px",  
#       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to CAR (code A -> J)

#Filter table for switch-range from A -> J
time_AtoJsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> J")

time_AtoJsr2 <- transform(time_AtoJsr, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_switch_range_from_WAM_to_CAR2.png", width = 480, height = 480)
hist(time_AtoJsr2$transitionTime, main = "Switch-range WAM -> CAR", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#histogram
#ggplot(time_AtoJsr2, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 1) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Switch-range from WAM to CAR") +
#  scale_x_reverse() + xlim(c(50,0)) +
#  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
#ggsave(filename = "Figure_freq_switch_range_from_WAM_to_CAR.png",  
#       plot = last_plot(),  device = "png",  path = NULL,
#       scale = 1,  width = 1000,  height = 1000,  units = "px",  
#       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram dispersal through time from WAM to CHA (code A -> B)

#Filter table for dispersal from A -> B
time_AtoB <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AB")

time_AtoB2 <- transform(time_AtoB, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_CHA.png", width = 480, height = 480)
hist(time_AtoB2$transitionTime, main = "WAM -> WAM + CHA", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Histogram dispersal through time from SAF to CHA (code C -> B)

#Filter table for dispersal from C -> B
time_CtoB <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> BC")

time_CtoB2 <- transform(time_CtoB, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_SAF_to_CHA.png", width = 480, height = 480)
hist(time_CtoB2$transitionTime, main = "SAF -> SAF + CHA", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Filter table for dispersal from A -> C
time_AtoC <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime) %>% 
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AC")

time_AtoC2 <- transform(time_AtoC, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_SAF.png", width = 480, height = 480)
hist(time_AtoC2$transitionTime, main = "Dispersal WAM -> WAM + SAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from A -> AJ
time_AtoJ <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AJ")

time_AtoJ2 <- transform(time_AtoJ, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_CAR.png", width = 480, height = 480)
hist(time_AtoJ2$transitionTime, main = "WAM -> WAM + CAR", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from A -> AF (WAM -> MES)
time_AtoF <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AF")

time_AtoF2 <- transform(time_AtoF, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_MES.png", width = 480, height = 480)
hist(time_AtoF2$transitionTime, main = "WAM -> WAM + MES", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from A -> AH (WAM -> DNO)
time_AtoH <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AH")

time_AtoH2 <- transform(time_AtoH, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_DNO.png", width = 480, height = 480)
hist(time_AtoH2$transitionTime, main = "WAM -> WAM + DNO", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from C -> CE (SAF -> NAF)
time_CtoE <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> CE")

time_CtoE2 <- transform(time_CtoE, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_SAF_to_NAF.png", width = 480, height = 480)
hist(time_CtoE2$transitionTime, main = "Dispersal SAF -> SAF + NAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from E -> EC (NAF -> SAF)
time_EtoC <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "E -> CE")

time_EtoC2 <- transform(time_EtoC, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_NAF_to_SAF.png", width = 480, height = 480)
hist(time_EtoC2$transitionTime, main = "Dispersal NAF -> NAF + SAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from F -> FG (MES -> CHO)
time_FtoG <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "F -> FG")

time_FtoG2 <- transform(time_FtoG, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_MES_to_CHO.png", width = 480, height = 480)
hist(time_FtoG2$transitionTime, main = "MES -> MES + CHO", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from G -> FG (CHO -> MES)
time_GtoF <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "G -> FG")

time_GtoF2 <- transform(time_GtoF, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_CHO_to_MES.png", width = 480, height = 480)
hist(time_GtoF2$transitionTime, main = "CHO -> CHO + MES", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from I -> AI (PAT -> WAM)
time_ItoA <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "I -> AI")

time_ItoA2 <- transform(time_ItoA, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_PAT_to_WAM.png", width = 480, height = 480)
hist(time_ItoA2$transitionTime, main = "Dispersal PAT -> PAT + WAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from D -> AD (EAM -> WAM)
time_DtoA <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "D -> AD")

time_DtoA2 <- transform(time_DtoA, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_EAM_to_WAM.png", width = 480, height = 480)
hist(time_DtoA2$transitionTime, main = "Dispersal EAM -> EAM + WAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from A -> AE (WAM -> NAF)
time_AtoE <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AE")

time_AtoE2 <- transform(time_AtoE, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_WAM_to_NAF.png", width = 480, height = 480)
hist(time_AtoE2$transitionTime, main = "WAM -> WAM + NAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from D -> DE (EAM -> NAF)
time_DtoE <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "D -> DE")

time_DtoE2 <- transform(time_DtoE, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_EAM_to_NAF.png", width = 480, height = 480)
hist(time_DtoE2$transitionTime, main = "EAM -> EAM + NAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from D -> EF (EAM -> MES)
time_DtoF <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "D -> EF")

time_DtoF2 <- transform(time_DtoF, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_EAM_to_MES.png", width = 480, height = 480)
hist(time_DtoF2$transitionTime, main = "EAM -> EAM + MES", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from G -> AG (CHO -> WAM)
time_GtoA <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "G -> AG")

time_GtoA2 <- transform(time_GtoA, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_CHO_to_WAM.png", width = 480, height = 480)
hist(time_GtoF2$transitionTime, main = "Dispersal CHO -> CHO + WAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

##Filter table for dispersal from C -> AC (SAF -> WAM)
time_CtoA <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> AC")

time_CtoA2 <- transform(time_CtoA, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_dispersal_from_SAF_to_WAM.png", width = 480, height = 480)
hist(time_CtoA2$transitionTime, main = "Dispersal SAF -> SAF + WAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()
