library(dplyr)
library(ggplot2)

#Allopatric speciations through time absolute values

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/")

#open table with results
tb_time <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",", dec = ".")

#Filter table for allopatric speciation from AG -> G WAM | CHO
time_AtoGvic <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% 
  filter(transitionCladog == "AG -> A" | transitionCladog == "AG -> G")

time_AtoGvic2 <- transform(time_AtoGvic, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_WAM_CHO.png", width = 480, height = 480)
hist(time_AtoGvic2$transitionTime, main = "Allopatry WAM + CHO -> WAM | CHO", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram vicariance WAM | EAM

#Filter table for Vicariance WAM | EAM
time_AD <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AD -> A" | transitionCladog == "AD -> D")

time_AD2 <- transform(time_AD, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_WAM_EAM.png", width = 480, height = 480)
hist(time_AD2$transitionTime, main = "Allopatry WAM + EAM -> WAM | EAM", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram vicariance WAM | PAT

#Filter table for Vicariance WAM | PAT
time_AI <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AI -> A" | transitionCladog == "AI -> I")

time_AI2 <- transform(time_AI, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_WAM_PAT.png", width = 480, height = 480)
hist(time_AI2$transitionTime, main = "Allopatry WAM + PAT -> WAM | PAT", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram vicariance WAM | CHO

#Filter table for Vicariance WAM | CHO
time_AG <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AG -> A" | transitionCladog == "AG -> G")

time_AG2 <- transform(time_AG, transitionTime = as.numeric(transitionTime))


#histogram vicariance WAM | CHA

#Filter table for Vicariance WAM | CHA
time_AB <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AB -> A" | transitionCladog == "AB -> B")

time_AB2 <- transform(time_AB, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_WAM_CHA2.png", width = 480, height = 480)
hist(time_AB2$transitionTime, main = "(WAM + CHA) -> WAM | CHA", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram vicariance NAF | SAF

#Filter table for Vicariance NAF | SAF
time_EC <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "CE -> C" | transitionCladog == "CE -> E")

time_EC2 <- transform(time_EC, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_NAF_SAF.png", width = 480, height = 480)
hist(time_EC2$transitionTime, main = "Allopatry SAF + NAF -> SAF | NAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#Filter table for Vicariance WAM | SAF
time_AC <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AC -> C" | transitionCladog == "CE -> E")

time_AC2 <- transform(time_AC, transitionTime = as.numeric(transitionTime))

#histogram
png("Figure_allopatry_WAM_SAF.png", width = 480, height = 480)
hist(time_AC2$transitionTime, main = "Allopatry (WAM + SAF) -> WAM | SAF", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()