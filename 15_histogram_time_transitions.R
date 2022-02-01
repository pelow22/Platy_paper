library(dplyr)
library(ggplot2)

#Dispersal and Switch-ranges through time absolute values

#Histogram dispersal through time from WAM to EAM (code A -> AD)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/output/DEC_M1")

#open table with results
tb_time <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",", dec = ".")

#Filter table for dispersal from A -> D
time_AtoD <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AD")

time_AtoD2 <- transform(time_AtoD, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoD2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Dispersals from WAM to EAM") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_EAM.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)


#Histogram switch-range through time from WAM to EAM (code A -> D)

#Filter table for switch-range from A -> D
time_AtoDsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> D")

time_AtoDsr2 <- transform(time_AtoDsr, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoDsr2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Switch-range from WAM to EAM") +
  scale_x_reverse() + xlim(c(50,0)) +
  ylim(c(0,125))
  
#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_switch_range_from_WAM_to_EAM.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)


#Histogram dispersal through time from WAM to PAT (code A -> AI)

#Filter table for dispersal from A -> AI
time_AtoI <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AI")

time_AtoI2 <- transform(time_AtoI, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoI2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Dispersals from WAM to PAT") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_PAT.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to PAT (code A -> AI)
#no events

#Histogram dispersal through time from WAM to CHO (code A -> AG)

#Filter table for dispersal from A -> AI
time_AtoG <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AG")

time_AtoG2 <- transform(time_AtoG, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoG2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Dispersals from WAM to CHO") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_dispersal_events_from_WAM_to_CHO.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to SAF (code A -> C)

#Filter table for switch-range from A -> C
time_AtoCsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> C")

time_AtoCsr2 <- transform(time_AtoCsr, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoCsr2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Switch-range from WAM to SAF") +
  scale_x_reverse() + xlim(c(50,0)) +
  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_switch_range_from_WAM_to_SAF.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#Histogram switch-range through time from WAM to CAR (code A -> J)

#Filter table for switch-range from A -> J
time_AtoJsr <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal_and_extinction") %>% filter(transitionAnag == "A -> J")

time_AtoJsr2 <- transform(time_AtoJsr, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AtoJsr2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Switch-range from WAM to CAR") +
  scale_x_reverse() + xlim(c(50,0)) +
  ylim(c(0,125))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_switch_range_from_WAM_to_CAR.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#histogram vicariance WAM | EAM

#Filter table for Vicariance WAM | EAM
time_AD <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AD -> A" | transitionCladog == "AD -> D")

time_AD2 <- transform(time_AD, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AD2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Allopatry between WAM and EAM") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,30))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_allopatric_events_between_WAM_and_EAM.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)


#histogram vicariance WAM | PAT

#Filter table for Vicariance WAM | PAT
time_AI <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AI -> A" | transitionCladog == "AI -> I")

time_AI2 <- transform(time_AI, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AI2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Allopatry between WAM and PAT") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,30))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_allopatric_events_between_WAM_and_PAT.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#histogram vicariance WAM | CHO

#Filter table for Vicariance WAM | CHO
time_AG <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AG -> A" | transitionCladog == "AG -> G")

time_AG2 <- transform(time_AG, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AG2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Allopatry between WAM and CHO") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,30))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_allopatric_events_between_WAM_and_CHO.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#histogram vicariance WAM | CHA

#Filter table for Vicariance WAM | CHA
time_AB <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "AB -> A" | transitionCladog == "AB -> B")

time_AB2 <- transform(time_AB, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_AB2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Allopatry between WAM and CHA") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,30))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_allopatric_events_between_WAM_and_CHA.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)

#histogram vicariance NAF | SAF

#Filter table for Vicariance NAF | SAF
time_EC <- tb_time %>% select(transitionCladogType, transitionCladog, transitionTime)%>%
  filter(transitionCladogType=="allopatry") %>% filter(transitionCladog == "CE -> C" | transitionCladog == "CE -> E")

time_EC2 <- transform(time_EC, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(time_EC2, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Allopatry between SAF and NAF") +
  scale_x_reverse()+ xlim(c(50,0)) +
  ylim(c(0,30))

#export figure (tá salvando na pasta output/DEC_M1)
ggsave(filename = "Figure_freq_allopatric_events_between_SAF_and_NAF.png",  
       plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  
       dpi = 320,  limitsize = TRUE,  bg = NULL)
