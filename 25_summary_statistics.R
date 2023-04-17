#Summary statistics

#open table with results
tb_time <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",", dec = ".")

#absolute number of dispersals
#Filter table for dispersal from WAM -> WAM + PAT
time_AtoI <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AI")

time_AtoI2 <- transform(time_AtoI, transitionTime = as.numeric(transitionTime))
summary(time_AtoI2$transitionTime)


#absolute number of dispersals
#Filter table for dispersal from WAM -> WAM + CHO
time_AtoG <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AG")

time_AtoG2 <- transform(time_AtoG, transitionTime = as.numeric(transitionTime))
summary(time_AtoG2$transitionTime)

#absolute number of dispersals
#Filter table for dispersal from WAM -> WAM + SAF
time_AtoC <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "A -> AC")

time_AtoC2 <- transform(time_AtoC, transitionTime = as.numeric(transitionTime))
summary(time_AtoC2$transitionTime)

#absolute number of dispersals
#Filter table for dispersal from SAF -> SAF + CHA
time_CtoB <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> BC")

time_CtoB2 <- transform(time_CtoB, transitionTime = as.numeric(transitionTime))
summary(time_CtoB2$transitionTime)

#absolute number of dispersals
#Filter table for dispersal from SAF -> SAF + NAF
time_CtoE <- tb_time %>% select(transitionAnagType, transitionAnag, transitionTime)%>%
  filter(transitionAnagType=="dispersal") %>% filter(transitionAnag == "C -> CE")

time_CtoE2 <- transform(time_CtoE, transitionTime = as.numeric(transitionTime))
summary(time_CtoE2$transitionTime)