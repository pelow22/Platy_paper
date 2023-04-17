library(multimode)
library(dplyr)

#https://towardsdatascience.com/modelling-bimodal-distributions-with-multimode-in-r-94dbb884abc9

#test for bimodality WAM -> WAM + CHO

#histogram
png("Figure_dispersal_from_WAM_to_CHO.png", width = 480, height = 480)
hist(time_AtoG2$transitionTime, main = "Dispersal WAM -> WAM + CHO", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,150),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

#testing for bimodality
plot(density(time_AtoG2$transitionTime))
multimode::modetest(time_AtoG2$transitionTime)

modetest(time_AtoG2$transitionTime, method = "ACR")#p-value < 2.2e-16
modetest(time_AtoG2$transitionTime, method = "SI")#p-value = 0.002
modetest(time_AtoG2$transitionTime, method = "HY")#p-value < 2.2e-16
modetest(time_AtoG2$transitionTime, method = "FM")#p-value < 2.2e-16
modetest(time_AtoG2$transitionTime, method = "HH")#p-value < 2.2e-16
modetest(time_AtoG2$transitionTime, method = "CH")#p-value < 2.2e-16

#Ameijeiras-Alonso et al. (2019) excess mass test
#data:  time_AtoG2$transitionTime
#Excess mass = 0.11019, p-value < 2.2e-16
#alternative hypothesis: true number of modes is greater than 1

locmodes(time_AtoG2$transitionTime, mod0 = 2, display = TRUE)

#Estimated location
#Modes:  1.73195    23.081 
#Antimode: 18.12841 

#Critical bandwidth: 1.228439

wamcho <- as.numeric(time_AtoG2$transitionTime)
write.csv2(wamcho, file = "wamcho_bimodality.csv", dec = ".")
getwd()
#fiz a filtragem no excel rsrsrsrs

summary(wancho1)
summary(wancho2)

#WAM to PAT 
modetest(time_AtoI2$transitionTime)
locmodes(time_AtoI2$transitionTime, mod0 = 1, display = TRUE)
summary(time_AtoI2$transitionTime)

#WAM to CAR 
modetest(time_AtoJ2$transitionTime)
locmodes(time_AtoJ2$transitionTime, mod0 = 1, display = TRUE)
summary(time_AtoJ2$transitionTime)

#WAM to SAF
modetest(time_AtoC2$transitionTime)
locmodes(time_AtoC2$transitionTime, mod0 = 1, display = TRUE)
summary(time_AtoC2$transitionTime)

#WAM to CHA
modetest(time_AtoB2$transitionTime)
locmodes(time_AtoB2$transitionTime, mod0 = 1, display = TRUE)
summary(time_AtoB2$transitionTime)

#WAM to MES
modetest(time_AtoF2$transitionTime)
locmodes(time_AtoF2$transitionTime, mod0 = 1, display = TRUE)
summary(time_AtoF2$transitionTime)

#WAM to EAM
modetest(time_AtoD2$transitionTime, method = "ACR")#p-value < 2.2e-16***
locmodes(time_AtoD2$transitionTime, mod0 = 2, display = TRUE)
modetest(time_AtoD2$transitionTime, method = "SI")#p-value = 0.306
modetest(time_AtoD2$transitionTime, method = "HY")#p-value = 0.066
modetest(time_AtoD2$transitionTime, method = "FM")#p-value < 2.2e-16***
modetest(time_AtoD2$transitionTime, method = "HH")#p-value = 0.104
modetest(time_AtoD2$transitionTime, method = "CH")#p-value = 0.002***
locmodes(time_AtoD2$transitionTime, mod0 = 1, display = TRUE)

summary(time_AtoD2$transitionTime)

#EAM to WAM
modetest(time_DtoA2$transitionTime)
locmodes(time_DtoA2$transitionTime, mod0 = 1, display = TRUE)
summary(time_DtoA2$transitionTime)

#SAF TO NAF
modetest(time_CtoE2$transitionTime)
locmodes(time_CtoE2$transitionTime, mod0 = 1, display = TRUE)
summary(time_CtoE2$transitionTime)

#SAF TO CHA
modetest(time_CtoB2$transitionTime)
locmodes(time_CtoB2$transitionTime, mod0 = 1, display = TRUE)
summary(time_CtoB2$transitionTime)
