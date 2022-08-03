library(ggpubr)
library(nortest)
library(dplyr)
library(ggplot2)
library(zoo)


#Plot past climate
#data from Tierney et al. 2020

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")

pcdatasm <- read.table(file = "THansenMethodSmoothed.csv", sep = ",", header = TRUE)
pcdatasm
plot(pcdatasm$AgeMa, pcdatasm$surfaceTemperature, type = "l")

pcdata <- read.table(file = "THansenMethod.csv", sep = ",", header = TRUE)
pcdata

#moving_average_2 <- rev(rollmean(pcdata$surfaceTemperature, k = 2))
#head(moving_average_5)

png("Figure_paleoclim.png", width = 480, height = 480)
plot(pcdata$AgeMa, pcdata$surfaceTemperature, type = "l", lwd=0.5, xlim = c(50,0), ylim = c(10,30),
     main = "Global surface temperature (°C)", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
lines(pcdatasm$AgeMa, pcdatasm$surfaceTemperature, type = "l", col = "black")
#lines(dis_time_rel$V1, ts(fitted(mbp3),start=1),lwd=2,lty=4, col = "gray50")
abline(v = c(28,20,12), lwd=0.5,lty=1)#breaking points
#lines(rev(rollmean(pcdata$surfaceTemperature, k = 2, fill = NA, align = "right")), col = "blue")
#ines(rev(rollmax(pcdata$surfaceTemperature, k = 2, fill = NA, align = "left")), col = "red")
#lines(rev(rollmedian(pcdata$surfaceTemperature, k = 2, fill = NA, align = "left")), col = "green")
dev.off()

### FIGURA BOA (usar)
png("Figure_relative_dispersal_paleoclim.png", width = 480, height = 480)
plot(dis_time_rel$V1, ts(fitted(mbp3),start=1),type = "l", lwd=2,lty=4, col = "gray50",
     xlim = c(50,0), ylim = c(0,12), ylab = NA, las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, 
     cex.sub=1.5,pch = 19, xlab = "Time", main = "Dispersal phases and surface temperature (°C)")

#plot(dis_time_rel$V1, dis_time_rel$V2, type = "l", lwd=2, xlim = c(50,0),
    #main = "Dispersal / LTT and Global temperature", xlab = "Time", ylab = NA,
     #las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     #frame = FALSE, col = "grey30", pch = 19)
#abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)#Epochs
abline(v = c(28,20,12), lwd=0.5,lty=1)#breaking points

par(new = TRUE)
plot(pcdata$AgeMa, pcdata$surfaceTemperature, type = "l", lwd=0.5, xlim = c(50,0), ylim = c(10,30),
     ylab = NA, axes = FALSE, las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19, xlab = NA)
axis(side = 4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,pch = 19)
lines(pcdatasm$AgeMa, pcdatasm$surfaceTemperature, type = "l", col = "red")
#lines(loess.smooth(pcdata$AgeMa, pcdata$surfaceTemperature, span = 1/250),lty=1, lwd=2, col = "red")

dev.off()

############## REgression Phases (response) ~ Temperature (predictor) NOT NORMAL
setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Paleotemperatures/PastClimates-master/")
phtemp <- read.table(file = "Temp_Phases.csv", sep = ";", header = TRUE)
head(phtemp)

phtemp$Phase <- as.factor(phtemp$Phase)
#phtemp$surfaceTemperature <- as.factor(phtemp$surfaceTemperature)
xtabs(~Phase + surfaceTemperature, data = phtemp)

str(phtemp)

modelphtemp <- lm(phtemp$surfaceTemperature ~ phtemp$Phase)
modelphtemp
summary(modelphtemp)
summary(modelphtemp)$coef

contrasts(phtemp$Phase)

plot(phtemp$Phase, phtemp$surfaceTemperature)



################ ANOVA
phtempANOVA <- read.table(file = "Temp_Phases.csv", sep = ";", header = TRUE)
head(phtempANOVA)

#http://www.sthda.com/english/wiki/normality-test-in-r
#Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(phtempANOVA$surfaceTemperature)

#Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution.
ggqqplot(phtempANOVA$surfaceTemperature)

#The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
shapiro.test(phtempANOVA$surfaceTemperature[6501:11500])

#Shapiro-Wilk normality test
#data:  phtempANOVA$surfaceTemperature[0:5000]
#W = 0.98158, p-value < 2.2e-16
# distribution of the data are significantly different from normal distribution (not normal)

surftemp <- phtempANOVA$surfaceTemperature
ad.test(surftemp)

#Anderson-Darling normality test
#data:  surftemp
#A = 678.3, p-value < 2.2e-16

###########################non parametric statistics
#http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

#estatísticas descritivas dos acidentes totais por formação vegetal

group_by(phtemp, Phase) %>%
  summarise(
    count = n(),
    mean = mean(surfaceTemperature, na.rm = TRUE),
    sd = sd(surfaceTemperature, na.rm = TRUE),
    median = median(surfaceTemperature, na.rm = TRUE),
    IQR = IQR(surfaceTemperature, na.rm = TRUE)
  )

# A tibble: 4 x 6
#Phase count  mean    sd median   IQR
#<fct> <int> <dbl> <dbl>  <dbl> <dbl>
#1 A      3606  19.5 1.58    19.0 1.47 
#2 B      4313  19.3 0.595   19.3 0.790
#3 C      2522  19.0 0.692   18.9 0.800
#4 D      6692  14.9 2.16    15.1 3.76


#Boxplot

ggboxplot(phtemp, x = "Phase", y = "surfaceTemperature",
          color = "Phase", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFBB"),
          order = c("A", "B", "C", "D"),
          ylab = "°C", xlab = "Dispersal Phase")

#Mean plots

ggline(phtemp, x = "Phase", y = "surfaceTemperature",
       add = c("mean_se", "jitter"),
       order = c("A", "B", "C", "D"),
       ylab = "°C", xlab = "Dispersal Phase")

#Queremos saber se há alguma diferença significativa na média total de acidente ofidico entre as cinco
#formações vegetais.

kruskal.test(surfaceTemperature ~ Phase, data = phtemp)

#Kruskal-Wallis rank sum test
#data:  surfaceTemperature by Phase
#Kruskal-Wallis chi-squared = 12172, df = 3, p-value < 2.2e-16

#Como o p-value < 0.05, conclui0se que tem diferenças significativas entre os tratamentos

#comparação par a par entre as formações vegetais.

pairwise.wilcox.test(phtemp$surfaceTemperature, phtemp$Phase,
                     p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  phtemp$surfaceTemperature and phtemp$Phase 

#  A       B       C      
#B < 2e-16 -       -      
#C 5.8e-09 < 2e-16 -      
#D < 2e-16 < 2e-16 < 2e-16
#P value adjustment method: none

###Lagged graphic
## 

dis_time_rel
lag5 <- cbind((dis_time_rel$V1+5), dis_time_rel$V2)