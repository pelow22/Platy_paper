library(ggpubr)
library(nortest)
library(dplyr)
library(ggplot2)
library(zoo)


#Plot past sealevel changes
#data from Miller et al. 2020

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Sea_levels_Cenozoic/")

seal <- read.table(file = "miller2020rsl_dataonly.txt", sep = "", header = TRUE, row.names=NULL)
head(seal)
plot(seal$age_calkaBP, seal$sealevel, type = "l")
seal_yr <- as.data.frame(cbind((seal$age_calkaBP/1000), seal$sealevel))

plot(seal_yr$V1, seal_yr$V2, type = "l")

#Smoothed data
seal_sm <- read.table(file = "miller2020rsl-sm_dataonly.txt", sep = "", header = TRUE, row.names=NULL)
head(seal_sm)
seal_sm_yr <- as.data.frame(cbind((seal_sm$age_calkaBP/1000), seal_sm$sealevel))
plot(seal_sm_yr$V1, seal_sm_yr$V2, type = "l")
lines(seal_sm$age_calkaBP, seal_sm$sealevel, type = "l", col = "red")


##### FIgure
setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/doc/Figures/")

png("Figure_relative_dispersal_sea_level.png", width = 480, height = 480)
plot(dis_time_rel$V1, ts(fitted(mbp3),start=1),type = "l", lwd=2,lty=4, col = "gray50",
     xlim = c(50,0), ylim = c(0,12), ylab = NA, las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, 
     cex.sub=1.5,pch = 19, xlab = "Time", main = "Dispersal phases and sea level (m)")

abline(v = c(28,20,12), lwd=0.5,lty=1)#breaking points

par(new = TRUE)
plot(seal_yr$V1, seal_yr$V2, type = "l", lwd=0.5, xlim = c(50,0), ylim = c(-150, 150),
     ylab = NA, axes = FALSE, las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "gray30", pch = 19, xlab = NA)
axis(side = 4, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,pch = 19)
#lines(pcdatasm$AgeMa, pcdatasm$surfaceTemperature, type = "l", col = "red")
lines(seal_sm_yr$V1, seal_sm_yr$V2, lty=1, lwd=2, col = "blue")

dev.off()


#Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(phseal$sealevel)

#Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution.
ggqqplot(phseal$sealevel)

#The R function shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
shapiro.test(phseal$sealevel[6501:11500])

#Shapiro-Wilk normality test
#data:  phseal$sealevel[6501:11500]
#W = 0.9933, p-value = 5.265e-13
# distribution of the data are significantly different from normal distribution (not normal)

sealevel <- phseal$sealevel
ad.test(sealevel)

#Anderson-Darling normality test
#data:  sealevel
#A = 79.759, p-value < 2.2e-16


############## REgression Phases (response) ~ sea level (predictor) NOT NORMAL
setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Sea_levels_Cenozoic/")
phseal <- read.table(file = "miller2020rsl_dataonly_phases.txt", sep = "", header = TRUE)
head(phseal)

phseal$Phase <- as.factor(phseal$Phase)
#phtemp$surfaceTemperature <- as.factor(phtemp$surfaceTemperature)
xtabs(~Phase + sealevel, data = phseal)

str(phseal)

modelphseal <- lm(phseal$sealevel ~ phseal$Phase)
modelphseal
summary(modelphseal)
summary(modelphseal)$coef

contrasts(phseal$Phase)

plot(phseal$Phase, phseal$sealevel)

#Boxplot

ggboxplot(phseal, x = "Phase", y = "sealevel",
          color = "Phase", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFBB"),
          order = c("A", "B", "C", "D"),
          ylab = "m", xlab = "Dispersal Phase")

#Mean plots

ggline(phseal, x = "Phase", y = "sealevel",
       add = c("mean_se", "jitter"),
       order = c("A", "B", "C", "D"),
       ylab = "m", xlab = "Dispersal Phase")

#Queremos saber se há alguma diferença significativa na média total de acidente ofidico entre as cinco
#formações vegetais.

kruskal.test(sealevel ~ Phase, data = phseal)

#Kruskal-Wallis rank sum test
#data:  sealevel by Phase
#Kruskal-Wallis chi-squared = 1653.3, df = 3, p-value < 2.2e-16

#Como o p-value < 0.05, conclui0se que tem diferenças significativas entre os tratamentos

#comparação par a par entre as formações vegetais.

pairwise.wilcox.test(phseal$sealevel, phseal$Phase,
                     p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
#data:  phseal$sealevel and phseal$Phase 
#A      B      C     
#B 0.42   -      -     
#C <2e-16 <2e-16 -     
#D <2e-16 <2e-16 <2e-16

#P value adjustment method: BH
