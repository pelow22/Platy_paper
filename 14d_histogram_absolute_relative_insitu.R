library(dplyr)
library(ggplot2)
library(ape)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")

########################### in-situ speciation histograms
#histogram frequency of  in-situ speciation events through time
### ABS and REL (see below)

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)

#filter by SR events

atbn5 <- atbn %>% select(transitionCladogType, ageStart) %>% 
  filter(transitionCladogType == "in_situ_speciation")

dtime5 <- transform(atbn5, ageStart = as.numeric(ageStart))


#histogram
png("Figure_absolute_in_situ_speciation_ageStart.png", width = 480, height = 480)
hist(dtime5$ageStart, main = substitute(paste(italic("In situ")," speciation events")), 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,1500),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)

dev.off()


histoins <- hist(dtime5$ageStart, breaks = seq(0,50,1), xlim = c(0,50), 
                ylab = "Sympatry", 
                xlab = "Time", main = NA)
absolutins <- histoins$counts
absolutins <- as.data.frame(absolutins)

#insert column with time workframe
absolutins$time <- seq(0,49,1)


#table with absolut switch-ranges
absolutins

#table with the ltt
meanltt

### RATE graph

rel_ins <- inner_join(absolutins, meanltt, by="time")
rel_ins2 <- rel_ins %>% mutate(relative= absolutins/mean)

png("Figure_relative_in_situ_speciation_ageStart.png", width = 480, height = 480)
plot(rel_ins2$time, rel_ins2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,80),
     main = substitute(paste(italic("In situ")," speciation / LTT")), xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

