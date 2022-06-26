library(dplyr)
library(ggplot2)
library(ape)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")

########################### Allopatry histograms
#histogram frequency of  Allopatric events through time
### ABS and REL (see below)

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)

#filter by SR events

  atbn4 <- atbn %>% select(transitionCladogType, ageStart) %>% filter(transitionCladogType == "allopatry")

dtime4 <- transform(atbn4, ageStart = as.numeric(ageStart))


#histogram (this was adopted)
png("Figure_absolute_allopatry_ageStart.png", width = 480, height = 480)
hist(dtime4$ageStart, main = "Allopatric speciation events", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,1500),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)

dev.off()


histoal <- hist(dtime4$ageStart, breaks = seq(0,50,1), xlim = c(0,50), 
                ylab = "Allopatry", 
                xlab = "Time", main = NA)
absolutal <- histoal$counts
absolutal <- as.data.frame(absolutal)

#insert column with time workframe
absolutal$time <- seq(0,49,1)


#table with absolut switch-ranges
absolutal

#table with the ltt
meanltt

### RATE graph

rel_al <- inner_join(absolutal, meanltt, by="time")
rel_al2 <- rel_al %>% mutate(relative= absolutal/mean)

png("Figure_relative_allopatry_ageStart.png", width = 480, height = 480)
plot(rel_al2$time, rel_al2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,80),
     main = "Allopatric speciation / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

