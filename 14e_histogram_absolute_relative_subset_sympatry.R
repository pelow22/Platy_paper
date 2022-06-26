library(dplyr)
library(ggplot2)
library(ape)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results/figures_head/")

########################### subset sympatry speciation histograms
#histogram frequency of  subset sympatry events through time
### ABS and REL (see below)

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)

#filter by SubSym events

atbn6 <- atbn %>% select(transitionCladogType, ageStart) %>% 
  filter(transitionCladogType == "subset_sympatry")

dtime6 <- transform(atbn6, ageStart = as.numeric(ageStart))


#histogram
png("Figure_absolute_subset_sympatry_ageStart.png", width = 480, height = 480)
hist(dtime6$ageStart, main = "Subset sympatry events", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,1500),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)

dev.off()


histoss <- hist(dtime6$ageStart, breaks = seq(0,50,1), xlim = c(0,50), 
                 ylab = "Subset sympatry", 
                 xlab = "Time", main = NA)
absolutss <- histoss$counts
absolutss <- as.data.frame(absolutss)

#insert column with time workframe
absolutss$time <- seq(0,49,1)


#table with absolut switch-ranges
absolutss

#table with the ltt
meanltt

### RATE graph

rel_ss <- inner_join(absolutss, meanltt, by="time")
rel_ss2 <- rel_ss %>% mutate(relative= absolutss/mean)

png("Figure_relative_subset_sympatry_ageStart.png", width = 480, height = 480)
plot(rel_ss2$time, rel_ss2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,80),
     main = "Subset sympatry / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()

