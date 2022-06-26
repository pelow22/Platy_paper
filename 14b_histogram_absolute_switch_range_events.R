library(dplyr)
library(ggplot2)
library(ape)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

########################### Switch-range (SR) histograms
#histogram frequency of SR events through time
### ABS and REL (see below)

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)

#filter by SR events

atbn3 <- atbn %>% select(transitionAnagType, transitionTime) %>% filter(transitionAnagType == "dispersal_and_extinction")

dtime3 <- transform(atbn3, transitionTime = as.numeric(transitionTime))


#histogram (this was adopted)
png("Figure_absolute_switch_range.png", width = 480, height = 480)
hist(dtime3$transitionTime, main = "Switch-range events", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,600),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)

dev.off()


histosr <- hist(dtime3$transitionTime, breaks = seq(0,50,1), xlim = c(0,50), 
               ylab = "Switch-range transition time", 
               xlab = "Time", main = NA)
absolutsr <- histosr$counts
absolutsr <- as.data.frame(absolutsr)

#insert column with time workframe
absolutsr$time <- seq(0,49,1)


#table with absolut switch-ranges
absolutsr

#table with the ltt
meanltt

### RATE graph

rel_sr <- inner_join(absolutsr, meanltt, by="time")
rel_sr2 <- rel_sr %>% mutate(relative= absolutsr/mean)

png("Figure_relative_switch_ranges.png", width = 480, height = 480)
plot(rel_sr2$time, rel_sr2$relative, type = "l", lwd=2, xlim = c(50,0), ylim = c(0,12),
     main = "Switch-range / LTT", xlab = "Time", ylab = NA,
     las = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     frame = FALSE, col = "grey30", pch = 19)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()