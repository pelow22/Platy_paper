library(dplyr)
library(ggplot2)
library(ape)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

#histogram frequency of dispersal events throught time

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)



#filter by dispersal events

atbn2 <- atbn %>% select(transitionAnagType, transitionTime) %>% filter(transitionAnagType == "dispersal")

dtime <- transform(atbn2, transitionTime = as.numeric(transitionTime))


#histogram (this was adopted)
png("Figure_absolute_dispersal.png", width = 480, height = 480)
hist(dtime$transitionTime, main = "Dispersal events", 
     xlab ="Time", xlim = c(50,0), breaks = seq(50,0,-1), ylim = c(0,600),
     col = "grey30", border = 'grey30', las = 1, ylab = NA,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v = c(33.9,23.03,5.33,2.58), lwd=0.5,lty=3)
dev.off()


#histogram
#histo <- ggplot(dtime, aes(x = transitionTime)) + 
#  geom_histogram(binwidth = 1, boundary = 0) +
#  theme_minimal()+
#  xlab("Time") +
#  ylab("Absolute dispersal events") +
#  scale_x_reverse()+ xlim(c(50,0))
#histo  

#export figure

#ggsave(filename = "Figure_freq_dispersal_events.png",  plot = last_plot(),  device = "png",  path = NULL,
 #      scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)

#histogram for extracting counts

histo2 <- hist(dtime$transitionTime, breaks = seq(0,50,1), xlim = c(0,50), 
               ylab = "Dispersal transition time", 
               xlab = "Time", main = NA)
absolut <- histo2$counts
absolut <- as.data.frame(absolut)

#insert column with time workframe
absolut$time <- seq(0,49,1)

absolut
df4
df5 <- df4
df5$time <- seq(-45, 0, 1)

#correct value for missing line
df5$time[1] = -46

#table with absolut dispersals
absolut

#table with the ltt
df5

