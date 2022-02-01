library(dplyr)
library(ggplot2)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

#histogram frequency of dispersal events throught time

#open results table
atbn <- read.csv(file = "All_transitions_by_node_BSM.csv", header = TRUE, sep = ",")
head(atbn)

#open tree
trfn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Phylogenies/phylo_Platy_Silvestro_2019_namesOK.txt"
tr = read.nexus(trfn)

#filter by dispersal events

atbn2 <- atbn %>% select(transitionAnagType, transitionTime) %>% filter(transitionAnagType == "dispersal")

dtime <- transform(atbn2, transitionTime = as.numeric(transitionTime))

#histogram
ggplot(dtime, aes(x = transitionTime)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal()+
  xlab("Time") +
  ylab("Dispersal events") +
  scale_x_reverse()
  
#export figure

ggsave(filename = "Figure_freq_dispersal_events.png",  plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)
