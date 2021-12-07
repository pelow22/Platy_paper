library(ggplot2)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

##Figure 1
#Number of species per biogeographic unit (bioregion)

#get the number of species per area
area_spp <- read.table(file = "bioregion_vs_number_spp.txt", header = TRUE)
area_spp

#bar plot
b <- ggplot(data = area_spp, aes(x = Bioregion, y = Spp.)) +
  geom_bar(stat = "identity", width = 0.5) + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = Spp.), hjust = 1.4, color = "grey90", size = 2.5)+
  xlab("Bioregion") +
  ylab("Richness (number of species)")
b

#export figure 1
ggsave(filename = "Figure1_spp_vs_bioregions.png",  plot = last_plot(),  device = "png",  path = NULL,
  scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)


##Figure 2
#Frequency of range
range <- read.table(file = "spp_vs_range.txt", header = TRUE)
range

#histogram
h <- ggplot(data = range, aes(x = Range)) +
  geom_histogram(binwidth = 0.5) +
  theme_minimal()+
  scale_x_continuous(breaks = c(1, 2, 3), minor_breaks = NULL)+
  xlab("Range (number of bioregions)") +
  ylab("Frequency of species")+  
  stat_bin(binwidth = 1, geom = "text", aes(label=..count..), 
              vjust = 1.4, color = "grey90", size = 2.5, na.rm = TRUE)
h

#export figure 2
ggsave(filename = "Figure2_freq_spp_vs_range.png",  plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)

