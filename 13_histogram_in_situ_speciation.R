library(ggplot2)

setwd("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/results")

##Figure
#frequency of in situ speciation per biogeographic unit (bioregion)

#get the number of species per area
area_spp2 <- read.table(file = "bioregion_vs_in_situ_speciation.txt", header = TRUE)
area_spp2

#bar plot
iss <- ggplot(data = area_spp2, aes(x = Bioregion, y = iss)) +
  geom_bar(stat = "identity", width = 0.5) + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = iss), hjust = - 0.1, color = "grey50", size = 2)+
  xlab("Bioregion") +
  ylab("In situ speciation (average)")
iss

#export figure
ggsave(filename = "Figure_bioregions_vs_in_situ_speciation.png",  plot = last_plot(),  device = "png",  path = NULL,
       scale = 1,  width = 1000,  height = 1000,  units = "px",  dpi = 320,  limitsize = TRUE,  bg = NULL)
