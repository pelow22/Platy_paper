#Coding points into areas
library(udunits2)
library(ggplot2)
library(sp)
library(rgdal)
library(plyr)

#Visualize the data on a map
wm <- borders("world", colour="gray50", fill="gray50")

#Open bioregions shapefile
areas <- readOGR(dsn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Bioregions/Platyrhinni",
                 layer = "Platyrhinni")
plot(areas)

#preparing shapefile to data frame (required by ggplot)
areas@data$id = rownames(areas@data)
area.points <- fortify(areas, region ="id")
areas.df <- join(area.points, areas@data, by = "id")

#sp1 = Aotus azarae
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp1_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  geom_point(data = sp1_splk,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "blue", size = 1)+
  theme_bw()

