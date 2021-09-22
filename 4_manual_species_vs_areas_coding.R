#Manual coding points into areas
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

#sp2 = Aotus griseimembra
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp2_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp3 = Aotus lemurinus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp3_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp5 = Aotus nigriceps
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp5_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp6 = Aotus trivirgatus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp6_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp8 = Cacajao melanocephalus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp8_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp9 = Cebus capucinus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp9_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp10 = Cebus olivaceus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp10_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp12 = Pithecia irrorata
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp12_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp13 = Pithecia monachus
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp13_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp14 = Saguinus nigricollis
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp14_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp4 = Aotus nancymai
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp4,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()

#sp15 = Aotus vociferans
ggplot()+ coord_fixed() + wm + coord_sf(xlim = c(-100, -30), ylim = c(-35, 22), expand = FALSE) +
  geom_polygon(data = areas.df, aes(x = long, y = lat, group = group, fill = bioregio)) +
  geom_point(data = sp15_cl,
             aes(x = decimalLongitude, y = decimalLatitude), colour = "darkred", size = 1)+
  theme_bw()
