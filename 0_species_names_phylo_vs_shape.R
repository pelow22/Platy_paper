library(ape)
library(rgdal)
library(stringr)

# Table comparisions: phylo tips and binomial

wd = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper"
setwd(wd)

#load phylogenies
trfn = "data/Phylogenies/phylo_Platy_Silvestro_2019.txt"
tr = read.nexus(trfn)

#extract tip names
phylo_tips <- tr$tips
summary(phylo_tips)
phylo_tips

#load shapefile
shp_all <- readOGR(dsn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Shapefiles", 
                   layer = "Platy_ranges_IUCN")
plot(shp_all)

#extract binominal names
shp_binomial <- shp_all$binomial
summary(shp_binomial)

#replace space by underscore between genus name and epithet specific
shp_binomial <- str_replace(shp_binomial, " ", "_") 
shp_binomial


