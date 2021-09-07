library(ape)
library(rgdal)
library(stringr)
library(dplyr)
library(udunits2)
library(ggplot2)
library(tmap)
library(maptools)


######Table comparisions: phylo tips and binomial

wd = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper"
setwd(wd)

#################load phylogenies (Silvestro et al. 2019)
trfn = "data/Phylogenies/phylo_Platy_Silvestro_2019.txt"
tr = read.nexus(trfn)

#extract tip names
phylo_tips <- as.data.frame(tr$tips)
summary(phylo_tips)
phylo_tips

#################load shapefile  (IUCN shapefiles)
shp_all <- readOGR(dsn = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Shapefiles", 
                   layer = "Platy_ranges_IUCN")
plot(shp_all)

#extract binominal names 
shp_binomial <- shp_all$binomial
summary(shp_binomial)

#replace space by underscore between genus name and epithet specific
shp_binomial <- str_replace(shp_binomial, " ", "_") 
shp_binomial <- as.data.frame(shp_binomial)
shp_binomial

#test whether both tables contain the exact same rows
setequal(phylo_tips, shp_binomial)

#table with all names for comparison
names <- full_join(shp_binomial, phylo_tips, by = c("shp_binomial" = "tip.label"),
  copy = TRUE,  suffix = c(".x", ".y"),  keep = TRUE, na_matches = "na")

############checking distributional data on PanTHERIA database

panthdb <- read.csv("D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Pantheria/Pantheria_subset_Platy_latlong.csv",
           header = TRUE, sep = ";")

# joining with tip.labels names.
names2 <- full_join(names, panthdb, by = c("tip.label" = "MSW05_Binomial"),
                    copy = TRUE,  suffix = c(".x", ".y"),  keep = TRUE, na_matches = "na")

#get a table with tip names, no shapefile range and latlong info in the Pantheria db.

panthdb_xy <- names2 %>% filter(is.na(names2$shp_binomial) & !is.na(names2$tip.label) 
                  & !is.na(names2$X26.2_GR_MaxLat_dd))

#plot xy data


###################get a table with tip names and no geographic information
tips_no_geo_info <- names2 %>% filter(is.na(names2$shp_binomial) & !is.na(names2$tip.label) 
                 & is.na(names2$X26.2_GR_MaxLat_dd))

#open fossil latlong (from Silvestro et al. 2019)