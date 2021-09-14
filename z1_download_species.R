#proximo passo: montar um script ou continuar aqui para baixar os dados geográficos dos que estão faltando.

## Download of species data with no range in IUCN shapefiles.

library(stringr)
library(dplyr)
library(Rocc)

devtools::install_github("saramortara/rocc")

#subset: tip.labels without range polygons and without xy coordinates
sp_no_range <- names3 %>% filter(is.na(names3$shp_binomial) & !is.na(names3$tip.label) 
                  & is.na(names3$Taxon))

sp_no_range2 <- sp_no_range$tip.label
#replace underscore by space
species_search <- str_replace(sp_no_range2, "_", " ")
summary(species_search)
write.csv(species_search, file = "species_search_list.txt")

# https://liibre.github.io/Rocc/articles/basic_workflow.html
#########Species Link
data_splink <- list()
for (sp in species_search) {
  data_splink[[sp]] <- rspeciesLink(species = sp, 
                                    filename = paste0(gsub(" ", "_", sp), "_splink"))
}

# download at "results" folder.
df_splink <- bind_rows(data_splink, .id = "species_search") 
dim(df_splink)

unique(df_splink$species_search)

########GBIF
data_gbif <- list()
for (sp in species_search) {
  data_gbif[[sp]] <- rgbif2(species = sp, 
                            filename = paste0(gsub(" ", "_", sp), "_gbif"))
}

names(data_gbif) <- species_search
df_gbif <- bind_rows(data_gbif, .id = "species_search")
dim(df_gbif)

###Binding data from different sources
df <- bind_dwc(splink_data = df_splink, gbif_data = df_gbif)


######################Download 2 after name cleaning
species_search <- read.csv(file = "D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Splink_gbif_data/species_search.txt",
                      header = TRUE)
#########Species Link
data_splink <- list()
for (sp in species_search) {
  data_splink[[sp]] <- rspeciesLink(species = sp, 
                                    filename = paste0(gsub(" ", "_", sp), "_splink"))
}

# download at "results" folder.
df_splink <- bind_rows(data_splink, .id = "species_search") 
dim(df_splink)

unique(df_splink$species_search)

##########GBIF
data_gbif <- list()
for (sp in species_search) {
  data_gbif[[sp]] <- rgbif2(species = sp, 
                            filename = paste0(gsub(" ", "_", sp), "_gbif", force = TRUE))
}

names(data_gbif) <- species_search
df_gbif <- bind_rows(data_gbif, .id = "species_search")
dim(df_gbif)

###Binding data from different sources
df <- bind_dwc(splink_data = df_splink, gbif_data = df_gbif)
