library(stringr)
library(dplyr)
library(Rocc)

sstab <-"D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Splink_gbif_data/species_search.txt"
species_search <- read.csv(file = sstab, header = TRUE)

#sp1 = Aotus azarae
sp1_splk <- rspeciesLink(species = species_search[1,1], Coordinates = "Yes", save = FALSE)
print(sp1_splk)

#sp2 = Aotus griseimembra NODATA
sp2_splk <- rspeciesLink(species = species_search[2,1], Coordinates = "Yes", save = FALSE)
print(sp2_splk)

#sp3 = Aotus lemurinus NODATA
sp3_splk <- rspeciesLink(species = species_search[3,1], Coordinates = "Yes", save = FALSE)
print(sp3_splk)

#sp4 = Aotus nancymai NODATA
sp4_splk <- rspeciesLink(species = species_search[4,1], Coordinates = "Yes", save = FALSE)
print(sp4_splk)

#sp5 = Aotus nigriceps
sp5_splk <- rspeciesLink(species = species_search[5,1], Coordinates = "Yes", save = FALSE)
print(sp5_splk)

#sp6 = Aotus trivirgatus
sp6_splk <- rspeciesLink(species = species_search[6,1], Coordinates = "Yes", save = FALSE)
print(sp6_splk)

#sp7 = Cacajao ayresi NODATA
sp7_splk <- rspeciesLink(species = species_search[7,1], Coordinates = "Yes", save = FALSE)
print(sp7_splk)

#sp8 = Cacajao melanocephalus
sp8_splk <- rspeciesLink(species = species_search[8,1], Coordinates = "Yes", save = FALSE)
print(sp8_splk)

#sp9 = Cebus capucinus
sp9_splk <- rspeciesLink(species = species_search[9,1], Coordinates = "Yes", save = FALSE)
print(sp9_splk)

#sp10 = Cebus olivaceus
sp10_splk <- rspeciesLink(species = species_search[10,1], Coordinates = "Yes", save = FALSE)
print(sp10_splk)

#sp11 = Chiropotes israelita NODATA returned.
sp11_splk <- rspeciesLink(species = species_search[11,1], Coordinates = "Yes", save = FALSE)
print(sp11_splk)

#sp12 = Pithecia irrorata
sp12_splk <- rspeciesLink(species = species_search[12,1], Coordinates = "Yes", save = FALSE)
print(sp12_splk)

#sp 13 = 	Pithecia monachus
sp13_splk <- rspeciesLink(species = species_search[13,1], Coordinates = "Yes", save = FALSE)
print(sp13_splk)

#sp14 = Leontocebus nigricollis (searched for Saguinus nigricollis)
sp14_splk <- rspeciesLink(species = "Saguinus nigricollis", Coordinates = "Yes", save = FALSE)
print(sp14_splk)