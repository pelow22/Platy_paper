library(rgbif)
library(magrittr)

sstab <-"D:/Usuários/peter/OneDrive/Pesquisa/2021_ArtigoQ1 2 Platyrhinni-Asus/Platy_paper/data/Splink_gbif_data/species_search.txt"
species_search <- read.csv(file = sstab, header = TRUE)

#sp1 = Aotus azarae
sp1 <- occ_search(name_suggest(q = species_search[1,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp1 <- sp1 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp1)

#sp2 = Aotus griseimembra
sp2 <- occ_search(name_suggest(q = species_search[2,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp2 <- sp2 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp2)

#sp3 = Aotus lemurinus
sp3 <- occ_search(name_suggest(q = species_search[3,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp3 <- sp3 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp3)

#sp4 = Aotus nancymai NODATA returned.
name_suggest(q = species_search[4,1], rank = "species")
n4 <- name_lookup(q = species_search[4,1], rank = "species")
sp4 <- occ_search(n4[["data"]][["key"]])

#sp5 = Aotus nigriceps
sp5 <- occ_search(name_suggest(q = species_search[5,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp5 <- sp5 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp5)

#sp6 = Aotus trivirgatus
sp6 <- occ_search(name_suggest(q = species_search[6,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp6 <- sp6 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp6)

#sp7 = Cacajao ayresi NODATA returned
sp7 <- occ_search(name_suggest(q = species_search[7,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
print(sp7)

#sp8 = Cacajao melanocephalus
sp8 <- occ_search(name_suggest(q = species_search[8,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp8 <- sp8 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp8)

#sp9 = Cebus capucinus
sp9 <- occ_search(name_suggest(q = species_search[9,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp9 <- sp9 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp9)

#sp10 = Cebus olivaceus
sp10 <- occ_search(name_suggest(q = species_search[10,1], rank = "species")[["data"]][["key"]], 
                  hasCoordinate = TRUE, limit = 5000)
sp10 <- sp10 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp10)

#sp11 = Chiropotes israelita NODATA returned.
sp11 <- occ_search(name_suggest(q = species_search[11,1], rank = "species")[["data"]][["key"]], 
                   hasCoordinate = TRUE, limit = 5000)
sp11 <- sp11 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp11)

#sp12 = Pithecia irrorata
sp12 <- occ_search(name_suggest(q = species_search[12,1], rank = "species")[["data"]][["key"]], 
                   hasCoordinate = TRUE, limit = 5000)
sp12 <- sp12 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp12)

#sp 13 = 	Pithecia monachus
sp13 <- occ_search(name_suggest(q = species_search[13,1], rank = "species")[["data"]][["key"]], 
                   hasCoordinate = TRUE, limit = 5000)
sp13 <- sp13 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp13)

#sp14 = Leontocebus nigricollis (searched for Saguinus nigricollis)
sp14 <- occ_search(name_suggest(q = species_search[14,1], rank = "species")[["data"]][["key"]], 
                   hasCoordinate = TRUE, limit = 5000)
sp14 <- sp14 %>% occ_issues(-bri, -ccm, -cdiv, -cdout, -cdrepf, -cucdmis, gass84, -gdativ)
print(sp14)

####################Cleaning data by GBIF issues
gi <- gbif_issues()
write.csv(gi, file = "Gbif_issues_code.txt") 