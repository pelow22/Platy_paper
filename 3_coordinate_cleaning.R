library(udunits2)
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)
library(rgdal)
library(magrittr)

#Visualize the data on a map
wm <- borders("world", colour="gray50", fill="gray50")

##############################################################sp1 = Aotus azarae

#Preparing data
sp1 <- sp1[["data"]] #GBIF
sp1_splk <- transform(sp1_splk, decimalLongitude = as.numeric(decimalLongitude), 
                          decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp1, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  geom_point(data = sp1_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
    theme_bw()

#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp1$countryCode <-  countrycode(sp1$countryCode, origin =  'iso2c', destination = 'iso3c')
sp1 <- data.frame(sp1)
sp1_flags <- clean_coordinates(x = sp1,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp1_flags)
plot(sp1_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp1_fl <- sp1[!sp1_flags$.summary,]

#Cleaned data frame.TO USE
sp1_cl <- sp1[sp1_flags$.summary,]



########################################################sp2 = Aotus griseimembra

#Preparing data
sp2 <- sp2[["data"]] #GBIF

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp2, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
    theme_bw()

#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp2$countryCode <-  countrycode(sp2$countryCode, origin =  'iso2c', destination = 'iso3c')
sp2 <- data.frame(sp2)
sp2_flags <- clean_coordinates(x = sp2,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))

summary(sp2_flags)
plot(sp2_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp2_fl <- sp2[!sp2_flags$.summary,]

#Cleaned data frame.TO USE
sp2_cl <- sp2[sp2_flags$.summary,]


###########################################################sp3 = Aotus lemurinus

#Preparing data
sp3 <- sp3[["data"]] #GBIF

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp3, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()

#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp3$countryCode <-  countrycode(sp3$countryCode, origin =  'iso2c', destination = 'iso3c')
sp3 <- data.frame(sp3)
sp3_flags <- clean_coordinates(x = sp3,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp3_flags)
plot(sp3_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp3_fl <- sp3[!sp3_flags$.summary,]

#Cleaned data frame.TO USE
sp3_cl <- sp3[sp3_flags$.summary,]

##############################################################sp5 = Aotus nigriceps

#Preparing data
sp5 <- sp5[["data"]] #GBIF
sp5_splk <- transform(sp5_splk, decimalLongitude = as.numeric(decimalLongitude), 
                      decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp5, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  geom_point(data = sp5_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  theme_bw()

#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp5$countryCode <-  countrycode(sp5$countryCode, origin =  'iso2c', destination = 'iso3c')
sp5 <- data.frame(sp5)
sp5_flags <- clean_coordinates(x = sp5,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp5_flags)
plot(sp5_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp5_fl <- sp5[!sp5_flags$.summary,]

#Cleaned data frame.TO USE
sp5_cl <- sp5[sp5_flags$.summary,]

##############################################################sp6 = Aotus trivirgatus

#Preparing data
sp6 <- sp6[["data"]] #GBIF
sp6_splk <- transform(sp6_splk, decimalLongitude = as.numeric(decimalLongitude), 
                      decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp6_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp6, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp6$countryCode <-  countrycode(sp6$countryCode, origin =  'iso2c', destination = 'iso3c')
sp6 <- data.frame(sp6)
sp6_flags <- clean_coordinates(x = sp6,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp6_flags)
plot(sp6_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp6_fl <- sp6[!sp6_flags$.summary,]

#Cleaned data frame.TO USE
sp6_cl <- sp6[sp6_flags$.summary,]


##############################################################sp8 = Cacajao melanocephalus

#Preparing data
sp8 <- sp8[["data"]] #GBIF
sp8_splk <- transform(sp8_splk, decimalLongitude = as.numeric(decimalLongitude), 
                      decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp8_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp8, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp8$countryCode <-  countrycode(sp8$countryCode, origin =  'iso2c', destination = 'iso3c')
sp8 <- data.frame(sp8)
sp8_flags <- clean_coordinates(x = sp8,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp8_flags)
plot(sp8_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp8_fl <- sp8[!sp8_flags$.summary,]

#Cleaned data frame.TO USE
sp8_cl <- sp8[sp8_flags$.summary,]


##############################################################sp9 = Cebus capucinus

#Preparing data
sp9 <- sp9[["data"]] #GBIF
sp9_splk <- transform(sp9_splk, decimalLongitude = as.numeric(decimalLongitude), 
                      decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp9_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp9, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp9$countryCode <-  countrycode(sp9$countryCode, origin =  'iso2c', destination = 'iso3c')
sp9 <- data.frame(sp9)
sp9_flags <- clean_coordinates(x = sp9,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp9_flags)
plot(sp9_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp9_fl <- sp9[!sp9_flags$.summary,]

#Cleaned data frame.TO USE
sp9_cl <- sp9[sp9_flags$.summary,]

#########################################################sp10 = Cebus olivaceus

#Preparing data
sp10 <- sp10[["data"]] #GBIF
sp10_splk <- transform(sp10_splk, decimalLongitude = as.numeric(decimalLongitude), 
                      decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp10_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp10, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp10$countryCode <-  countrycode(sp10$countryCode, origin =  'iso2c', destination = 'iso3c')
sp10 <- data.frame(sp10)
sp10_flags <- clean_coordinates(x = sp10,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                         "zeros", "countries"))
summary(sp10_flags)
plot(sp10_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp10_fl <- sp10[!sp10_flags$.summary,]

#Cleaned data frame.TO USE
sp10_cl <- sp10[sp10_flags$.summary,]

#########################################################sp12 = Pithecia irrorata

#Preparing data
sp12 <- sp12[["data"]] #GBIF
sp12_splk <- transform(sp12_splk, decimalLongitude = as.numeric(decimalLongitude), 
                       decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp12_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp12, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp12$countryCode <-  countrycode(sp12$countryCode, origin =  'iso2c', destination = 'iso3c')
sp12 <- data.frame(sp12)
sp12_flags <- clean_coordinates(x = sp12,
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                          "zeros", "countries"))
summary(sp12_flags)
plot(sp12_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp12_fl <- sp12[!sp12_flags$.summary,]

#Cleaned data frame.TO USE
sp12_cl <- sp12[sp12_flags$.summary,]


######################################################sp13 = 	Pithecia monachus

#Preparing data
sp13 <- sp13[["data"]] #GBIF
sp13_splk <- transform(sp13_splk, decimalLongitude = as.numeric(decimalLongitude), 
                       decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp13_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp13, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp13$countryCode <-  countrycode(sp13$countryCode, origin =  'iso2c', destination = 'iso3c')
sp13 <- data.frame(sp13)
sp13_flags <- clean_coordinates(x = sp13,
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                          "zeros", "countries"))
summary(sp13_flags)
plot(sp13_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp13_fl <- sp13[!sp13_flags$.summary,]

#Cleaned data frame.TO USE
sp13_cl <- sp13[sp13_flags$.summary,]

##############sp14 = Leontocebus nigricollis (searched for Saguinus nigricollis)

#Preparing data
sp14 <- sp14[["data"]] #GBIF
sp14_splk <- transform(sp14_splk, decimalLongitude = as.numeric(decimalLongitude), 
                       decimalLatitude = as.numeric(decimalLatitude)) #SpeciesLink

#plot data to get an overview
ggplot()+ coord_fixed()+ wm +
  geom_point(data = sp14_splk, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "blue", size = 0.5)+
  geom_point(data = sp14, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  
  theme_bw()
#
#Wrapper function. The clean_coordinates function is a wrapper around a large set of automated cleaning steps to flag errors

##convert country code from ISO2c to ISO3c
sp14$countryCode <-  countrycode(sp14$countryCode, origin =  'iso2c', destination = 'iso3c')
sp14 <- data.frame(sp14)
sp14_flags <- clean_coordinates(x = sp14,
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                          "zeros", "countries"))
summary(sp14_flags)
plot(sp14_flags, lon = "decimalLongitude", lat = "decimalLatitude")

#flagged rows
sp14_fl <- sp14[!sp14_flags$.summary,]

#Cleaned data frame.TO USE
sp14_cl <- sp14[sp14_flags$.summary,]
