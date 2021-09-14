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

