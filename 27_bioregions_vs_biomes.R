#spatial concordance between Platyrrhini bioregions and
#biomes (shapefile from Antonelli et al. 2018),
#bioregions and subregions (Morrone 2014)

library(udunits2)
library(sabre)
library(sf)

#Map 1 reference map (column name = ECO_NAME)
shp_wwf_biomes <- st_read("data/hibrid_wwf_experts/hibrid_wwf_experts.shp")
     
#Map 1B reference map (column name = Subregio_1 or Province_1)
shp_morrone <- st_read("data/Lowenberg_Neto_2014_shapefile/Lowenberg_Neto_2014.shp")
shp_morrone84 <- st_transform(shp_morrone, crs = 4326)

#Map 1C reference map (column name = BIOME or ECO_NAME)
shp_wwf_official <- st_read("data/wwf_official_teow/official/wwf_terr_ecos_neo.shp")
shp_wwf_official2 <- st_make_valid(shp_wwf_official)

#Map 1D reference map (column name = Label)
shp_interfluvial <- st_read("data/InterfluvialZones/Interfluvial_zones.shp")
shp_interfluvial2 <- st_make_valid(shp_interfluvial)

#Map 2 tested map (column name = bioregio)
shp_bioregions <- st_read("data/Platyrhinni_bioregions_shapefile/Platyrhinni/Platyrhinni.shp") 


#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
#BIOME vs PLATY

vm_output <- vmeasure_calc(shp_wwf_biomes, shp_bioregions, ECO_NAME, bioregio)
vm_output

plot(vm_output$map1) 
plot(vm_output$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
# 1) BIOME vs PLATY

gof_output <- mapcurves_calc(x = shp_wwf_biomes, y = shp_bioregions, x_name = ECO_NAME, y_name = bioregio)
gof_output

plot(gof_output$map1) 
plot(gof_output$map2)

#tests (inverting Map 1 and 2)

vmeasure_calc(shp_bioregions, shp_wwf_biomes, bioregio, ECO_NAME)
mapcurves_calc(y = shp_wwf_biomes, x = shp_bioregions, y_name = ECO_NAME, x_name = bioregio)

#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
# 2) MORRONE_SUBREGION vs PLATY

vm_output2 <- vmeasure_calc(shp_morrone84, shp_bioregions, Subregio_1, bioregio)
vm_output2

plot(vm_output2$map1) 
plot(vm_output2$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
#MORRONE_SUBREGION vs PLATY

gof_output2 <- mapcurves_calc(x = shp_morrone84, y = shp_bioregions, x_name = Subregio_1, y_name = bioregio)
gof_output2

plot(gof_output2$map1) 
plot(gof_output2$map2)


#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
# 3) MORRONE_PROVINCES vs PLATY

vm_output3 <- vmeasure_calc(shp_morrone84, shp_bioregions, Province_1, bioregio)
vm_output3

plot(vm_output3$map1) 
plot(vm_output3$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
#MORRONE_SUBREGION vs PLATY

gof_output3 <- mapcurves_calc(x = shp_morrone84, y = shp_bioregions, x_name = Province_1, y_name = bioregio)
gof_output3

plot(gof_output3$map1) 
plot(gof_output3$map2)

#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
# 4) WWF OFFICIAL BIOME vs PLATY

vm_output4 <- vmeasure_calc(shp_wwf_official2, shp_bioregions, BIOME, bioregio)
vm_output4

plot(vm_output4$map1) 
plot(vm_output4$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
#MWWF OFFICIAL BIOME vs PLATY

gof_output5 <- mapcurves_calc(x = shp_wwf_official2, y = shp_bioregions, x_name = BIOME, y_name = bioregio)
gof_output5

plot(gof_output5$map1) 

#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
# 5) WWF OFFICIAL ECOREGIONS vs PLATY

vm_output6 <- vmeasure_calc(shp_wwf_official2, shp_bioregions, ECO_NAME, bioregio)
vm_output6

plot(vm_output6$map1) 
plot(vm_output6$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
#MWWF OFFICIAL ECOREGION vs PLATY

gof_output7 <- mapcurves_calc(x = shp_wwf_official2, y = shp_bioregions, x_name = ECO_NAME, y_name = bioregio)
gof_output7

plot(gof_output7$map1) 
plot(gof_output7$map2)

#V-measure (https://jakubnowosad.com/posts/2018-09-10-sabre-bp/)
# 6) INTERFLUVIAL vs PLATY

vm_output8 <- vmeasure_calc(shp_interfluvial2, shp_bioregions, Label, bioregio)
vm_output8

plot(vm_output8$map1) 
plot(vm_output8$map2)

#GOF Mapcurves (https://www.rdocumentation.org/packages/sabre/versions/0.4.3/topics/mapcurves_calc)
#INTERFLUVIAL vs PLATY

gof_output9 <- mapcurves_calc(x = shp_interfluvial2, y = shp_bioregions, x_name = Label, y_name = bioregio)
gof_output9

plot(gof_output9$map1) 
plot(gof_output9$map2)

#test
vm_output10 <- vmeasure_calc(shp_bioregions, shp_bioregions, bioregio, bioregio)
vm_output10

plot(vm_output10$map1) 
plot(vm_output10$map2)
