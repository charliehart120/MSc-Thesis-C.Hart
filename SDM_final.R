#####Charlie Hart Thesis - CID: 02009906#####

#Install packages
install.packages("sdm")
library(dplyr)
library(stringr)
library(spThin)
library(raster)
library(tidyr)
library(sp)
library(usdm)
library(sdm)
library(rgdal)
library(ggplot2)
library(ggpubr)
installAll()



#Set working directory
setwd("C:/Users/charl/Desktop/Imperial/Research project/Habitat association/Data")

#####Data prep####

#Read in merged data for adding XY coordinates
merged_data <- read.csv("sdm_data/merged_detections1.csv")
merged_locs <- read.csv("sdm_data/merged_locs.csv")

#add locations to detection data
#remove _ and spaces from names
xy <- merged_locs %>%
  mutate(Site=str_replace_all(Site," ",""))%>%
  mutate(Site=str_replace_all(Site,"_",""))

merged_dets <- merged_data %>%
  mutate(Site=str_replace_all(Site," ",""))%>%
  mutate(Site=str_replace_all(Site,"_",""))

#join locations with detections
data <- merged_dets %>%
  left_join(xy)


#### Spatial thinning dataset ####

#spatial thinning 0.5k
xy_0.5 <- thin(
  data,
  lat.col = "Y",
  long.col = "X",
  spec.col = "Species",
  0.5,
  1,
  locs.thinned.list.return = FALSE,
  write.files = TRUE,
  max.files = 5,
  "C:/Users/charl/Desktop/Imperial/Research project/Habitat association/Data/sdm_data",
  out.base = "data_0.5.csv",
  write.log.file = TRUE,
  log.file = "spatial_thin_log",
  verbose = TRUE
)
#after thinning 282 sites remaining

#Read thinned data
data_0.5 <- xy_0.5[2:3]

#merge detections to thinned locs - (Filtered to 500m)
filter_data <- data_0.5 %>% 
  left_join(data)
View(filter_data)

#write final data set for analysis
write.csv(filter_data,"sdm_data/filter_data_0.5_1.csv")

#read final data for analysis after amending some names in Excel
final <- read.csv("sdm_data/Data for analysis.csv")
View(final)



####Species presence/absence####

####Clouded leopard####
#count detections at each site
CL <-
  final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Sunda clouded leopard")

#change count to 0 or 1
CL_pa <- CL %>% mutate(n=ifelse(n>0,1,0))

#rejoin XY data
CL_pa_1 <- merge(CL_pa, xy, by="Site")

#count presence sites (37)
CL_pa_1 %>% filter(n==1)

#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- CL_pa_1[ , c("X", "Y")]   # coordinates
data   <- CL_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_clPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_clPA)

# plot spdf 
spplot(spdf_clPA, "n")
plot(spdf_clPA)
head(spdf_clPA)
plot(spdf_clPA[spdf_clPA$n == 1,],col='blue',pch=16)
plot(spdf_clPA[spdf_clPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_clPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_1.shp',overwrite=TRUE)


#Sun bear#

#Sun bear
#count detections at each site
SB <-
  final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Sun bear")

#change count to 0 or 1
SB_pa <- SB %>% mutate(n=ifelse(n>0,1,0))

View(SB_pa_1)

#rejoin XY data
SB_pa_1 <- merge(SB_pa, xy, by="Site")

#count presence sites (73)
SB_pa_1 %>% filter(n==1)

#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- SB_pa_1[ , c("X", "Y")]   # coordinates
data   <- SB_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_SBPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_SBPA)


# plot spdf 
spplot(spdf_SBPA, "n")
plot(spdf_SBPA)

head(spdf_SBPA)

plot(spdf_SBPA[spdf_SBPA$n == 1,],col='blue',pch=16)
plot(spdf_SBPA[spdf_SBPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_SBPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)
#write presence/absence shapefile

shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_2.shp',overwrite=TRUE)



##Asian elephant##

#count detections at each site

AE <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Asian Elephant")

#change count to 0 or 1
AE_pa <- AE %>% mutate(n=ifelse(n>0,1,0))

View(AE_pa)

#rejoin XY data
AE_pa_1 <- merge(AE_pa, xy, by="Site")

#count presence sites (10)
AE_pa_1 %>% filter(n==1)

#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- AE_pa_1[ , c("X", "Y")]   # coordinates
data   <- AE_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_AEPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_AEPA)

# plot spdf 
spplot(spdf_AEPA, "n")
plot(spdf_AEPA)

head(spdf_AEPA)

plot(spdf_AEPA[spdf_AEPA$n == 1,],col='blue',pch=16)
plot(spdf_AEPA[spdf_AEPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_AEPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_3.shp',overwrite=TRUE)



##Sambar deer#

#count detections at each site

SD <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Sambar deer")

#change count to 0 or 1
SD_pa <- SD %>% mutate(n=ifelse(n>0,1,0))

View(SD_pa)

#rejoin XY data
SD_pa_1 <- merge(SD_pa, xy, by="Site")

#count presence sites (138)
SD_pa_1 %>% filter(n==1)
View(SD_pa_1)
#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- SD_pa_1[ , c("X", "Y")]   # coordinates
data   <- SD_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_SDPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_SDPA)

# plot spdf 
spplot(spdf_SDPA, "n")
plot(spdf_SDPA)

head(spdf_SDPA)

plot(spdf_SDPA[spdf_SDPA$n == 1,],col='blue',pch=16)
plot(spdf_SDPA[spdf_SDPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_SDPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_4.shp',overwrite=TRUE)

##Bearded pig##

#count detections at each site

BP <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Bearded pig")

#change count to 0 or 1
BP_pa <- BP %>% mutate(n=ifelse(n>0,1,0))

View(BP_pa)

#rejoin XY data
BP_pa_1 <- merge(BP_pa, xy, by="Site")

#count presence sites (216)
BP_pa_1 %>% filter(n==1)

View(BP_pa_1)
#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- BP_pa_1[ , c("X", "Y")]   # coordinates
data   <- BP_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_BPPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_BPPA)

# plot spdf 
spplot(spdf_BPPA, "n")
plot(spdf_BPPA)

head(spdf_BPPA)

plot(spdf_BPPA[spdf_BPPA$n == 1,],col='blue',pch=16)
plot(spdf_BPPA[spdf_BPPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_BPPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_6.shp',overwrite=TRUE)


##Bornean yellow muntjac##

#count detections at each site

YM <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Bornean yellow muntjac")

#change count to 0 or 1
YM_pa <- YM %>% mutate(n=ifelse(n>0,1,0))

View(YM_pa)

#rejoin XY data
YM_pa_1 <- merge(YM_pa, xy, by="Site")

#count presence sites (117)
YM_pa_1 %>% filter(n==1)
View(YM_pa_1)
#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- YM_pa_1[ , c("X", "Y")]   # coordinates
data   <- YM_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_YMPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_YMPA)

# plot spdf 
spplot(spdf_YMPA, "n")
plot(spdf_YMPA)

head(spdf_YMPA)

plot(spdf_YMPA[spdf_YMPA$n == 1,],col='blue',pch=16)
plot(spdf_YMPA[spdf_YMPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_YMPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_7.shp',overwrite=TRUE)


##Orangutan##

#count detections at each site

OU <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Orang utan")

#change count to 0 or 1
OU_pa <- OU %>% mutate(n=ifelse(n>0,1,0))

View(OU_pa)

#rejoin XY data
OU_pa_1 <- merge(OU_pa, xy, by="Site")

#count presence sites (37)
OU_pa_1 %>% filter(n==1)
View(OU_pa_1)
#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- OU_pa_1[ , c("X", "Y")]   # coordinates
data   <- OU_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_OUPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_OUPA)

# plot spdf 
spplot(spdf_OUPA, "n")
plot(spdf_OUPA)

head(spdf_OUPA)

plot(spdf_OUPA[spdf_OUPA$n == 1,],col='blue',pch=16)
plot(spdf_OUPA[spdf_OUPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_OUPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

#write presence/absence shapefile
shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_8.shp',overwrite=TRUE)



##Banded civet##

#count detections at each site

BC <- final %>% 
  group_by(Site,Species) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  complete(Site,Species,fill=list(n=0)) %>%
  filter(Species=="Banded civet")

#change count to 0 or 1
BC_pa <- BC %>% mutate(n=ifelse(n>0,1,0))

View(BC_pa)

#rejoin XY data
BC_pa_1 <- merge(BC_pa, xy, by="Site")

#count presence sites (54)
BC_pa_1 %>% filter(n==1)
View(BC_pa_1)
#prepare spatial points data frame (SPDF)

# prepare coordinates, data, and projection
coords <- BC_pa_1[ , c("X", "Y")]   # coordinates
data   <- BC_pa_1[3]          # data
crs    <- CRS("+init=epsg:4326") # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_BCPA <- SpatialPointsDataFrame(coords      = coords,
                                    data        = data, 
                                    proj4string = crs)
# check the object class
class(spdf_BCPA)

# plot spdf 
spplot(spdf_BCPA, "n")
plot(spdf_BCPA)

head(spdf_BCPA)

plot(spdf_BCPA[spdf_BCPA$n == 1,],col='blue',pch=16)
plot(spdf_BCPA[spdf_BCPA$n == 0,],col='red',pch=16)


#convert projection of spdf
# UTM50N (EPSG:32650)
#transform species data
geo_proj = " +proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"

species_trnsfrmd = spTransform(spdf_BCPA,geo_proj)
species_trnsfrmd

extent(species_trnsfrmd)
head(species_trnsfrmd)

shapefile(species_trnsfrmd, filename='sdm_data/species_shp_files/Species_9.shp',overwrite=TRUE)


####'Scale of effect' analysis####

#read presence/absence spdf data
species1 <- shapefile("species_shp_files/Species_9.shp")
plot(species1)

#Finding best spatial scale for covariates
#Access
access <- raster("Covariates/access/access_ext.TXT")
access_250m <- raster("Covariates/access/access_250m.TXT")
access_500m <- raster("Covariates/access/access_500m.TXT")
access_1k <- raster("Covariates/access/access_1k.TXT")
access_2k <- raster("Covariates/access/access_2k.TXT")
access_4k <- raster("Covariates/access/access_4k.TXT")
access_8k <- raster("Covariates/access/access_8k.TXT")

#create rasterstack
access_stack <- stack(access,access_250m,access_500m,access_1k,access_2k,access_4k,access_8k)

#prepare data for model
d <- sdmData(train=species1, predictors=access_stack)

#Model fitting
m2_access <- sdm(n~.,data=d, methods = c('glm','rf'),replication='sub',test.percent=30,n=10)

#Canopy height
CH <- raster("Covariates/canopy height/ch.txt")
CH_250m <- raster("Covariates/canopy height/ch_250m.txt")
CH_500m <- raster("Covariates/canopy height/ch_500m.txt")
CH_1k <- raster("Covariates/canopy height/ch_1k.txt")
CH_2k <- raster("Covariates/canopy height/ch_2k.txt")
CH_4k <- raster("Covariates/canopy height/ch_4k.txt")
CH_8k <- raster("Covariates/canopy height/ch_8k.txt")

#create rasterstack
CH_stack <- stack(CH,CH_250m,CH_500m,CH_1k,CH_2k,CH_4k,CH_8k)

#prepare data for model
d <- sdmData(train=species1, predictors=CH_stack)

#Model fitting
m2_CH <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
             test.percent=30,n=10)

#dist_agroforestry
dist_agro <- raster("Covariates/dist_agro/dist_agroforestry_ext.TXTdist_agro.50.asc")
dist_agro_250 <- raster("Covariates/dist_agro/dist_agroforestry_250m.TXTdist_agro.50.asc")
dist_agro_500 <- raster("Covariates/dist_agro/dist_agroforestry_500m.TXTdist_agro.50.asc")
dist_agro_1k <- raster("Covariates/dist_agro/dist_agroforestry_1k.TXTdist_agro.50.asc")
dist_agro_2k <- raster("Covariates/dist_agro/dist_agroforestry_2k.TXTdist_agro.50.asc")
dist_agro_4k <- raster("Covariates/dist_agro/dist_agroforestry_4k.TXTdist_agro.50.asc")
dist_agro_8k <- raster("Covariates/dist_agro/dist_agroforestry_8k.TXTdist_agro.50.asc")

dist_agro_stack <- stack(dist_agro,dist_agro_250,dist_agro_500,dist_agro_1k,dist_agro_2k,dist_agro_4k,dist_agro_8k)

d <- sdmData(train=species1, predictors=dist_agro_stack)

m2_dist_agro <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                    test.percent=30,n=10)

#dist_deg
dist_deg <- raster("Covariates/dist_deg_forest/dist_degraded_forest_ext.50.asc")
dist_deg_250 <- raster("Covariates/dist_deg_forest/dist_degraded_forest_250m.50.asc")
dist_deg_500 <- raster("Covariates/dist_deg_forest/dist_degraded_forest_500m.50.asc")
dist_deg_1k <- raster("Covariates/dist_deg_forest/dist_degraded_forest_1k.50.asc")
dist_deg_2k <- raster("Covariates/dist_deg_forest/dist_degraded_forest_2k.50.asc")
dist_deg_4k <- raster("Covariates/dist_deg_forest/ddist_degraded_forest_4k.50.asc")
dist_deg_8k <- raster("Covariates/dist_deg_forest/dist_degraded_forest_8k.50.asc")

dist_deg_stack <- stack(dist_deg,dist_deg_250, dist_deg_500,dist_deg_1k,dist_deg_2k,dist_deg_4k,dist_deg_8k)

d <- sdmData(train=species1, predictors=dist_deg_stack)

m2_dist_deg <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                   test.percent=30,n=10)

#dist_forest
dist_forest <- raster("Covariates/dist_forest/dist_forest_ext_agg.asc")
dist_forest_250 <- raster("Covariates/dist_forest/dist_forest_250m.TXTdist_forest.50.asc")
dist_forest_500 <- raster("Covariates/dist_forest/dist_forest_500m.TXTdist_forest.50.asc")
dist_forest_1k <- raster("Covariates/dist_forest/dist_forest_1k.TXTdist_forest.50.asc")
dist_forest_2k <- raster("Covariates/dist_forest/dist_forest_2k.TXTdist_forest.50.asc")
dist_forest_4k <- raster("Covariates/dist_forest/dist_forest_4k.TXTdist_forest.50.asc")
dist_forest_8k <- raster("Covariates/dist_forest/dist_forest_8k.TXTdist_forest.50.asc")

dist_forest_stack <- stack(dist_forest,dist_forest_250, dist_forest_500,dist_forest_1k,dist_forest_2k,dist_forest_4k,dist_forest_8k)

d <- sdmData(train=species1, predictors=dist_forest_stack)

m2_dist_forest <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                      test.percent=30,n=10)

##dist_low-mod
dist_low_mod <- raster("Covariates/dist_low_mod/dist_low_mod_extdist_low_mod.50.asc")
dist_low_mod_250 <- raster("Covariates/dist_low_mod/dist_low_mod_250mdist_low_mod.50.asc")
dist_low_mod_500 <- raster("Covariates/dist_low_mod/dist_low_mod_500mdist_low_mod.50.asc")
dist_low_mod_1k <- raster("Covariates/dist_low_mod/dist_low_mod_1kdist_low_mod.50.asc")
dist_low_mod_2k <- raster("Covariates/dist_low_mod/dist_low_mod_2kdist_low_mod.50.asc")
dist_low_mod_4k <- raster("Covariates/dist_low_mod/dist_low_mod_4kdist_low_mod.50.asc")
dist_low_mod_8k <- raster("Covariates/dist_low_mod/dist_low_mod_8kdist_low_mod.50.asc")

dist_low_mod_stack <- stack(dist_low_mod,dist_low_mod_250,dist_low_mod_500,dist_low_mod_1k,dist_low_mod_2k,dist_low_mod_4k,dist_low_mod_8k)

d <- sdmData(train=species1, predictors=dist_low_mod_stack)

m2_dist_low_mod <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                       test.percent=30,n=10)

##dist_mod_high
mh1 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_extdist_mod_high.50.asc")
mh2 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_250mdist_mod_high.50.asc")
mh3 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_500mdist_mod_high.50.asc")
mh4 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_1kdist_mod_high.50.asc")
mh5 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_2kdist_mod_high.50.asc")
mh6 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_4kdist_mod_high.50.asc")
mh7 <- raster("Covariates/dist_mod_high/dist_moderate_high_condition_forest_8kdist_mod_high.50.asc")
mhstack <- stack(mh1,mh2,mh3,mh4,mh5,mh6,mh7)  
d <- sdmData(train=species1, predictors=mhstack)
m2_dist_mod_high <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                        test.percent=30,n=10)

##dist_non-forest in forest
nf_ext <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forestdist_non_forest.50.asc")
nf_250 <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_250mdist_non_forest.50.asc")
nf_500 <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_500mdist_non_forest.50.asc")
nf_1k <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_1kdist_non_forest.50.asc")
nf_2k <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_2kdist_non_forest.50.asc")
nf_4k <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_4kdist_non_forest.50.asc")
nf_8k <- raster("Covariates/dist_non-forest_in_forest/dist_non-forest_in_forest_8kdist_non_forest.50.asc")
nf_stack <- stack(nf_ext,nf_250,nf_500,nf_1k,nf_2k,nf_4k,nf_8k)
d <- sdmData(train=species1, predictors=nf_stack)
m2_dist_nf_in_f <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                       test.percent=30,n=10)

#dist_oil palm
op_ext <- raster("Covariates/dist_OP/distance_palm_oil_extdist_OP.50.asc")
op_250 <- raster("Covariates/dist_OP/distance_palm_oil_250mdist_OP.50.asc")
op_500 <- raster("Covariates/dist_OP/distance_palm_oil_500mdist_OP.50.asc")
op_1k <- raster("Covariates/dist_OP/distance_palm_oil_1kdist_OP.50.asc")
op_2k <- raster("Covariates/dist_OP/distance_palm_oil_2kdist_OP.50.asc")
op_4k <- raster("Covariates/dist_OP/distance_palm_oil_4kdist_OP.50.asc")
op_8k <- raster("Covariates/dist_OP/distance_palm_oil_8kdist_OP.50.asc")
op_stack <- stack(op_ext,op_250,op_500,op_1k,op_2k,op_4k,op_8k)
d <- sdmData(train=species1, predictors=op_stack)
m2_op <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
             test.percent=30,n=10)

#dist_river
dist_river_ext <- raster("Covariates/dist_river/dist_river_extdist_river.50.asc")
dist_river_250 <- raster("Covariates/dist_river/dist.river.agg.50.250.asc")
dist_river_500 <- raster("Covariates/dist_river/dist_river_500mdist_river.50.asc")
dist_river_1k <- raster("Covariates/dist_river/dist_river_1kdist_river.50.asc")
dist_river_2k <- raster("Covariates/dist_river/dist_river_2kdist_river.50.asc")
dist_river_4k <- raster("Covariates/dist_river/dist_river_4kdist_river.50.asc")
dist_river_8k <- raster("Covariates/dist_river/dist_river_8kdist_river.50.asc")
dist_river_stack <- stack(dist_river_ext,dist_river_250,dist_river_500,dist_river_1k,dist_river_2k,dist_river_4k,dist_river_8k)
d <- sdmData(train=species1, predictors=dist_river_stack)
m2_dist_river <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                     test.percent=30,n=10)

#dist_main roads
dist_road_ext <- raster("Covariates/dist_road/distance_main-roads_extdist_road.50.asc")
dist_road_250 <- raster("Covariates/dist_road/distance_main-roads_250mdist_road.50.asc")
dist_road_500 <- raster("Covariates/dist_road/distance_main-roads_500mdist_road.50.asc")
dist_road_1k <- raster("Covariates/dist_road/distance_main-roads_1kdist_road.50.asc")
dist_road_2k <- raster("Covariates/dist_road/distance_main-roads_2kdist_road.50.asc")
dist_road_4k <- raster("Covariates/dist_road/distance_main-roads_4kdist_road.50.asc")
dist_road_8k <- raster("Covariates/dist_road/distance_main-roads_8kdist_road.50.asc")
dist_road_stack <- stack(dist_road_ext,dist_agro_250,dist_road_500,dist_road_1k,dist_road_2k,dist_road_4k,dist_road_8k)
d <- sdmData(train=species1, predictors=dist_road_stack)
m2_dist_road <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                    test.percent=30,n=10)

#elevation
el_ext <- raster("Covariates/elevation/elevation_extelevation.50.asc")
el_250 <- raster("Covariates/elevation/elevation_250melevation.50.asc")
el_500 <- raster("Covariates/elevation/elevation_500melevation.50.asc")
el_1k <- raster("Covariates/elevation/elevation_1kelevation.50.asc")
el_2k <- raster("Covariates/elevation/elevation_2kelevation.50.asc")
el_4k <- raster("Covariates/elevation/elevation_4kelevation.50.asc")
el_8k <- raster("Covariates/elevation/elevation_8kelevation.50.asc")
el_stack <- stack(el_ext,el_250,el_500,el_1k,el_2k,el_4k,el_8k)
d <- sdmData(train=species1, predictors=el_stack)
m2_elevation <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                    test.percent=30,n=10)

#settlements
sett_ext <- raster("Covariates/settlements/settlements_ext.txt")
sett_250 <- raster("Covariates/settlement/settlements_250m.txt")
sett_500 <- raster("Covariates/settlements/settlements_500m.txt")
sett_1k <- raster("Covariates/settlements/settlements_1k.txt")
sett_2k <- raster("Covariates/settlements/settlements_2k.txt")
sett_4k <- raster("Covariates/settlements/settlements_4k.txt")
sett_8k <- raster("Covariates/settlements/settlements_8k.txt")
sett_stack <- stack(sett_ext,sett_250,sett_500,sett_1k,sett_2k,sett_4k,sett_8k)
d <- sdmData(train=species1, predictors=sett_stack)
m2_settlements <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                      test.percent=30,n=10)

#slope
slope_ext <- raster("Covariates/slope/slope_ext_agg.asc")
slope_250 <- raster("Covariates/slope/slope_250mslope.50.asc")
slope_500 <- raster("Covariates/slope/slope_500mslope.50.asc")
slope_1k <- raster("Covariates/slope/slope_1kslope.50.asc")
slope_2k <- raster("Covariates/slope/slope_2kslope.50.asc")
slope_4k <- raster("Covariates/slope/slope_4kslope.50.asc")
slope_8k <- raster("Covariates/slope/slope_8k_agg.asc")
slope_stack <- stack(slope_ext,slope_250,slope_500,slope_1k,slope_2k,slope_4k,slope_8k)
d <- sdmData(train=species1, predictors=slope_stack)
m2_slope <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                test.percent=30,n=10)

#Land use
landuse <- raster("Covariates/land use/lulc_ext.txt")
landuse_250m <- raster("Covariates/land use/lulc_250m.txt")
landuse_500m <- raster("Covariates/land use/lulc_500m.txt")
landuse_1k <- raster("Covariates/land use/lulc_1k.txt")
landuse_2k <- raster("Covariates/land use/lulc_2k.txt")
landuse_4k <- raster("Covariates/land use/lulc_4k.txt")
landuse_8k <- raster("Covariates/land use/lulc_8k.txt")
landuse_stack <- stack(landuse,landuse_250m,landuse_500m,landuse_1k,landuse_2k,landuse_4k,landuse_8k)
d <- sdmData(train=species1, predictors=landuse_stack)
m2_landuse <- sdm(n~.,data=d,methods = c('glm','rf'),replicatin='sub',
                  test.percent=30,n=10)


#get variable importance - most strongly correlated scale used for further modelling
getVarImp(m2_access, id=2, wtest = 'training')
getVarImp(m2_CH, id=2, wtest = 'training')
getVarImp(m2_dist_agro, id=2, wtest = 'training')
getVarImp(m2_dist_deg, id=2, wtest = 'training')
getVarImp(m2_dist_forest, id=2, wtest = 'training')
getVarImp(m2_dist_low_mod, id=2, wtest = 'training')
getVarImp(m2_dist_mod_high, id=2, wtest = 'training')
getVarImp(m2_dist_nf_in_f, id=2, wtest = 'training')
getVarImp(m2_op, id=2, wtest = 'training')
getVarImp(m2_dist_river, id=2, wtest = 'training')
getVarImp(m2_dist_road, id=2, wtest = 'training')
getVarImp(m2_elevation, id=2, wtest = 'training')
getVarImp(m2_settlements, id=2, wtest = 'training')
getVarImp(m2_slope, id=2, wtest = 'training')
getVarImp(m2_landuse, id=2, wtest = 'training')

#repeat for each species

####SDM####
#set working directory
setwd("C:/C_Hart")

#read species data - clouded leopard
species1 <- shapefile("species_shp_files/Species_1.shp")

#read scaled covaritates
dist_forest <- raster("Covariates/CL_covs/dist_forest_500m.asc")
dist_main_road <- raster("Covariates/CL_covs/dist_main-roads_8k.asc")
elevation <- raster("Covariates/CL_covs/elevation_250m.asc")
dist_river <- raster("Covariates/CL_covs/dist_river_50m.asc")
dist_palm_oil <- raster("Covariates/CL_covs/distance_palm_oil_50m.asc")
dist_agroforestry <- raster("Covariates/CL_covs/dist_agroforestry_1k.asc")
dist_degraded_forest_250m <- raster("Covariates/CL_covs/dist_degraded_forest_4k.asc")
landuse <- raster("Covariates/CL_covs/land_use_8k.txt")
ch <- raster("Covariates/CL_covs/canopy_height_2k.txt")
dist_mod_high_condition_forest <- raster("Covariates/CL_covs/dist_moderate_high_condition_forest_500mdist_mod_high.50.asc")
access <- raster("Covariates/CL_covs/access_250m.txt")
slope <- raster("Covariates/CL_covs/slope_2ks.asc")
dist_low_mod_forest <- ("Covariates/CL_covs/dist_low_mod_500mdist_low_mod.50.asc")
settlements <- raster("Covariates/CL_covs/settlements_8k.txt")


#stacking for analysis
env<-stack(landuse,settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest_250m, dist_mod_high_condition_forest,ch)
env

#VIF to remove multicollinearity
vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(landuse,settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest_250m, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod high was high VIF (5.5) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest_250m,ch,landuse)
vif(env3)

#prepare data for model
d <- sdmData(train=species1, predictors=env3)

# run with train and test data and 7 algorithms
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','gam','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

#run with best performing algorithms
m3 <- sdm(n~.,data=d,methods=c('rf','brt','fda','glm'),replicatin='sub',
          test.percent=30,n=20)
m3

#get variable importance
vi_CL <- getVarImp(m3)
VI_cl <- plot(vi1,'cor')


# make prediction map using ensemble of best performing models
e2 <- ensemble(m3,newdata=env3,filename='species_data/hsm/ensemble_plot_CL.img',setting=list(method='weighted',stat='AUC'))
plot(e2)


#species 2 -sun bear
#read species data
species2 <- shapefile("species_shp_files/Species_2.shp")
plot(species2[species2$n ==1,],col='blue',pch=16)
plot(species2[species2$n ==0,],col='red',pch=16, add=T)

#read covariates
dist_forest <- raster("Covariates/SB_covs/dist_forest_250m.asc")
dist_main_road <- raster("Covariates/SB_covs/distance_main-roads_500m.asc")
elevation <- raster("Covariates/SB_covs/elevation_500m.asc")
dist_river <- raster("Covariates/SB_covs/dist_river_250m.asc")
dist_palm_oil <- raster("Covariates/SB_covs/distance_palm_oil_50m.asc")
dist_agroforestry <- raster("Covariates/SB_covs/dist_agroforestry_250m.asc")
dist_degraded_forest <- raster("Covariates/SB_covs/dist_degraded_forest_8k.asc")
landuse <- raster("Covariates/SB_covs/land_use_1k.txt")
ch <- raster("Covariates/SB_covs/canopy_height_250m.txt")
dist_mod_high_condition_forest <- raster("Covariates/SB_covs/dist_moderate_high_condition_forest_250mdist_mod_high.50.asc")
access <- raster("Covariates/SB_covs/access_500m.txt")
slope <- raster("Covariates/SB_covs/slope_8k.asc")
dist_low_mod_forest <- ("Covariates/SB_covs/dist_low_mod_250mdist_low_mod.50.asc")
settlements <- raster("Covariates/SB_covs/settlements_8k.txt")


#stacking for analysis
env<-stack(settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest, landuse, dist_mod_high_condition_forest,ch)
env

#test collinearity
vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,landuse, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod-high condition forest was high VIF (5.4) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,ch,landuse)
vif(env3)

#prepare data
d <- sdmData(train=species2, predictors=env3)

# run with train and test data
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','gam','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

getModelInfo(m2) # info on runs including modelID, whether they are successfully fitted and evaluated, etc.


#variable importance
getVarImp(m2,id=1,wtest='training')
getVarImp(m2,id=2,wtest='training')
getVarImp(m2,id=3,wtest='training')
getVarImp(m2,id=6,wtest='training')

#model 3 - use best performing algorithms 
m3 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt'),replicatin='sub',
          test.percent=30,n=20)
m3

getVarImp(m3)

##Prediction
#predict
e1 <- ensemble(m3,newdata=env3,filename='species_data/hsm/ensemble_SB2.img',setting=list(method='weighted',stat='AUC'))
plot(e1)

#get variable importance
vi_SB <- getVarImp(m3)
VI_SB <- plot(vi1,'cor')


#Species 6 - beaded pig
species6 <- shapefile("species_shp_files/Species_6.shp")

dist_forest <- raster("Covariates/BP_covs/dist_forest_500m.asc")
dist_main_road <- raster("Covariates/BP_covs/dist_main-roads_2k.asc")
elevation <- raster("Covariates/BP_covs/elevation_500m.asc")
dist_river <- raster("Covariates/BP_covs/dist_river_2k.asc")
dist_palm_oil <- raster("Covariates/BP_covs/dist_palm_oil_50m.asc")
dist_agroforestry <- raster("Covariates/BP_covs/dist_agroforestry_500m.asc")
dist_degraded_forest <- raster("Covariates/BP_covs/dist_degraded_forest_1k.asc")
landuse <- raster("Covariates/BP_covs/land_use_500m.txt")
ch <- raster("Covariates/BP_covs/canopy_height_2k.txt")
dist_mod_high_condition_forest <- raster("Covariates/BP_covs/dist_moderate_high_condition_forest_500mdist_mod_high.50.asc")
access <- raster("Covariates/BP_covs/access_500m.txt")
slope <- raster("Covariates/BP_covs/slope_500m.asc")
dist_low_mod_forest <- ("Covariates/BP_covs/dist_low_mod_4kdist_low_mod.50.asc")
settlements <- raster("Covariates/BP_covs/settlements_4k.txt")


#stacking for analysis
env<-stack(settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest, landuse, dist_mod_high_condition_forest,ch)
env

#test multicollinearity 
vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,landuse, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod-high condition forest was high VIF (5.4) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,ch,landuse)
vif(env3)

#prepare data
d <- sdmData(train=species6, predictors=env3)

# run with train and test data
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

m3 <- sdm(n~.,data=d,methods=c('rf','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m3

m4 <- sdm(n~.,data=d,methods=c('rf','brt','svm'),replicatin='sub',
          test.percent=30,n=10)
m4

m5 <- sdm(n~.,data=d,methods=c('brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m5

#predict
e2 <- ensemble(m5,newdata=env3,filename='species_data/hsm/ensemble_Bearded_pig.img',setting=list(method='weighted',stat='AUC'))
plot(e2)
getVarImp(m5)

#get variable importance
vi_BP <- getVarImp(m3)
vi_bp <- plot(vi_BP,'cor', main=NULL)



#Yellow muntjac
species7 <- shapefile("species_shp_files/Species_7.shp")

dist_forest <- raster("Covariates/YM_covs/dist_forest_500m.asc")
dist_main_road <- raster("Covariates/YM_covs/dist_main-roads_2k.asc")
elevation <- raster("Covariates/YM_covs/elevation_500m.asc")
dist_river <- raster("Covariates/YM_covs/dist_river_500m.asc")
dist_palm_oil <- raster("Covariates/YM_covs/dist_palm_oil_4k.asc")
dist_agroforestry <- raster("Covariates/YM_covs/dist_agroforestry_500m.asc")
dist_degraded_forest <- raster("Covariates/YM_covs/dist_degraded_forest_1k.asc")
landuse <- raster("Covariates/YM_covs/land_use_4k.txt")
ch <- raster("Covariates/YM_covs/canopy_height_500m.txt")
dist_mod_high_condition_forest <- raster("Covariates/YM_covs/dist_moderate_high_condition_forest_500mdist_mod_high.50.asc")
access <- raster("Covariates/YM_covs/access_250m.txt")
slope <- raster("Covariates/YM_covs/slope_4k.asc")
dist_low_mod_forest <- ("Covariates/YM_covs/dist_low_mod_8k.asc")
settlements <- raster("Covariates/YM_covs/settlements_500m.txt")

#stacking for analysis
env<-stack(settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest, landuse, dist_mod_high_condition_forest,ch)
env

vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,landuse, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod-high condition forest was high VIF (5.4) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,ch,landuse)
vif(env3)

#prepare data
d <- sdmData(train=species7, predictors=env3)

# run with train and test data
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

#variable importance plot
getVarImp(m)
vi1 <- getVarImp(m2)
plot(vi1,'cor')
 
#predict
e2 <- ensemble(m4,newdata=env3,filename='species_data/hsm/ensemble_Yellow_muntjac',setting=list(method='weighted',stat='AUC'))
plot(e2)

#get variable importance
vi_ym1 <- getVarImp(m2)
vi_ym <- plot(vi_ym1,'cor')




#species 8 - Orang utan
species8 <- shapefile("species_shp_files/Species_8.shp")

dist_forest <- raster("Covariates/OU_covs/dist_forest_250m.asc")
dist_main_road <- raster("Covariates/OU_covs/dist_main-roads_2k.asc")
elevation <- raster("Covariates/OU_covs/elevation_1k.asc")
dist_river <- raster("Covariates/OU_covs/dist_river_2k.asc")
dist_palm_oil <- raster("Covariates/OU_covs/dist_palm_oil_2k.asc")
dist_agroforestry <- raster("Covariates/OU_covs/dist_agroforestry_1k.asc")
dist_degraded_forest <- raster("Covariates/OU_covs/dist_degraded_forest_8k.asc")
landuse <- raster("Covariates/OU_covs/land_use_1k.txt")
ch <- raster("Covariates/OU_covs/canopy_height_1k.txt")
dist_mod_high_condition_forest <- raster("Covariates/OU_covs/dist_moderate_high_condition_forest_250mdist_mod_high.50.asc")
access <- raster("Covariates/OU_covs/access_2k.txt")
slope <- raster("Covariates/OU_covs/slope_8k.asc")
dist_low_mod_forest <- ("Covariates/OU_covs/dist_low_mod_1k.asc")
settlements <- raster("Covariates/OU_covs/settlements_50m.txt")

#stacking for analysis
env<-stack(settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest, landuse, dist_mod_high_condition_forest,ch)
env

#test multicollinearity
vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,landuse, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod-high condition forest was high VIF (5.4) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,ch,landuse)
vif(env3)

#prepare data
d <- sdmData(train=species8, predictors=env3)

# run with train and test data
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

m3 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt'),replicatin='sub',
          test.percent=30,n=20)
m3

m4 <- sdm(n~.,data=d,methods=c('rf','brt'),replicatin='sub',
          test.percent=30,n=20)
m4

m5 <- sdm(n~.,data=d,methods=c('glm'),replicatin='sub',
          test.percent=30)
m5

#predict
e2 <- ensemble(m3,newdata=env3,filename='species_data/hsm/ensemble_Orangutan',setting=list(method='weighted',stat='AUC'))
plot(e2)
getVarImp(m5)

e3 <- ensemble(m4,newdata=env3,filename='species_data/hsm/ensemble_Orangutan_2',setting=list(method='weighted',stat='AUC'))
plot(e3)

e3 <- predict(m5,newdata=env3,mean=T)
plot(e3)

#get variable importance
vi_OU <- getVarImp(m4)
vi_ou <- plot(vi_OU,'cor')




#speceis 9 - Banded civet
species9 <- shapefile("species_shp_files/Species_9.shp")

dist_forest <- raster("Covariates/BC_covs/dist_forest_250m.asc")
dist_main_road <- raster("Covariates/BC_covs/dist_main-roads_2k.asc")
elevation <- raster("Covariates/BC_covs/elevation_250m.asc")
dist_river <- raster("Covariates/BC_covs/dist_river_4k.asc")
dist_palm_oil <- raster("Covariates/BC_covs/dist_palm_oil_2k.asc")
dist_agroforestry <- raster("Covariates/BC_covs/dist_agroforestry_50m.asc")
dist_degraded_forest <- raster("Covariates/BC_covs/dist_degraded_forest_500m.asc")
landuse <- raster("Covariates/BC_covs/land_use_2k.txt")
ch <- raster("Covariates/BC_covs/canopy_height_500m.txt")
dist_mod_high_condition_forest <- raster("Covariates/BC_covs/dist_moderate_high_condition_forest_500mdist_mod_high.50.asc")
access <- raster("Covariates/BC_covs/access_250m.txt")
slope <- raster("Covariates/BC_covs/slope_500m.asc")
dist_low_mod_forest <- ("Covariates/BC_covs/dist_low_mod_extdist_low_mod.50.asc")
settlements <- raster("Covariates/BC_covs/settlements_4k.txt")

#stacking for analysis
env<-stack(settlements,dist_low_mod_forest,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest, landuse, dist_mod_high_condition_forest,ch)
env

vif(env)

#dist low mod was very high VIF (15.59) so remove from analysis
env2<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,landuse, dist_mod_high_condition_forest,ch)
vif(env2)
#dist mod-high condition forest was high VIF (5.4) so remove from analysis
env3<-stack(settlements,slope,access,elevation, dist_forest,dist_main_road, dist_river, dist_agroforestry,dist_palm_oil, dist_degraded_forest,ch,landuse)
vif(env3)

#prepare data
d <- sdmData(train=species9, predictors=env3)

# run with train and test data
m2 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt','svm'),replicatin='sub',
          test.percent=30,n=20)
m2

m3 <- sdm(n~.,data=d,methods=c('fda','glm','rf','mars','brt'),replicatin='sub',
          test.percent=30,n=20)
m3

m4 <- sdm(n~.,data=d,methods=c('fda','glm','rf','brt'),replicatin='sub',
          test.percent=30,n=20)
m4

#predict
e2 <- ensemble(m4,newdata=env3,filename='species_data/hsm/ensemble_Bandedcivet',setting=list(method='weighted',stat='AUC'))
plot(e2)

e3 <- ensemble(m4,newdata=env3,filename='species_data/hsm/ensemble_Orangutan_2',setting=list(method='weighted',stat='AUC'))
plot(e3)

e3 <- predict(m5,newdata=env3,mean=T)
plot(e3)

#get variale importance
vi_BC1 <- getVarImp(m4)
vi_bc <- plot(vi_BC1,'cor')


#plot Variable Importance output
plot1 <- ggarrange(vi_cl,vi_sb,vi_ym, vi_bp, vi_ou, vi_bc,
                   ncol = 2, nrow = 3, labels = c("A","B","C","D","E","F"))
#save output
ggsave(filename = plot1, plot = plot1, device = "svg")

