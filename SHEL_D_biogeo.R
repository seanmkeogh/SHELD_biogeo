
################################################################
# The following code was run by Sean Keogh - keogh026@umn.edu
# HUC10 shapefiles downloaded from Dan MacGuigan Gdrive
# https://drive.google.com/drive/folders/146eSx7yVRu55uqsbu7uBb2VunNqqZX6q
############################################################################################################

#load libraries
library(sf); library(tidyverse); library(scales); library(readxl) # load necessary libraries.
library(rgbif)
library(cowplot) 
library(maps);library(rgdal);library(gridExtra);library(maptools);library(raster);
library(rgeos);library(grid);library(mapplots);library(mapproj);library(ggspatial);
library(ggrepel);library(rnaturalearth);library(CoordinateCleaner);library(leaflet) 

#set your working directory
wd <- "~/Desktop/IZ_NMNH/MusselMuseum/"
# read in both occurrences
occs<-read_xlsx(paste0(wd,'initial_submission/APPENDIX_2.xlsx'),
                sheet='all_records',na=c('NA','','No Data','Unknown'))


############################################################################################################
## CREATE HAAG BIOGEOGRAPHIC ZONES SHAPEFILES
# All shapefiles will be made available. Contact keogh026@umn.edu with issues.
############################################################################################################

# Read in all Watershed Boundary Datasets - here it is all HUC10's
watersheds <- readOGR(paste(wd, "ranges_SK/shapefiles/watersheds/WBD_HU10_watersheds.shp", sep=""))
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

########## DRAW St. Lawrence-Great Lakes
Great_lakes<- c("0401","0402","0403","0404","0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415")
HUCs<- (Great_lakes)
river_name <- "StLaw-GreatLakes"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot StLaw-GreatLakes
StLaw-GreatLakes <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/StLaw-GreatLakes.shp", sep=""))
StLaw-GreatLakes <- spTransform(StLaw-GreatLakes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet(StLaw-GreatLakes) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Mississippi Embayment
Missembay<- c("08020201","08020203","08020204","08020205","080203","080204","080201",
              "0801","0803","0805","0806",
              "080402","080403",
              "080801", "08080202","08080201",
              "080701","080703","08070201",
              "080901","080903","08090203",
              "111403","111402","11140106","11140101","11140102","11140103","11140104",
              "11110101","11110102","11110104","111102")
HUCs<- (Missembay)
river_name <- "Missembay"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
miss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Missembay.shp", sep=""))
miss <- spTransform(miss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet(miss) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Upper Miss
UpperMiss<- c("1029","07")
HUCs<- (UpperMiss)
river_name <- "UpperMiss"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/UpperMiss.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=miss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black") %>% 
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="red")
plot3 %>% addTiles()

########## DRAW Ohioan
Ohioan<- c("05130205",
           "0501","0502","0503","0504","0505","0506","0507","0508","0509","0510","0511","0512","0514",
           "06040006","06040005","06040001")
HUCs<- (Ohioan)
river_name <- "Ohioan"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Ohioan.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet(umiss) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Tenn-Cumb
Tenn_Cumb<- c("05130201","05130202","05130203","05130204","05130206",
           "051301",
           "06040002","06040003","06040004",
           "0603","0602","0601")
HUCs<- (Tenn_Cumb)
river_name <- "Tenn_Cumb"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Tenn_Cumb.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black") %>% 
plot3 %>% addTiles()

########## DRAW Int_highlands
Int_highlands<- c("08020202","080401",
              "11140105","11140107","11140108","11140109",
              "1101","1107","11110105","11110103")
HUCs<- (Int_highlands)
river_name <- "Int_highlands"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Int_highlands.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Great_plains
Great_plains<- c("1102","1103","1104","1105","1106","1108","1109","1110","1112","1113",
                 "1001","1003","1004","1005","1006","1007","1008","1009","1010","1011","1012","1013",
                 "1014","1015","1016","1017","1018","1019","1020","1021","1022","1023","1024","1025","1026",
                 "1027","1028","1030",
                 "09")
HUCs<- (Great_plains)
river_name <- "Great_plains"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Great_plains.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Western_gulf
Western_gulf<- c("1205","1206","1207","1208","1209","1210","1211",
                 "13")
HUCs<- (Western_gulf)
river_name <- "Western_gulf"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Western_gulf.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Sabine_Trin
Sabine_Trin<- c("08080203","08080204","08080205","08080206",
                "1204","1203","1202","1201")
HUCs<- (Sabine_Trin)
river_name <- "Sabine_Trin"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Sabine_Trin.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Pont_Pearl_Pasc
Pont_Pearl_Pasc<- c("08070202","08070203","08070204","08070205",
                    "08090201","08090202",
                    "0318","0317")
HUCs<- (Pont_Pearl_Pasc)
river_name <- "Pont_Pearl_Pasc"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Pont_Pearl_Pasc.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Mobile
Mobile<- c("0316","0315")
HUCs<- (Mobile)
river_name <- "Mobile"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Mobile.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Escambia_Choc
Escambia_Choc<- ("0314")
HUCs<- (Escambia_Choc)
river_name <- "Escambia_Choc"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Escambia_Choc.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Apalachicolan
Apalachicolan<- c("03110103","03110102","031102",
                 "0312","0313")
HUCs<- (Apalachicolan)
river_name <- "Apalachicolan"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Apalachicolan.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Penn_Florida
Penn_Florida<- c("03110101",
                  "0310","0309","0308",
                  "03070204","03070205")
HUCs<- (Penn_Florida)
river_name <- "Penn_Florida"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Penn_Florida.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Southern_atl
Southern_atl<- c("03070201","03070202","03070203","030701",
                 "0306","0305","0304","0303","0302","0301",
                 "020802")
HUCs<- (Southern_atl)
river_name <- "Southern_atl"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Southern_atl.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Northern_atl
Northern_atl<- c("020801",
                 "0207","0206","0205","0204","0203","0202","0201","01")
HUCs<- (Northern_atl)
river_name <- "Northern_atl"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Northern_atl.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

########## DRAW Pacific
Pacific<- c("1002","14","15","16","17","18","19")
HUCs<- (Pacific)
river_name <- "Pacific"
huc10s <- as.character(watersheds$huc10)
target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
huc10s <- as.character(target_watershed$huc10)
target_range <- aggregate(target_watershed, dissolve=T)
target_range <- as(target_range, "SpatialPolygonsDataFrame")
#write shapefile
setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones')
writeOGR(target_range, dsn = '.', layer = paste(river_name), driver = "ESRI Shapefile", overwrite_layer=TRUE)
#Plot
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Pacific.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot3 <- leaflet() %>%
  addPolygons(data=umiss,stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black")
plot3 %>% addTiles()

#Plot all zones
umiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/UpperMiss.shp", sep=""))
umiss <- spTransform(umiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lmiss <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Missembay.shp", sep=""))
lmiss <- spTransform(lmiss, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plains <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Great_plains.shp", sep=""))
plains <- spTransform(plains, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Int_highlands <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Int_highlands.shp", sep=""))
Int_highlands <- spTransform(Int_highlands, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Ohioan <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Ohioan.shp", sep=""))
Ohioan <- spTransform(Ohioan, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Tenn_Cumb <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Tenn_Cumb.shp", sep=""))
Tenn_Cumb <- spTransform(Tenn_Cumb, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
StLaw_GreatLakes<- readOGR(paste(wd, "SHEL-D/Biogeo_zones/StLaw-GreatLakes.shp", sep=""))
StLaw_GreatLakes <- spTransform(StLaw_GreatLakes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Western_gulf <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Western_gulf.shp", sep=""))
Western_gulf <- spTransform(Western_gulf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sabine_Trin <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Sabine_Trin.shp", sep=""))
Sabine_Trin <- spTransform(Sabine_Trin, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Pont_Pearl_Pasc <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Pont_Pearl_Pasc.shp", sep=""))
Pont_Pearl_Pasc <- spTransform(Pont_Pearl_Pasc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Mobile <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Mobile.shp", sep=""))
Mobile <- spTransform(Mobile, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Escambia_Choc <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Escambia_Choc.shp", sep=""))
Escambia_Choc <- spTransform(Escambia_Choc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Penn_Florida <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Penn_Florida.shp", sep=""))
Penn_Florida <- spTransform(Penn_Florida, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Apalachicolan <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Apalachicolan.shp", sep=""))
Apalachicolan <- spTransform(Apalachicolan, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Southern_atl <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Southern_atl.shp", sep=""))
Southern_atl <- spTransform(Southern_atl, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Northern_atl <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Northern_atl.shp", sep=""))
Northern_atl <- spTransform(Northern_atl, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Pacific <- readOGR(paste(wd, "SHEL-D/Biogeo_zones/Pacific.shp", sep=""))
Pacific <- spTransform(Pacific, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

leaflet() %>%
  addPolygons(data=umiss,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black") %>% 
  addPolygons(data=lmiss,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="red")%>% 
  addPolygons(data=plains,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="green")%>% 
  addPolygons(data=Int_highlands,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="yellow")%>% 
  addPolygons(data=Ohioan,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="blue")%>% 
  addPolygons(data=Tenn_Cumb,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="orange")%>% 
  addPolygons(data=StLaw_GreatLakes,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="brown")%>% 
  addPolygons(data=Western_gulf,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="teal")%>% 
  addPolygons(data=Sabine_Trin,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="darkorchid")%>% 
  addPolygons(data=Pont_Pearl_Pasc,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="skyblue")%>% 
  addPolygons(data=Mobile,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="purple")%>% 
  addPolygons(data=Escambia_Choc,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="crimson")%>% 
  addPolygons(data=Apalachicolan,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="royalblue")%>% 
  addPolygons(data=Penn_Florida,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="olivedrab")%>% 
  addPolygons(data=Southern_atl,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="seagreen")%>% 
  addPolygons(data=Northern_atl,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="goldenrod")%>% 
  addPolygons(data=Pacific,stroke = FALSE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="maroon")%>% addTiles()

##########################################################################################
### Use coordinatecleaner to test if 
### species occurrences occur in Haag's faunal zones
################################################################################

###### Read in FMCS names
mussel.names <- read_csv(paste0(wd,"Updated_data/6a_FMCS_NAMES_LIST.csv"), trim_ws = TRUE, col_types = cols(.default = "c"))

# Remove Disconaias fimbriata as it as no occurrences in our dataset and therefore breaks the for loop
mussel.names<-mussel.names %>% 
  subset(FMCS_NAME !='Disconaias fimbriata')
#species list
mussel_sp<-mussel.names$FMCS_NAME

#clean occurrence data
occs<-occs %>% 
  filter(InRange=='TRUE') %>% 
  filter(state_mismatch=='FALSE') %>% 
  filter(outside_NHD=='FALSE') %>% 
  filter(dupe_flag=='FALSE')
occs<-occs[,-52] # delete existing 'species' column, I need it below

# run for loop for all species aside from Disconaias to flag records
species1='Actinonaias ligamentina'
for(species1 in mussel_sp){
  setwd("~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/Biogeo_zones/")
  clean<-occs %>%
    filter(species_update==species1)
  clean$species<-clean$species_update # for some reason cc_iucn needs column names to be 'species'
  ## make all lat-long records numeric and drop NA's
  clean$standardized_longitude<-as.numeric(clean$standardized_longitude)
  clean$standardized_latitude<-as.numeric(clean$standardized_latitude)
  clean<- clean %>%
    drop_na(standardized_longitude) %>%
    drop_na(standardized_latitude)
  
  # read in Miss Embayment
  range_shp = sf::st_read(dsn = "Missembay.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Missembay region
  clean$Missembay<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")

  # read in UpperMiss
  range_shp = sf::st_read(dsn = "UpperMiss.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in UpperMiss region
  clean$UpperMiss<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Ohioan
  range_shp = sf::st_read(dsn = "Ohioan.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Ohioan region
  clean$Ohioan<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Tenn_Cumb
  range_shp = sf::st_read(dsn = "Tenn_Cumb.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Tenn_Cumb region
  clean$Tenn_Cumb<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Int_highlands
  range_shp = sf::st_read(dsn = "Int_highlands.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Int_highlands region
  clean$Int_highlands<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Great_plains
  range_shp = sf::st_read(dsn = "Great_plains.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Great_plains region
  clean$Great_plains<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in StLaw_GreatLakes
  range_shp = sf::st_read(dsn = "StLaw-GreatLakes.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in StLaw_GreatLakes region
  clean$StLaw_GreatLakes<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Western_gulf
  range_shp = sf::st_read(dsn = "Western_gulf.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Western_gulf region
  clean$Western_gulf<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Sabine_Trin
  range_shp = sf::st_read(dsn = "Sabine_Trin.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Sabine_Trin region
  clean$Sabine_Trin<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Pont_Pearl_Pasc
  range_shp = sf::st_read(dsn = "Pont_Pearl_Pasc.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Pont_Pearl_Pasc region
  clean$Pont_Pearl_Pasc<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Mobile
  range_shp = sf::st_read(dsn = "Mobile.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Mobile region
  clean$Mobile<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Escambia_Choc
  range_shp = sf::st_read(dsn = "Escambia_Choc.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Escambia_Choc region
  clean$Escambia_Choc<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Apalachicolan
  range_shp = sf::st_read(dsn = "Apalachicolan.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Apalachicolan region
  clean$Apalachicolan<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Penn_Florida
  range_shp = sf::st_read(dsn = "Penn_Florida.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Penn_Florida region
  clean$Penn_Florida<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Southern_atl
  range_shp = sf::st_read(dsn = "Southern_atl.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Southern_atl region
  clean$Southern_atl<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Northern_atl
  range_shp = sf::st_read(dsn = "Northern_atl.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Northern_atl region
  clean$Northern_atl<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  # read in Pacific
  range_shp = sf::st_read(dsn = "Pacific.shp")
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  
  #now check if sp. records that occur in Pacific region
  clean$Pacific<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  
  Mississippi_Embayment<-sum(clean$Missembay == "TRUE")
  Upper_Mississippi<-sum(clean$UpperMiss == "TRUE")
  Ohioan<-sum(clean$Ohioan == "TRUE")
  Tennessee_Cumberland<-sum(clean$Tenn_Cumb == "TRUE")
  Interior_Highlands<-sum(clean$Int_highlands == "TRUE")
  Great_Plains<-sum(clean$Great_plains == "TRUE")
  StLawrence_GreatLakes<-sum(clean$StLaw_GreatLakes == "TRUE")
  Western_Gulf<-sum(clean$Western_gulf == "TRUE")
  Sabine_Trinity<-sum(clean$Sabine_Trin == "TRUE")
  Pontchartrain_Pearl_Pascagoula<-sum(clean$Pont_Pearl_Pasc == "TRUE")
  Mobile_Basin<-sum(clean$Mobile == "TRUE")
  Escambia_Choctawhatchee<-sum(clean$Escambia_Choc == "TRUE")
  Apalachicolan<-sum(clean$Apalachicolan == "TRUE")
  Peninsular_Florida<-sum(clean$Penn_Florida == "TRUE")
  Southern_Atlantic<-sum(clean$Southern_atl == "TRUE")
  Northern_Atlantic<-sum(clean$Northern_atl == "TRUE")
  Pacific<-sum(clean$Pacific == "TRUE")
  data<-cbind.data.frame(Mississippi_Embayment,Upper_Mississippi,Ohioan,Tennessee_Cumberland,
                         Interior_Highlands,Great_Plains,StLawrence_GreatLakes,Western_Gulf,
                         Sabine_Trinity,Pontchartrain_Pearl_Pascagoula,Mobile_Basin,Escambia_Choctawhatchee,
                         Apalachicolan,Peninsular_Florida,Southern_Atlantic,Northern_Atlantic,
                         Pacific)
  data
  data[data>2]<-'PRESENT'
  data[data==0]<-'ABSENT'
  data[data>0 & data<3]<-'QUESTIONABLE'
  data<-cbind.data.frame(data,species1)
  
  setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/species_files/')
  write_csv(data,file = paste0(species1,".csv"))
}

## combine all records to master spreadsheet

setwd('~/Desktop/IZ_NMNH/MusselMuseum/SHEL-D/species_files/')
temp <- list.files(pattern="*.csv")
df <- data.frame(Mississippi_Embayment=numeric(0),Upper_Mississippi=numeric(0),Ohioan=numeric(0),Tennessee_Cumberland=numeric(0),
                 Interior_Highlands=numeric(0),Great_Plains=numeric(0),StLawrence_GreatLakes=numeric(0),Western_Gulf=numeric(0),
                 Sabine_Trinity=numeric(0),Pontchartrain_Pearl_Pascagoula=numeric(0),Mobile_Basin=numeric(0),Escambia_Choctawhatchee=numeric(0),
                 Apalachicolan=numeric(0),Peninsular_Florida=numeric(0),Southern_Atlantic=numeric(0),Northern_Atlantic=numeric(0),
                 Pacific=numeric(0),species1=numeric(0))
for (i in 1:length(temp)) {
  tmp <- read.csv(temp[i])
  df <- rbind(df, tmp[, c("Mississippi_Embayment","Upper_Mississippi","Ohioan","Tennessee_Cumberland",
                          "Interior_Highlands","Great_Plains","StLawrence_GreatLakes","Western_Gulf",
                          "Sabine_Trinity","Pontchartrain_Pearl_Pascagoula","Mobile_Basin","Escambia_Choctawhatchee",
                          "Apalachicolan","Peninsular_Florida","Southern_Atlantic","Northern_Atlantic",
                          "Pacific","species1")])
}

df$species<-df$species1
df<-df[,-18]
data<-df[,c(ncol(df),1:(ncol(df)-1))]

write_csv(data,paste0(wd,'SHEL_D_HaagFaunalZones.csv'))