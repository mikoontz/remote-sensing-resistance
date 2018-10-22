# This script is used to count the total number of fires burning in yellow
# pine/mixed conifer forest in the Sierra Nevada since 1984 using 2 different
# datasets:
# 1) The USFS Region 5 GIS database, the current leading source of severity
# data for this region in this time period
# 2) The FRAP + Earth Engine method presented in this paper

library(sf)
library(raster)
library(here)

mixed_con <- raster::raster(here::here("data/data_output/landcover_PFR/mixed_conifer.tif"))
sn <- sf::st_read(here::here("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp"))

r5 <- sf::st_read("data/features/veg_severity_perimeters18_1.gdb/") %>% st_transform(4326)

r5_sn <-
  r5 %>% 
  st_intersection(sn) %>% 
  filter(st_is_valid(.))

# use the extract function from the raster packag to pull out the values
# from the mixed conifer raster layer (which has values of 1 for mixed
# conifer forest and values of 0 otherwise). For each of the Region 5
# USFS Geospatial fires in the Sierra Nevada between 1984 and 2017, 
# count the number of pixels that are 1 (using length(which(x == 1)))
# If a fire burned with at least 1 pixel in mixed conifer/yellow pine
# forest, the resulting value will be greater than 0.
mixed_con_fires <- raster::extract(x = mixed_con, y = r5_sn, fun = function(x, ...) length(which(x == 1)))
mixed_con_fires

# 430 fires in the Region 5 Geospatial database that burned between 1984 and 2017 at least 
# partially in yellow pine/mixed conifer forest as defined by FRID

# how many of the fires have more than 0 pixels in yellow pine/mixed conifer forest?
length(which(as.vector(mixed_con_fires) != 0))



