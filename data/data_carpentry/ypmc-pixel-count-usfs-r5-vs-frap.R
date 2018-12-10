# This script is used to count the total number of pixels burning in yellow
# pine/mixed conifer forest in each fire within the Sierra Nevada since 1984 
# using 2 different datasets:
# 1) The USFS Region 5 GIS database, the current leading source of severity
# data for this region in this time period
# 2) The FRAP + Earth Engine method presented in this paper

library(sf)
library(tidyverse)
library(raster)
library(here)

mixed_con <- raster::raster(here::here("data/data_output/landcover_PFR/mixed_conifer.tif"))
sn <- sf::st_read(here::here("data/data_output/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp"))
r5 <- sf::st_read("data/features/fire_perim/veg_severity_perimeters18_1.gdb/") %>% st_transform(4326)

if (!file.exists(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp"))) {
  # use the extract function from the raster package to pull out the values
  # from the mixed conifer raster layer (which has values of 1 for mixed
  # conifer forest and values of 0 otherwise). For each of the Region 5
  # USFS Geospatial fires in the Sierra Nevada between 1984 and 2017, 
  # count the number of pixels that are 1 (using length(which(x == 1)))
  # If a fire burned with at least 1 pixel in mixed conifer/yellow pine
  # forest, the resulting value will be greater than 0.
  r5_sn <-
    r5 %>% 
    st_intersection(sn) %>% 
    filter(st_is_valid(.)) %>% 
    mutate(mixed_con_pixels = as.vector(raster::extract(x = mixed_con, y = ., fun = function(x, ...) length(which(x == 1)))))
  
  # how many of the fires have more than 0 pixels in yellow pine/mixed conifer forest?
  # 430 fires in the Region 5 Geospatial database that burned between 1984 and 2017 at least 
  # partially in yellow pine/mixed conifer forest as defined by FRID
  
  st_write(r5_sn, "data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp")
  
} else {
  r5_sn <- st_read(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp"))}

# The FRAP fire perimeters in mixed-con -----------------------------------

# Should already be intersected with the Sierra Nevada outline

if (!file.exists("data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp")) {
  frap_sn <- sf::st_read("data/data_output/fire_perim/fire17_1_sn")
  frap_sn_ypmc <- 
    frap_sn %>% 
    mutate(mixed_con_pixels = as.vector(raster::extract(x = mixed_con, y = ., fun = function(x, ...) length(which(x == 1))))) %>% 
    filter(mixed_con_pixels > 0)

  st_write(frap_sn_ypmc, "data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp")  
}
