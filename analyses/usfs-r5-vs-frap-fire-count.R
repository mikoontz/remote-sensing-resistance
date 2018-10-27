# This script is used to count the total number of fires burning in yellow
# pine/mixed conifer forest in the Sierra Nevada since 1984 using 2 different
# datasets:
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

r5_sn <-
  r5_sn %>% 
  mutate(mixed_con_pixels = mixed_con_fires)

r5_sn

st_write(r5_sn, "data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp")

# r5_sn <- st_read("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp")

r5_sn

# 744243.9 hectares of burned area in region 5 dataset
sum(r5_sn$mxd_cn_ * 30 * 30) / 10000

# The FRAP fire perimeters in mixed-con -----------------------------------

# Should already be intersected with the Sierra Nevada outline

if(!file.exists("data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp")) {
  frap_sn <- sf::st_read("data/data_output/fire_perim/fire17_1_sn")
  frap_sn_ypmc <- 
    frap_sn %>% 
    mutate(mixed_con_pixels = as.vector(raster::extract(x = mixed_con, y = ., fun = function(x, ...) length(which(x == 1))))) %>% 
    filter(mixed_con_pixels > 0)

  st_write(frap_sn_ypmc, "data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp")  
}

frap_sn_ypmc <- 
  sf::st_read("data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp") %>% 
  rename(mixed_con_pixels = mxd_cn_,
         alarm_date = alrm_dt,
         gis_acres = gis_crs)

frap_sn_ypmc <-
  frap %>% 
  filter(year_ %in% 1984:2017) %>% 
  filter(mixed_con_pixels > 0) %>% 
  filter(!is.na(alarm_date))

# About 789 fires with > 50% in YPMC
mixed_con_frap %>% 
  filter((mixed_con_pixels * 30 * 30) > (0.5 * area_ha * 10000))

# Which fires were left out of the analysis, even though they burned partially
# in YPMC forest and within the Sierra Nevada between 1984 and 2017?
missing <-
  mixed_con_frap %>% 
  left_join(s, by = "gis_acres") %>% 
  filter(is.na(alarm_date.y))

plot(missing$geometry)

missing %>% 
  # filter(as.numeric(as.character(year_)) == 2017) %>% 
  dplyr::select(year_, alarm_date.x, mixed_con_pixels) %>% 
  arrange(alarm_date.x) %>% 
  as.data.frame()

# approximate hectarage of total burned area in yellow pine/mixed-conifer forest
# in the Sierra Nevada between 1984 and 2017: 771342.8
# Out of ~ 2.25e6 hectares ~34%, but lots of overlapping burned area.
sum(mixed_con_frap$mixed_con_pixels * 30 * 30) / 10000
