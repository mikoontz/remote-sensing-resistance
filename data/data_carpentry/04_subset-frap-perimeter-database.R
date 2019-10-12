# A programmatic approach to spatially and temporally subsetting the full 
# FRAP fire perimeter database. Previous approach (as annotated in the 
# "check_fire-perimeter-data.R" script) used QGIS for the spatial subset, but
# we should be able to use the 'sf' package. This data manipulation also
# uses the fire perimeters through 2017 (whereas previous efforts used only 
# perimeters through 2016, because 2017 wasn't over yet)

# Database available here: http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download
# Database accessed 2018-07-10

library(sf)
library(dplyr)
library(lubridate)
library(lwgeom)
library(ggplot2)
library(raster)
library(velox)


# Get all the layers of the geodatabase file
(frap_layers <- st_layers("data/data_raw/fire_perim/fire18_1.gdb/"))

# We want just the non-prescription fires
fires <- 
  st_read("data/data_raw/fire_perim/fire18_1.gdb/", layer = "firep18_1") %>% 
  filter(ALARM_DATE > ymd("1982-08-22")) %>%
  dplyr::rename(geometry = Shape) %>% 
  mutate(area_ha = as.numeric(st_area(.) / 10000)) %>% 
  st_make_valid() %>% 
  rename_all(tolower)

glimpse(fires)

# The Sierra Nevada geometry from the Jepson
sn <- st_read("data/data_output/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% 
  st_transform(st_crs(fires))

# Spatial subset to the Sierra Nevada region
fires_sn <- 
  fires[sn, ]

# 2419 fires in Sierra Nevada during Landsat 4-8
glimpse(fires_sn)

# Clip the fire geometries so they don't extend beyond the Sierra Nevada
fires_sn <-
  fires_sn %>% 
  st_intersection(sn)

# fires_sn <-
#   fires_sn %>% 
#   filter(st_geometry_type(.) %in% c("MULTIPOLYGON", "POLYGON"))

# All fires are now valid geometries
all(st_is_valid(fires_sn))
# All fires are valid polygons
unique(st_geometry_type(fires_sn))
  
# Transform fires to epsg4326 for upload to Google Earth Engine
fires_sn_4326 <-
  fires_sn %>% 
  st_transform(4326)


# one fire has a bad geometry, so we fix it -------------------------------
# Fire has linestrings and polygons, so just subset to the polygons
bad_fire <- 
  fires_sn_4326 %>% 
  dplyr::filter(st_geometry_type(.) == "GEOMETRYCOLLECTION") %>% 
  st_cast() %>% 
  dplyr::filter(st_geometry_type(.) == "POLYGON") %>% 
  st_union()

st_geometry(fires_sn_4326[st_geometry_type(fires_sn_4326) == "GEOMETRYCOLLECTION", ]) <- bad_fire


# write the Sierra Nevada fires to disk -----------------------------------
dir.create("data/data_output/fire_perim/fire18_1_sn", recursive = TRUE)
fires_sn_4326 %>%
  dplyr::select(-shape_area, -shape_length) %>% 
  st_write(dsn = "data/data_output/fire_perim/fire18_1_sn/fire18_1_sn.shp", delete_dsn = TRUE)


# how many pixels per fire were yellow pine/mixed-conifer -----------------
# filter to just fires that had at least 1 pixel that covered FRID-derived
# yellow pine/mixed-conifer

ypmc_mask <- raster::raster("data/data_output/landcover_PFR/ypmc-mask.tif")
vx <- velox::velox(ypmc_mask)

# This is a long-running step
fires_sn_ypmc_4326 <- 
  fires_sn_4326 %>% 
  mutate(ypmc_pxls = as.vector(vx$extract(as(., "Spatial"), fun = function(x, ...) length(which(x == 1))))) %>% 
  filter(ypmc_pxls > 0)

dir.create("data/data_output/fire_perim/fire18_1_sn_ypmc", recursive = TRUE)  
fires_sn_ypmc_4326 %>% 
  dplyr::select(-shape_length, -shape_area) %>% 
  st_write("data/data_output/fire_perim/fire18_1_sn_ypmc/fire18_1_sn_ypmc.shp", delete_dsn = TRUE)

