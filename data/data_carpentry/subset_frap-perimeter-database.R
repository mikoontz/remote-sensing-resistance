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


# Get all the layers of the geodatabase file
(frap_layers <- st_layers("data/features/fire_perim/fire17_1.gdb/"))

# We want just the non-prescription fires
fires <- 
  st_read("data/features/fire_perim/fire17_1.gdb/", layer = "firep17_1") %>% 
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

# 2345 fires in Sierra Nevada during Landsat 4-8
glimpse(fires_sn)

# 2295 fires after removing non-polygons (geometries like points, where a point was taken and
# a radius of the fire estimated). Non-polygon fires often represent very small fires (< 1 acre)
fires_sn <-
  fires_sn %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# 50 fires from the original FRAP database will be removed from our analysis  
2345 - 2295

# Clip the fire geometries so they don't extend beyond the Sierra Nevada
fires_sn <-
  fires_sn %>% 
  st_intersection(sn)

fires <-
  fires %>% 
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# All fires are now valid geometries
all(st_is_valid(fires))
# All fires are valid polygons
unique(st_geometry_type(fires))
  
# Transform fires to epsg4326 for upload to Google Earth Engine
fires_4326 <-
  fires %>% 
  st_transform(4326)

# Write the 2017 perimeters in epsg4326 format to a file
fires_4326 %>% 
  select(-shape_area, -shape_length) %>% 
  st_write(dsn = "data/data_output/fire_perim/fire17_1/fire17_1.shp", 
         driver = "ESRI Shapefile")

# Transform Sierra Nevada fires to epsg4326 for upload to Earth Engine (potentially)
fires_sn_4326 <-
  fires_sn %>%
  st_transform(4326)

fires_sn_4326 %>% 
  dplyr::select(-shape_area, -shape_length) %>% 
  st_write(dsn = "data/data_output/fire_perim/fire17_1_sn",
           driver = "ESRI Shapefile")

fires_by_year <-
  fires_sn %>% 
  group_by(YEAR_) %>% 
  summarize(count = n(), area = sum(area)) %>% 
  as.data.frame()

plot(as.numeric(as.character(fires_by_year$YEAR_)), as.numeric(fires_by_year$area) / 10000, xlim = c(1984, 2017), type = "l")


