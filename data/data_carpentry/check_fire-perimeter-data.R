# The original data source: 
# California Department of Forestry and Fire Protection
# http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download
# Accessed 2017-06-13

# The new Table feature of Google Earth Engine means that there is no need to 
# clean these data in order to upload them as a Fusion Table (.kml format)
# Instead, this script will act as a check on the uploaded shapefile of fire
# perimeters originally downloaded from FRAP

# The fire perimeter dataset from the above website was opened in QGIS, 
# saved as a shapefile, then uploaded as an "Asset" to Google Earth Engine.
# Google Earth Engine asset is viewable if you have Earth Engine access: 
# https://code.earthengine.google.com/?asset=users/mkoontz/fire_perim_16_1

### Pre-R steps for this script:
# 1) Read fire_perim_16_1.gdb file into QGIS
#     represents most comprehensive mapped database of fire perimeters in California
# 2) Read SierraEcoregion_Jepson shapefile into QGIS (derived from Jepson Herbarium Geographic Subdivisions of California)
#     represents delineation of the Sierra Nevada region
# 3) Vector -> Geoprocessing Tools -> Intersect
#     Input Vector Layer = fire perimeter layer
#     Intersect Layer = Sierra Nevada delineation layer
# Output shapefile represents the fire perimeters within the Sierra Nevada

library(dplyr)
library(lubridate)
library(sf)

fires <- st_read("data/features/fire_perim/fire_perim_sn_16_1_shp/", stringsAsFactors = FALSE)
sn <- st_read("data/features/SierraEcoregion_Jepson/")

l5_fires <- 
  fires %>%
  filter(ALARM_DATE > ymd("1984-04-01")) %>%
  filter(ALARM_DATE < ymd("2011-05-04")) %>%
  rename(fire_date = ALARM_DATE, fire_name = FIRE_NAME) %>%
  dplyr::select(fire_name, fire_date, Shape_Area, GIS_ACRES, geometry) %>%
  mutate(area = st_area(.))

# Confirmed: 1652 fires is exactly how many Google Earth Engine shows using the
# same filters
dim(l5_fires)

# Note that the intersection operation in QGIS clipped fires that spanned the
# border of the Sierra Nevada region such that only the parts of the fire
# perimeter polygons that fell within the Sierra Nevada region remained
# See, for instance, the GORMON fire that burned at the very southern tip of
# our Sierra Nevada demarcation. Only the sliver of the fire that was within
# the Sierra Nevada was kept. This is NOT what happens with the filterBounds()
# operation in Google Earth Engine, so additional clipping will be necessary
# there.

# Find the Storrie Fire

storrie <-
  fires %>% 
  filter(ALARM_DATE > ymd("2000-01-01")) %>% 
  filter(ALARM_DATE < ymd("2000-12-30")) %>% 
  filter(FIRE_NAME == "STORRIE")

storrie
plot(storrie$geometry)

# Find the McNally Fire

mcnally <-
  fires %>% 
  filter(ALARM_DATE > ymd("2002-01-01")) %>% 
  filter(ALARM_DATE < ymd("2002-12-30")) %>% 
  filter(FIRE_NAME == "MCNALLY")

mcnally
plot(mcnally$geometry)
