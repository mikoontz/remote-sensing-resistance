# Script to convert .kml version of Jepson ecoregion designations into a
# usable outline of the Sierra Nevada

library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(lwgeom)

jep <- st_read("data/features/jepcodes-v7.kml")
glimpse(jep)

sn_names <- c("nSNF", "cSNF", "sSNF", "nSNH", "cSNH", "sSNH", "Teh")

# Using the "squeeze" approach to get rid of slivers between neighboring polygons
# https://github.com/r-spatial/sf/issues/547

sn <- 
  jep %>% 
  dplyr::filter(Name %in% sn_names) %>% 
  st_transform(3310) %>% 
  st_buffer(0.0001) %>%
  st_difference(.) %>%
  st_union() %>% 
  st_transform(4326)

# Using the st_snap() function as also suggested in the same link above.
# This seems to make the resulting geometry valid, which is handy.
sn <- 
  jep %>% 
  dplyr::filter(Name %in% sn_names) %>% 
  st_transform(3310) %>% 
  st_snap(x = ., y = ., tolerance = 0.0001) %>%
  st_union() %>% 
  st_transform(4326) %>% 
  st_zm()

st_write(sn, dsn = "data/data_output/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp")
