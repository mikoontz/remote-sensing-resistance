rm(list = ls())

library(sf)
library(raster)
library(dplyr)

bps <- raster("data/features/landcover_LANDFIRE/landfire_biophysical_settings.tif")
sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(proj4string(bps)) %>%
  as("Spatial")

plot(bps)
plot(sn, add = TRUE)

bps_sn <- bps %>%
  crop(sn) %>%
  mask(sn)

writeRaster(bps_sn, filename = "data/features/landcover_LANDFIRE/landfire_biophysical_settings_sn.tif")
