library(raster)
library(dplyr)
library(sf)
library(fasterize)

r <- raster("data/features/sierra_nevada_30m_landsat_template.tif")
sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(proj4string(r))

nsn <- st_read("data/features/fire_return_interval_departure/FRID_NorthSierra15_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

ssn <- st_read("data/features/fire_return_interval_departure/FRID_SouthSierra15_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

nsn_r <- fasterize(sf = nsn, raster = r)
ssn_r <- fasterize(sf = ssn, raster = r)

sn_r <- raster::merge(nsn_r, ssn_r)

plot(sn_r)
plot(sn$geometry, add = TRUE)

writeRaster(x = sn_r, filename = "data/features/landcover_PFR/mixed_conifer.tif")
