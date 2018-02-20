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

# Directly combine the vector shapes into a single, large vector shape
sn_v <- rbind(nsn, ssn)
st_write(obj = sn_v, dsn = "data/data_output/landcover_PFR/mixed_conifer/mixed_conifer.shp", driver = "ESRI Shapefile")
# Successful write; some warnings about truncated column names and truncated
# values in the shape_area column (we can always recalculate these)
# This shapefile was uploaded to Google Earth Engine and is available here:
#


# Use fasterize to turn the vector shapes into rasters
nsn_r <- fasterize(sf = nsn, raster = r)
ssn_r <- fasterize(sf = ssn, raster = r)

sn_r <- raster::merge(nsn_r, ssn_r)

plot(sn_r)
plot(sn$geometry, add = TRUE)

writeRaster(x = sn_r, filename = "data/data_output/landcover_PFR/mixed_conifer.tif")
