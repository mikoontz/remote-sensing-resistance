library(raster)
library(dplyr)
library(sf)
library(fasterize)
library(rgdal)

r <- raster("data/data_raw/sierra-nevada-30m-landsat-template.tif")
sn <- st_read("data/data_output/SierraEcoregion_Jepson/") %>%
  st_transform(proj4string(r))

nsn <- st_read("data/data_raw/fire-return-interval-departure/FRID_NorthSierra18_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

ssn <- st_read("data/data_raw/fire-return-interval-departure/FRID_SouthSierra18_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

# Directly combine the vector shapes into a single, large vector shape
sn_v <- rbind(nsn, ssn)

# Use fasterize to turn the vector shapes into rasters
sn_r <- fasterize(sf = sn_v, raster = r)

sn_r <- mask(x = sn_r, mask = sn)

plot(sn_r)
plot(sn$geometry, add = TRUE)

dir.create("data/data_output/landcover_PFR", recursive = TRUE)
writeRaster(x = sn_r, filename = "data/data_output/landcover_PFR/ypmc-mask.tif", overwrite = TRUE)

# Create a low-res version of the ypmc-mask (clipped to the Sierra Nevada geometry)
# Good for visualization only, as it isn't the same resolution of the Landsat data
r_masked <- raster(sn, resolution = res(r) * 10)
sn_r_masked <- fasterize(sf = sn_v, raster = r_masked)
plot(sn_r_masked)

writeRaster(x = sn_r_masked, filename = "data/data_output/landcover_PFR/ypmc-mask_10-fold-res.tif", overwrite = TRUE)
