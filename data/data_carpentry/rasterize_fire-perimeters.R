library(sf)
library(fasterize)
library(raster)
library(viridis)

# Load project boundary and raster template
# raster_template <- raster("features/sierra-nevada-250m-evi-template.tif")

# Read in fire perimeters layers
#st_layers(dsn = "features/FRAP-fire-perimeters/fire16_1.gdb") # what layers are aviailable in the geodatabase?
fires <-
  st_read("data/features/fire_perim/fire_perim_sn_16_1_shp/", stringsAsFactors = FALSE) %>% 
  st_transform(4326) %>% 
  dplyr::select(YEAR_, FIRE_NAME, ALARM_DATE) %>% 
  mutate(year = as.numeric(YEAR_)) %>% 
  filter(year >= 1984) %>% 
  mutate(id = 1:nrow(.))
sn <- st_read("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% st_transform(4326)

r_template <- raster("data/data_output/landcover_PFR/mixed_conifer_sn-mask_10-fold-res.tif")

# Rasterize the fire polygons, taking the most recent year
fires_raster <- fasterize(sf = fires, field = "id", raster = r_template, fun = "count")

plot(fires_raster, col = viridis(10), axes = FALSE, box = FALSE)
max(fires_raster[], na.rm = TRUE)

# Export a GeoTiff for use with R
writeRaster(fires_raster, filename = "data/data_output/frap-fire-extent-image.tif", overwrite = TRUE)
