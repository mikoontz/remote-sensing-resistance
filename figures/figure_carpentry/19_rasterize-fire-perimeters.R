library(sf)
library(fasterize)
library(raster)
library(viridis)

# Read in fire perimeters layers
#st_layers(dsn = "features/FRAP-fire-perimeters/fire16_1.gdb") # what layers are aviailable in the geodatabase?
fires <-
  st_read("data/data_output/fire_perim/fire18_1_sn/fire18_1_sn.shp", stringsAsFactors = FALSE) %>% 
  st_transform(4326) %>% 
  dplyr::select(year_, fire_name, alarm_date) %>% 
  mutate(year = as.numeric(year_)) %>% 
  filter(year >= 1984) %>% 
  mutate(id = 1:nrow(.))

sn <- st_read("data/data_output/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp")

r_template <- raster("data/data_output/landcover_PFR/ypmc-mask_10-fold-res.tif")

# Rasterize the fire polygons, taking the most recent year
fires_raster <- fasterize(sf = fires, field = "id", raster = r_template, fun = "count")

plot(fires_raster, col = viridis(10), axes = FALSE, box = FALSE)
max(fires_raster[], na.rm = TRUE)

# Export a GeoTiff for use with R
writeRaster(fires_raster, filename = "figures/frap-fire-extent-image.tif", overwrite = TRUE)
