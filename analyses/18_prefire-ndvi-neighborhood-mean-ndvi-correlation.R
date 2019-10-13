library(tidyverse)
library(sf)
library(here)

ss_burned <- sf::st_read(here::here("data/data_output/burned-fire-samples_configured.geoJSON")) %>% st_drop_geometry()

preFire_ndvi.neighborhood_mean_NDVI_correlation <- 
  cor(ss_burned$preFire_ndvi_s, ss_burned$focal_mean_ndvi_1_s, method = "spearman")

write_rds(preFire_ndvi.neighborhood_mean_NDVI_correlation, here::here("analyses/analyses_output/preFire_ndvi.neighborhood_mean_NDVI_correlation.rds"))