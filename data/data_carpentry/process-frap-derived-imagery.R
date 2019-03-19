# Script to rename bands of the Python raster exports
library(tidyverse)
library(raster)
library(rasterVis)

band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw')

fire <- raster::stack("data/ee_fire-images/19880704_00017_0000395195b1272a72a0_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870829_00059_0000de101e16afb630c0_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870830_00045_0000bafc4812e81fdc1f_epsg3310.tif") # Hamm Fire
fire <- raster::stack("data/ee_fire-images/19870830_00177_0000f8d62e28cf5fa43c_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870830_00421_00003a70ebf6fedca59e_epsg3310.tif")

names(fire) <- band_names

# Overall RBR values from the model
best_model <- 
  read_csv(here::here("data/data_output/cbi_calibration_model_comparison.csv")) %>% 
  dplyr::arrange(desc(r2_kfold)) %>% 
  dplyr::slice(1)

low_sev_lower_rbr_threshold <- best_model$low_sev
# low_sev_lower_rbr_threshold <- 0.04509658
mod_sev_lower_rbr_threshold <- best_model$mod_sev
# mod_sev_lower_rbr_threshold <- 0.1125589
hi_sev_lower_rbr_threshold <- best_model$hi_sev
# hi_sev_lower_rbr_threshold <- 0.2823349

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

breaks <- c(cellStats(fire[["rbr"]], min), seq(burned_unburned_rbr_threshold, high_sev_low_sev_rbr_threshold, length.out = 100), cellStats(fire[["rbr"]], max))
# cols <- colorRampPalette(c('#008000', RBR_viz, '#ff0000'))
cols <- colorRampPalette(RBR_viz)
levelplot(fire[["rbr"]], at = breaks, col.regions = cols)

breaks <- c(cellStats(fire[["rbr"]], min), seq(burned_unburned_rbr_threshold, cellStats(fire[["rbr"]], max), length.out = 100))
cols <- colorRampPalette(RBR_viz)

levelplot(fire[["rbr"]], at = breaks, col.regions = cols)
