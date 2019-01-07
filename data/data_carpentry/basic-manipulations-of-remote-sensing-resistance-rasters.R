# Purpose: an example script for working with the wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017 dataset

# Load the necessary packages
library(raster)
library(rasterVis)
library(tidyverse)
library(sf)
library(here)

# Read the raster band metadata file
band_metadata <- read_csv(here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_raster-metadata.csv"))

# Alternatively, use this vector of band names directly
band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw')

# Read one of the .geoTIFF images directly  -------------------------------
hamm <- raster::brick(here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/19870830_00045_0000bafc4812e81fdc1f_epsg3310.tif"))

# Name the raster bands
names(hamm) <- band_names

# Plot the raster

# Thresholds for burned/unburned and high severity/not high severity (calibration shown in Koontz et al. (2019))
burned_unburned_rbr_threshold <- 0.041191844
high_sev_low_sev_rbr_threshold <- 0.2836425

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

breaks <- c(cellStats(hamm[["rbr"]], min), seq(burned_unburned_rbr_threshold, high_sev_low_sev_rbr_threshold, length.out = 100), cellStats(hamm[["rbr"]], max))
cols <- colorRampPalette(RBR_viz)

# Use levelplot() from the rasterVis package to appropriately designate the color ramp
# across the calibrated severity thresholds
levelplot(hamm[["rbr"]], at = breaks, col.regions = cols)


# Alternatively, use the metadata to find a fire to plot ------------------

# First, read the metadata
img_col_metadata <- st_read(here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON"), stringsAsFactors = FALSE)

# Specify some of the known metadata about the fire
target_img_name <- "HAMM"
target_img_year <- 1987

# Subset the metadata to include just the fires that fit the above criteria
target_img_ftr <- 
  img_col_metadata %>% 
  dplyr::filter(fire_name == target_img_name) %>% 
  dplyr::filter(alarm_year == target_img_year) %>% 
  slice(1) # I'm using slice() to get just the first row of this subsetted metadata

# Get the fire_id from the metadata dataframe
target_img_id <-
  target_img_ftr %>% 
  dplyr::pull(fire_id)

# Use the fire_id to search for the raster of interest from the whole folder of rasters and read it
# into R using the brick() function from the raster package
target_img <- raster::brick(list.files(here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters"), pattern = target_img_id, full.names = TRUE))

# Get the filename so that 
target_img_filename <- list.files(here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters"), pattern = target_img_id)

names(target_img) <- band_names

# Alternatively, use the band metadata to assign band names
names(target_img) <- band_metadata$band_name

burned_unburned_rbr_threshold <- 0.041191844
high_sev_low_sev_rbr_threshold <- 0.2836425

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

breaks <- c(cellStats(target_img[["rbr"]], min), seq(burned_unburned_rbr_threshold, high_sev_low_sev_rbr_threshold, length.out = 100), cellStats(target_img[["rbr"]], max))
# cols <- colorRampPalette(c('#008000', RBR_viz, '#ff0000'))
cols <- colorRampPalette(RBR_viz)

levelplot(target_img[["rbr"]], at = breaks, col.regions = cols)

# We can add the perimeter data from the fire metadata if we'd like, but we
# have to first transform it from EPSG:4326 to EPSG:3310 to match the
# coordinate reference system of the raster imagery
plot(target_img[["rbr"]])
plot(st_transform(st_geometry(target_img_ftr), 3310), add = TRUE)

