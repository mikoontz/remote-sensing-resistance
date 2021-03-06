# Purpose: configure the metadata and the bandnames for the export of all severity measurements derived from the FRAP dataset.

library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(rasterVis)
library(here)

# The original FRAP perimeter representing all fires that burned in yellow pine/mixed-conifer forest in the Sierra Nevada, California

frap_ypmc_perims <- 
  st_read(here::here("data/data_output/fire_perim/fire18_1_sn_ypmc/fire18_1_sn_ypmc.shp"), stringsAsFactors = FALSE) %>% 
  dplyr::mutate(year_ = as.numeric(year_)) %>% 
  dplyr::select(-year_) %>% 
  dplyr::mutate(alarm_year = year(alarm_date),
                alarm_month = month(alarm_date),
                alarm_day = day(alarm_date),
                cont_year = year(cont_date),
                cont_month = month(cont_date),
                cont_day = day(cont_date)) %>% 
  dplyr::select(-c(alarm_date, cont_date))

frap_ypmc_perims

# Earth Engine metadata (which includes the Earth Engine system:index value to link to the raster images)
epoch <- ymd("1970-01-01")

ee_metadata <- read_csv("data/data_output/ee_fire-samples/fires-strat-samples_metadata_2018_48-day-window_L4578_none-interp_all.csv") %>% 
  dplyr::rename(fire_id = `system:index`) %>% 
  dplyr::select(-`.geo`) %>% 
  dplyr::select(-c(alarm_date, cont_date, year_))

ftr_col_metadata <- left_join(frap_ypmc_perims, ee_metadata)

ftr_col_metadata %>% 
  dplyr::filter(!is.na(fire_id)) %>% 
  dplyr::filter(alarm_year >= 1984) %>%
  dim()

severity_imgs_filenames <- list.files("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2018_rasters/")
severity_imgs_fire_ids <- substr(x = severity_imgs_filenames, start = 16, stop = 35)

img_col_metadata <- 
  tibble(fire_id = severity_imgs_fire_ids, sev_img_present = TRUE) %>% 
  full_join(ftr_col_metadata) %>% 
  dplyr::mutate(sev_img_present = ifelse(is.na(sev_img_present), yes = FALSE, no = sev_img_present)) %>% 
  st_as_sf()

# Write the metadata for the fires from the FRAP database that burned at least
# partially in yellow pine/mixed-conifer forest between 1984 and 2018.
# The "sev_img_present" column indicates whether there is a corresponding
# raster image in our database for that FRAP record. TRUE = record present; 
# FALSE = no record present
# There are 1115 raster images in our database and 32 FRAP records without an
# associated raster
# 26 of those FRAP records without a raster image were ingested into Earth 
# Engine, but a raster image wasn't generated (possibly because Landsat imagery
# wasn't available both before and after the fire)
# 1 of those FRAP records wasn't properly ingested into Earth Engine, so it
# doesn't have an associated fire_id (which is generated internally by Earth
# Engine)
# This export includes the geometry of the perimeters
st_write(obj = img_col_metadata, dsn = "data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2018_fire-metadata.geoJSON", delete_dsn = TRUE)

# Write the metadata for these FRAP records without the geometry information
# as a csv file
st_write(obj = img_col_metadata, dsn = "data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2018_fire-metadata.csv", delete_dsn = TRUE)

# This metadata represents the band names and descriptions for each .geoTIFF
# raster image in the database.
band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw', 'earlyfire_vpd')

band_metadata <-
  data_frame(band_number = 1:50,
             band_name = band_names,
             band_description = c("Relative delta normalized burn ratio",
                                  "Prefire normalized burn ratio",
                                  "Postfire normalized burn ratio",
                                  "Relative delta normalized difference vegetation index",
                                  "Relative burn ratio",
                                  "Prefire normalized difference vegetation index",
                                  "Postfire normalized difference vegetation index",
                                  "Neighborhood standard deviation of normalized difference vegetation index in a square with a 1-Landsat-pixel radius (90m x 90m); a measure of forest structural variability",
                                  "Neighborhood mean of normalized difference vegetation index in a square with a 1-Landsat-pixel radius (90m x 90m)",
                                  "Neighborhood standard deviation of normalized difference vegetation index in a square with a 2-Landsat-pixel radius (150m x 150m); a measure of forest structural variability",
                                  "Neighborhood mean of normalized difference vegetation index in a square with a 2-Landsat-pixel radius (150m x 150m)",
                                  "Neighborhood standard deviation of normalized difference vegetation index in a square with a 3-Landsat-pixel radius (210m x 210m); a measure of forest structural variability",
                                  "Neighborhood mean of normalized difference vegetation index in a square with a 3-Landsat-pixel radius (210m x 210m)",
                                  "Neighborhood standard deviation of normalized difference vegetation index in a square with a 4-Landsat-pixel radius (270m x 270m); a measure of forest structural variability",
                                  "Neighborhood mean of normalized difference vegetation index in a square with a 4-Landsat-pixel radius (270m x 270m)",
                                  "Alarm date of the fire expressed as milliseconds since epoch (1970-01-01 00:00 GMT); all pixels in each image will have the same value",
                                  "Alarm date day of the year; all pixels in each image will have the same value",
                                  "Year of the alarm date; all pixels in each image will have the same value",
                                  "Month of the alarm date; all pixels in each image will have the same value",
                                  "Day of the month of the alarm date; all pixels in each image will have the same value",
                                  "Longitude of the pixel in EPSG:4326",
                                  "Latitude of the pixel in EPSG:4326",
                                  "Whether the pixel covered yellow pine/mixed-conifer forest according to the presettlement fire regime in the 2015 version of the US Forest Service Fire Return Interval Departure dataset",
                                  "Slope of the pixel in degrees",
                                  "Aspect of the pixel in degrees",
                                  "Neighborhood standard deviation of elevation in a square with a 1-Landsat-pixel radius (90m x 90m); a measure of topographic roughness",
                                  "Neighborhood standard deviation of elevation in a square with a 2-Landsat-pixel radius (150m x 150m); a measure of topographic roughness",
                                  "Neighborhood standard deviation of elevation in a square with a 3-Landsat-pixel radius (210m x 210m); a measure of topographic roughness",
                                  "Neighborhood standard deviation of elevation in a square with a 4-Landsat-pixel radius (270m x 270m); a measure of topographic roughness",
                                  "Elevation in meters according to the Shuttle Range Topography Mission digital elevation model",
                                  "Prefire value of blue surface reflectance from Landsat satellites with radiometric correction; Band 1 for Landsat 4, 5, and 7 (0.45-0.52 μm); Band 2 for Landsat 8 (0.452-0.512 μm); scale factor of 0.0001",
                                  "Prefire value of green surface reflectance from Landsat satellites with radiometric correction; Band 2 for Landsat 4, 5, and 7 (0.52-0.60 μm); Band 3 for Landsat 8 (0.533-0.590 μm); scale factor of 0.0001",
                                  "Prefire value of red surface reflectance from Landsat satellites with radiometric correction; Band 3 for Landsat 4, 5, and 7 (0.63-0.69 μm); Band 4 for Landsat 8 (0.636-0.673 μm); scale factor of 0.0001",
                                  "Prefire value of near infrared surface reflectance from Landsat satellites with radiometric correction; Band 4 for Landsat 4, 5, and 7 (0.77-0.90 μm); Band 5 for Landsat 8 (0.851-0.879 μm); scale factor of 0.0001",
                                  "Prefire value of shortwave infrared 1 surface reflectance from Landsat satellites with radiometric correction; Band 5 for Landsat 4, 5, and 7 (1.55-1.75 μm); Band 6 for Landsat 8 (1.566-1.651 μm); scale factor of 0.0001",
                                  "Prefire value of brightness temperature (Kelvin) from Landsat satellites with radiometric correction; Band 6 for Landsat 4, 5, and 7 (10.40-12.50 μm); Band 10 for Landsat 8 (10.60-11.19 μm); scale factor of 0.1",
                                  "Prefire value of shortwave infrared 2 surface reflectance from Landsat satellites with radiometric correction; Band 7 for Landsat 4, 5, and 7 (2.08-2.35 μm); Band 7 for Landsat 8 (2.107-2.294 μm); scale factor of 0.0001",
                                  "Postfire value of blue surface reflectance from Landsat satellites with radiometric correction; Band 1 for Landsat 4, 5, and 7 (0.45-0.52 μm); Band 2 for Landsat 8 (0.452-0.512 μm); scale factor of 0.0001",
                                  "Postfire value of green surface reflectance from Landsat satellites with radiometric correction; Band 2 for Landsat 4, 5, and 7 (0.52-0.60 μm); Band 3 for Landsat 8 (0.533-0.590 μm); scale factor of 0.0001",
                                  "Postfire value of red surface reflectance from Landsat satellites with radiometric correction; Band 3 for Landsat 4, 5, and 7 (0.63-0.69 μm); Band 4 for Landsat 8 (0.636-0.673 μm); scale factor of 0.0001",
                                  "Postfire value of near infrared surface reflectance from Landsat satellites with radiometric correction; Band 4 for Landsat 4, 5, and 7 (0.77-0.90 μm); Band 5 for Landsat 8 (0.851-0.879 μm); scale factor of 0.0001",
                                  "Postfire value of shortwave infrared 1 surface reflectance from Landsat satellites with radiometric correction; Band 5 for Landsat 4, 5, and 7 (1.55-1.75 μm); Band 6 for Landsat 8 (1.566-1.651 μm); scale factor of 0.0001",
                                  "Postfire value of brightness temperature (Kelvin) from Landsat satellites with radiometric correction; Band 6 for Landsat 4, 5, and 7 (10.40-12.50 μm); Band 10 for Landsat 8 (10.60-11.19 μm); scale factor of 0.1",
                                  "Postfire value of shortwave infrared 2 surface reflectance from Landsat satellites with radiometric correction; Band 7 for Landsat 4, 5, and 7 (2.08-2.35 μm); Band 7 for Landsat 8 (2.107-2.294 μm); scale factor of 0.0001",
                                  "gridMET-derived energy release component for 3 days prior to the fire alarm date",
                                  "gridMET-derived 100-hour fuel moisture for 3 days prior to the fire alarm date",
                                  "gridMET-derived vapor pressure deficit for 3 days prior to the fire alarm date",
                                  "gridMET-derived wind velocity during the first 2 days of the fire (starting on the alarm date)",
                                  "gridMET-derived hot-windy-dry index for the first 2 days of the fire (starting on the alarm date)",
                                  "gridMET-derived vapor pressure deficit for the first 2 days of the fire (starting on the alarm date)"))


# Landsat 4, 5, and 7
# Name	Scale Factor	Description
# B1	0.0001	Band 1 (blue) surface reflectance, 0.45-0.52 μm
# B2	0.0001	Band 2 (green) surface reflectance, 0.52-0.60 μm
# B3	0.0001	Band 3 (red) surface reflectance, 0.63-0.69 μm
# B4	0.0001	Band 4 (near infrared) surface reflectance, 0.77-0.90 μm
# B5	0.0001	Band 5 (shortwave infrared 1) surface reflectance, 1.55-1.75 μm
# B6	0.1	Band 6 brightness temperature (Kelvin), 10.40-12.50 μm
# B7	0.0001	Band 7 (shortwave infrared 2) surface reflectance, 2.08-2.35 μm
# 
# Landsat 8
# Name	Scale Factor	Description
# B1	0.0001	Band 1 (Ultra Blue) surface reflectance, 0.435-0.451 μm
# B2	0.0001	Band 2 (Blue) surface reflectance, 0.452-0.512 μm
# B3	0.0001	Band 3 (Green) surface reflectance, 0.533-0.590 μm
# B4	0.0001	Band 4 (Red) surface reflectance, 0.636-0.673 μm
# B5	0.0001	Band 5 (Near Infrared) surface reflectance, 0.851-0.879 μm
# B6	0.0001	Band 6 (Shortwave Infrared 1) surface reflectance, 1.566-1.651 μm
# B7	0.0001	Band 7 (Shortwave Infrared 2) surface reflectance, 2.107-2.294 μm
# B10	0.1	Band 10 brightness temperature (Kelvin), 10.60-11.19 μm

# Write the raster band details to a metadata file
write_csv(band_metadata, path = here::here("data/data_output/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2018_raster-metadata.csv"))

