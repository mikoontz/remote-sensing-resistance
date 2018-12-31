# Purpose: configure the metadata and the bandnames for the export of all severity measurements derived from the FRAP dataset.

library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(rasterVis)
library(here)

# The original FRAP perimeter representing all fires that burned in yellow pine/mixed-conifer forest in the Sierra Nevada, California

frap_ypmc_perims <- 
  st_read(here::here("data/data_output/fire_perim/fire17_1_sn_ypmc/fire17_1_sn_ypmc.shp"), stringsAsFactors = FALSE) %>% 
  dplyr::mutate(year_ = as.numeric(year_)) %>% 
  dplyr::rename(fire_name = fire_nam, 
                alarm_date = alrm_dt, 
                cont_date = cont_dt, 
                comments = commnts,
                report_ac = reprt_c,
                gis_acres = gis_crs,
                c_method = c_methd,
                objective = objectv,
                ypmc_pixel_count = mxd_cn_) %>% 
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

ee_metadata <- read_csv(here::here("data/ee_fire-samples/fires-strat-samples_metadata_2017_48-day-window_L4578_none-interp_all.csv")) %>% 
  dplyr::rename(fire_id = `system:index`) %>% 
  dplyr::select(-`.geo`) %>% 
  dplyr::rename(ypmc_pixel_count = mxd_cn_) %>% 
  # dplyr::mutate(alarm_year = year(epoch + milliseconds(alarm_date)),
  #               alarm_month = month(epoch + milliseconds(alarm_date)),
  #               alarm_day = day(epoch + milliseconds(alarm_date)),
  #               cont_year = year(epoch + milliseconds(alarm_date)),
  #               cont_month = month(epoch + milliseconds(alarm_date)),
  #               cont_day = day(epoch + milliseconds(alarm_date))) %>% 
  dplyr::select(-c(alarm_date, cont_date, year_))

ftr_col_metadata <- left_join(frap_ypmc_perims, ee_metadata)

ftr_col_metadata %>% 
  dplyr::filter(!is.na(fire_id)) %>% 
  dplyr::filter(alarm_year > 1983) %>% 
  dim()

severity_imgs_filenames <- list.files(here::here("data/data_output/ee-frap-derived-fire-imagery/"))
severity_imgs_fire_ids <- substr(x = severity_imgs_filenames, start = 16, stop = 35)

img_col_metadata <- 
  data_frame(fire_id = severity_imgs_fire_ids, sev_img_present = TRUE) %>% 
  full_join(ftr_col_metadata) %>% 
  dplyr::mutate(sev_img_present = ifelse(is.na(sev_img_present), yes = FALSE, no = sev_img_present)) %>% 
  dplyr::filter(sev_img_present) %>% 
  dplyr::select(-sev_img_present) %>% 
  st_as_sf()

test_img_name <- "HAMM"
test_img_year <- 1987

test_img_ftr <- 
  img_col_metadata %>% 
  dplyr::filter(fire_name == test_img_name) %>% 
  dplyr::filter(alarm_year == test_img_year)

test_img_id <-
  test_img_ftr %>% 
  dplyr::pull(fire_id)

test_img <- raster::brick(list.files(here::here("data/data_output/ee-frap-derived-fire-imagery"), pattern = test_img_id, full.names = TRUE)[1])
test_img_filename <- list.files(here::here("data/data_output/ee-frap-derived-fire-imagery"), pattern = test_img_id)[1]

band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw')

names(test_img) <- band_names

burned_unburned_rbr_threshold <- 0.041191844
high_sev_low_sev_rbr_threshold <- 0.2836425

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

breaks <- c(cellStats(test_img[["rbr"]], min), seq(burned_unburned_rbr_threshold, high_sev_low_sev_rbr_threshold, length.out = 100), cellStats(test_img[["rbr"]], max))
# cols <- colorRampPalette(c('#008000', RBR_viz, '#ff0000'))
cols <- colorRampPalette(RBR_viz)

levelplot(test_img[["rbr"]], at = breaks, col.regions = cols)

plot(test_img[["rbr"]])
plot(st_transform(st_geometry(test_img_ftr), 3310), add = TRUE)

