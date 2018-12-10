library(tidyr)
library(lubridate)
library(sf)
library(dplyr)
library(here)

if (file.exists(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp"))) {
  r5_sn_mc <- 
    st_read(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp")) %>% 
    dplyr::rename(FIRE_YEAR = FIRE_YE,
                  FIRE_NAME = FIRE_NA,
                  mixed_con_pixels = mxd_cn_)
} else {
  source(here::here("data/data_carpentry/ypmc-pixel-count-usfs-r5-vs-frap.R"))
}

if (file.exists(here::here("data/data_output/all-fire-samples_configured.rds"))) {
  ss <- readRDS(here::here("data/data_output/all-fire-samples_configured.rds"))
  ss_burned <- readRDS(here::here("data/data_output/burned-fire-samples_configured.rds"))
} else {
  source(here::here("data/data_carpentry/configure_fire_samples.R"))
}

total_conifer_samps <- 
  ss_burned %>% 
  filter(conifer_forest == 1) %>% 
  nrow()

s <-
  ss_burned %>%
  filter(conifer_forest == 1) %>% 
  group_by(fire_id, year, gis_acres, agency, alarm_date, cont_date, cause, comments, date, fire_name, fire_num, inc_num, objective, ordinal_day, state, unit_id, c_method, report_ac, mxd_cn_) %>% 
  nest()

# Approxmiately many fires would be included under the Steel et al. 2018
# criteria (> 50% burning in yellow pine mixed conifer)
r5_sn_mc %>% 
  filter((mixed_con_pixels * 30 * 30) > (0.5 * as.numeric(st_area(.))))

# Area and number of fires comparison between USFS Region 5 geospatial dataset and FRAP-derived dataset

# This is the number of Region 5 fires that burned at least partially in YPMC
r5_fire_count_ypmc <- 
  r5_sn_mc %>% 
  filter(mixed_con_pixels != 0) %>% 
  nrow()

# This is the area of yellow pine/mixed-conifer covered by the Region 5 perimeters
# in hectares
r5_area_ypmc_burned <-
  r5_sn_mc %>% 
  summarize(total_area_ypmc = sum(mixed_con_pixels) * 30 * 30 / 10000) %>% 
  pull(total_area_ypmc)

# This is the number of FRAP-derived fires (our algorithm) that burned at least
# partially in 
frap_fire_count_ypmc <- nrow(s)

frap_area_ypmc_burned <-
  s %>% 
  summarize(total_area_ypmc = sum(mxd_cn_) * 30 * 30 / 10000) %>% 
  pull(total_area_ypmc)

# How many more hectares are covered by our new dataset?
(frap_area_ypmc_burned - r5_area_ypmc_burned)
