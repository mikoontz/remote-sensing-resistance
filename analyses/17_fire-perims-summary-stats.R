library(tidyr)
library(lubridate)
library(sf)
library(dplyr)
library(here)

if (file.exists(here::here("data/data_output/region-5-geospatial-fires_sn_ypmc/region-5-geospatial-fires_sn_ypmc.shp"))) {
  r5_sn_ypmc <- 
    st_read(here::here("data/data_output/region-5-geospatial-fires_sn_ypmc/region-5-geospatial-fires_sn_ypmc.shp"))
} else {
  source(here::here("data/data_carpentry/16_ypmc-pixel-count-usfs-r5.R"))
}

if (file.exists(here::here("data/data_output/all-fire-samples_configured.geoJSON"))) {
  ss_burned <- sf::st_read(here::here("data/data_output/burned-fire-samples_configured.geoJSON"), stringsAsFactors = FALSE)
} else {
  source(here::here("data/data_carpentry/11_configure-fire-samples.R"))
}

total_conifer_samps <- 
  ss_burned %>% 
  filter(conifer_forest == 1) %>% 
  nrow()

frap_sn_ypmc <- st_read(here::here("data/data_output/fire_perim/fire18_1_sn_ypmc/fire18_1_sn_ypmc.shp"))

# Approxmiately many fires would be included under the Steel et al. 2018
# criteria (> 50% burning in yellow pine mixed conifer)
# About 277
r5_sn_ypmc %>% 
  filter((ypmc_pxls * 30 * 30) > (0.5 * as.numeric(st_area(.))))

# Area and number of fires comparison between USFS Region 5 geospatial dataset and FRAP-derived dataset

# This is the number of Region 5 fires that burned at least partially in YPMC
r5_fire_count_ypmc <- 
  r5_sn_ypmc %>% 
  filter(ypmc_pxls != 0) %>% 
  nrow()

# This is the area of yellow pine/mixed-conifer covered by the Region 5 perimeters
# in hectares
r5_area_ypmc_burned <-
  r5_sn_ypmc %>% 
  summarize(total_area_ypmc = sum(ypmc_pxls) * 30 * 30 / 10000) %>% 
  pull(total_area_ypmc)

# This is the number of FRAP-derived fires (our algorithm) that burned at least
# partially in YPMC

frap_fire_count_ypmc <- length(unique(ss_burned$fire_id))

frap_area_ypmc_burned <-
  frap_sn_ypmc %>% 
  summarize(total_area_ypmc = sum(ypmc_pxls) * 30 * 30 / 10000) %>% 
  pull(total_area_ypmc)

# How many more hectares are covered by our new dataset?
(frap_area_ypmc_burned - r5_area_ypmc_burned)

fire_perims_summary_stats_list <-
  list(frap_area_ypmc_burned = frap_area_ypmc_burned,
       frap_fire_count_ypmc = frap_fire_count_ypmc,
       r5_area_ypmc_burned = r5_area_ypmc_burned,
       r5_fire_count_ypmc = r5_fire_count_ypmc,
       total_conifer_samps = total_conifer_samps)

write_rds(fire_perims_summary_stats_list, here::here("analyses/analyses_output/fire-perims-summary-stats-list.rds"))
