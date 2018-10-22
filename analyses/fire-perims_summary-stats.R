library(tidyr)
library(lubridate)
library(sf)
library(dplyr)
library(here)

frap <- st_read(dsn = here::here("data/features/fire_perim/fire_perim_sn_16_1_shp/"),
                 stringsAsFactors = FALSE) %>% 
  st_transform(4326)

if (file.exists(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp"))) {
  r5_sn_mc <- 
    st_read(here::here("data/data_output/region-5-geospatial-fires_sn_mixed-conifer/region-5-geospatial-fires_sn_mixed-conifer.shp")) %>% 
    rename(FIRE_YEAR = FIRE_YE,
           FIRE_NAME = FIRE_NA,
           mixed_con_pixels = mxd_cn_)
} else {
  source(here::here("data/data_carpentry/filter-sn_usfs_fire_perims.R"))
}

if (file.exists(here::here("data/data_output/all-fire-samples.rds"))) {
  load(here::here("data/data_output/all-fire-samples.rds"))
} else {
  source(here::here("data/data_carpentry/merge_fire_samples.R"))
}

samps

total_conifer_samps <- 
  samps %>% 
  filter(conifer_forest == 1) %>% 
  nrow()

s <-
  samps %>%
  filter(conifer_forest == 1) %>% 
  group_by(fire_id, year, gis_acres, agency, alarm_date, cont_date, cause, comments, date, fire_name, fire_num, inc_num, objective, ordinal_day, shape_area, shape_leng, state, unit_id, c_method, report_ac) %>% 
  nest()

total_conifer_fires <- nrow(s)
conifer_year_range <- range(s$year)

total_fires_usfs <- 
  r5_sn_mc %>% 
  filter(mixed_con_pixels != 0) %>% 
  nrow()
unique(r5_sn_mc$FIRE_YEAR)
