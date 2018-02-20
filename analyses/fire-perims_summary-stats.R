library(tidyr)
library(lubridate)
library(sf)
library(lme4)
library(dplyr)
library(ggplot2)

frap <- st_read(dsn = "data/features/fire_perim/fire_perim_sn_16_1_shp/",
                 stringsAsFactors = FALSE) %>% 
  st_transform(4326)

usfs <- st_read(dsn = "data/features/fire_perim/veg_severity_perimeters16_1.gdb/",
                stringsAsFactors = FALSE) %>% 
  mutate(geometry = st_sfc(Shape)) %>% 
  st_set_geometry("geometry") %>% 
  select(-Shape) %>% 
  st_transform(4326)

sn <- st_read("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% 
  st_transform(4326)

usfs_sn <- st_intersection(x = usfs, y = sn)
# 
# frap %>%
#   filter(!is.na(ALARM_DATE)) %>% 
#   dim()

if (file.exists("data/data_output/all-fire-samples.rds")) {
  load("data/data_output/all-fire-samples.rds")
} else {
  source("data/data_carpentry/merge_fire_samples.R")
}

samps

total_conifer_samps <- nrow(samps %>% filter(conifer_forest == 1))

s <-
  samps %>%
  filter(conifer_forest == 1) %>% 
  group_by(fire_id, year, gis_acres, agency, alarm_date, cont_date, cause, comments, date, fire_name, fire_num, inc_num, objective, ordinal_day, shape_area, shape_leng, state, unit_id, c_method, report_ac) %>% 
  nest()

total_conifer_fires <- nrow(s)
conifer_year_range <- range(s$year)

total_fires_usfs <- nrow(usfs_sn)
unique(usfs_sn$FIRE_YEAR)
