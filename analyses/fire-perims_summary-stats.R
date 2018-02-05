library(tidyr)
library(lubridate)
library(sf)
library(lme4)
library(dplyr)
library(ggplot2)

frap <- st_read(dsn = "data/features/fire_perim/fire_perim_sn_16_1_shp/",
                 stringsAsFactors = FALSE)
sn <- st_read("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% 
  st_transform(4326)

samps1 <- st_read("data/fire_samples/fires-strat-samples_48-day-window_L45_none-interp.geojson")
samps2 <- st_read("data/fire_samples/fires-strat-samples_48-day-window_L578_none-interp.geojson")
samps3 <- st_read("data/fire_samples/fires-strat-samples_48-day-window_L78_none-interp.geojson")
samps <- rbind(samps1, samps2, samps3)

# Extract unique Fire ID from the sample point IDs
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)
samps$samp_id <- as.numeric(substr(as.character(samps$id), start = 22, stop = nchar(as.character(samps$id))))

samps %>% 
  group_by(fire_id) %>% 
  tally()

samps %>% 
  filter(conifer_forest == 1) %>% 
  group_by(fire_id) %>% 
  tally()

s <-
  samps %>%
  filter(conifer_forest == 1) %>% 
  group_by(agency, alarm_date, cause, comments, date, fire_name, fire_num, gis_acres, inc_num, objective, ordinal_day, shape_area, shape_leng, state, unit_id, year_, c_method, report_ac, cont_date, fire_id) %>% 
  nest()

head(s)  

