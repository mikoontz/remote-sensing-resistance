# Merge the fire samples into a single rds file for lighter weight storage

library(sf)
library(dplyr)
library(purrr)
library(here)

samps_files <- list.files(path = here::here("data/ee_fire-samples"), full.names = TRUE)

# These are the old fire samples
samps_files <-
  c("data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L45_none-interp.geojson", 
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L578_none-interp.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_1.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_2.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_3.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_4.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_5.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_6.geojson")

# These are the most recently collected fire samples that include texture values too.
# samps_files <- samps_files[grepl(samps_files, pattern = "_[0-9].geojson")] # This line will work if/when we delete the older samples
samps_files <- 
  c("data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_01.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_02.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_03.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_04.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_05.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_06.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_07.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_08.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_09.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_10.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_11.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_12.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_13.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_rough.geojson")

samps_files <- 
  c("data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_all.geojson",
    "data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L4578_none-interp_rough.geojson")


samps_list <-
  samps_files %>%
  map(.f = function(file_name) {
    st_read(here::here(file_name))
  })

samps <- do.call(rbind, samps_list)

saveRDS(samps, file = here::here("data/data_output/all-fire-samples.rds"))
