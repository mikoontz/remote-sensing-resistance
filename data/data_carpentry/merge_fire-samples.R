# Merge the fire samples into a single rds file for lighter weight storage

library(sf)
library(dplyr)
library(purrr)

samps_files <- list.files(path = here::here("data/ee_fire-samples"), full.names = TRUE)
samps_files <- samps_files[grepl(samps_files, pattern = "_[0-9].geojson")]

samps_files <-
  c("/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L45_none-interp.geojson", 
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L578_none-interp.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_1.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_2.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_3.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_4.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_5.geojson",
    "/Users/mikoontz/dev/manuscripts/remote-sensing-resistance/data/ee_fire-samples/fires-strat-samples_2017_48-day-window_L78_none-interp_6.geojson")

samps_list <-
  samps_files %>%
  map(.f = function(file_name) {
    st_read(file_name)
  })

samps <- do.call(rbind, samps_list)

saveRDS(samps, file = here::here("data/data_output/all-fire-samples.rds"))
