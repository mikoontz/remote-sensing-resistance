# Merge the fire samples into a single rds file for lighter weight storage

library(sf)
library(dplyr)

samps1 <- st_read(here::here("data/ee_fire-samples/fires-strat-samples_48-day-window_L45_none-interp.geojson"))
samps2 <- st_read(here::here("data/ee_fire-samples/fires-strat-samples_48-day-window_L578_none-interp.geojson"))
samps3 <- st_read(here::here("data/ee_fire-samples/fires-strat-samples_48-day-window_L78_none-interp.geojson"))

samps <- rbind(samps1, samps2, samps3)

# Extract unique Fire ID from the sample point IDs
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)
samps$samp_id <- as.numeric(substr(as.character(samps$id), start = 22, stop = nchar(as.character(samps$id))))
samps$year <- as.numeric(as.character(samps$year_))
samps <- select(samps, -year_)

save(samps, list = "samps", file = here::here("data/data_output/all-fire-samples.rds"))

samps_files <- list.files(here::here("data/ee_fire-samples/"), full.names = TRUE)
texture_files <- samps_files[grepl(pattern = "texture", x = samps_files)]

texture_list <- lapply(X = texture_files, FUN = st_read)
texture_list <- lapply(texture_list, FUN = function(x) { x[, sort(colnames(x))] } )

texture_samps <- do.call(rbind, texture_list)

save(texture_samps, list = "texture_samps", file = here::here("data/data_output/all-fire-samples_texture.rds"))
