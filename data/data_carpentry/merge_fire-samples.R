# Merge the fire samples into a single rds file for lighter weight storage

library(sf)
library(dplyr)
library(purrr)

samps_files <- list.files(path = here::here("data/ee_fire-samples"), full.names = TRUE)
samps_list <- 
  samps_files %>% 
  map(.f = function(file_name) {
    st_read(file_name)
  })

samps <- do.call(rbind, samps_list)

# Extract unique Fire ID from the sample point IDs
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)
samps$samp_id <- as.numeric(substr(as.character(samps$id), start = 22, stop = nchar(as.character(samps$id))))
samps$year <- as.numeric(as.character(samps$year_))
samps <- dplyr::select(samps, -year_)

saveRDS(samps, file = here::here("data/data_output/all-fire-samples.rds"))