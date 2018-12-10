# Merge the fire samples into a single rds file for lighter weight storage

library(sf)
library(dplyr)
library(purrr)

# samps_files <- list.files(path = here::here("data/ee_fire-samples"), full.names = TRUE)
# samps_list <- 
#   samps_files %>% 
#   map(.f = function(file_name) {
#     st_read(file_name)
#   })
# 
# samps <- do.call(rbind, samps_list)

saveRDS(samps, file = here::here("data/data_output/all-fire-samples.rds"))
