library(raster)
library(dplyr)
library(sf)

lookup <- read.csv("data/features/landcover_LANDFIRE/landfire_biophysical_settings_lookup_table.csv")
bps_sn <- raster("data/features/landcover_LANDFIRE/landfire_biophysical_settings_sn.tif")

nsn <- st_read("data/features/fire_return_interval_departure/FRID_NorthSierra15_1.gdb/")

colnames(nsn)
nsn[1:5, ]
forest <- c()

unique(nsn$PFR)
