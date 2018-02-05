# We use this analysis for stats about the CBI plots

library(dplyr)
library(lubridate)
library(sf)

# Get 48-day window, bicubic interpolation data
cbi_48_bicubic <- st_read("data/cbi_calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)
cbi_48_bicubic$date <- as.POSIXct(cbi_48_bicubic$date / 1000, origin="1970-01-01")

conifer_only <- subset(cbi_48_bicubic, subset = conifer_forest == 1)

total_plots <- nrow(cbi_48_bicubic)
total_conifer_plots <- nrow(conifer_only)

total_conifer_fires <- n_distinct(conifer_only$fire_name)
total_conifer_years <- n_distinct(year(conifer_only$date))

sort(unique(year(conifer_only$date)))
