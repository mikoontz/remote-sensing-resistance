# We use this analysis for stats about the CBI plots

library(dplyr)
library(lubridate)
library(sf)
library(here)

# Get 48-day window, bicubic interpolation data
cbi_48_bicubic <- st_read(here::here("data/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson"), stringsAsFactors = FALSE)
cbi_48_bicubic$date <- as.POSIXct(cbi_48_bicubic$date / 1000, origin="1970-01-01")

conifer_only_cbi <- subset(cbi_48_bicubic, subset = conifer_forest == 1)

total_plots <- nrow(cbi_48_bicubic)
total_conifer_cbi_plots <- nrow(conifer_only_cbi)

total_conifer_cbi_fires <- n_distinct(conifer_only_cbi$fire_name)
total_conifer_cbi_years <- sort(unique(year(conifer_only_cbi$date)))

sort(unique(year(conifer_only_cbi$date)))

cbi_summary_stats_list <-
  list(total_conifer_cbi_plots = total_conifer_cbi_plots,
       total_conifer_cbi_fires = total_conifer_cbi_fires,
       total_conifer_cbi_years = total_conifer_cbi_years,
       total_plots = total_plots)

write_rds(cbi_summary_stats_list, here::here("analyses/analyses_output/cbi_summary_stats_list.rds"))
