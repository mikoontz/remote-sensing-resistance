# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

library(lubridate)
library(sf)
library(tidyverse)
library(here)
library(lubridate)
library(brms)
library(purrr)

samps <- st_read("data/data_output/ee_fire-samples/fires-strat-samples_2018_48-day-window_L4578_none-interp.geojson") 

# Extract unique Fire ID from the sample point IDs
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)
samps$samp_id <- as.numeric(substr(as.character(samps$id), start = 22, stop = nchar(as.character(samps$id))))
samps$year <- as.numeric(as.character(samps$year_))
samps <- dplyr::select(samps, -year_)

circular_aspect <- function(aspect) {
  
  new_aspect <- ((-1 * (aspect)) * pi / 180) - (3/4 * pi)
  return(cos(new_aspect))
  
}

samps$c_aspect <- circular_aspect(samps$aspect)

fold_aspect <- function(asp) {
  
  rad <- asp * pi / 180
  folded_aspect <- pi - abs(rad - pi)
  return(folded_aspect)
  
}

samps$folded_aspect <- fold_aspect(samps$aspect)

# G column: latitude
# H column: slope
# I column: folded aspect

# Potential annual heat load
pahl <- function(lat, slope, folded_asp) {
  lat <- lat * pi / 180
  slope <- slope * pi / 180
  exp(-1.467 + 1.582 * cos(lat) * cos(slope) - 1.5 * cos(folded_asp) * sin(slope) * sin(lat) - 0.262 * sin(lat) * sin(slope) + 0.607 * sin(folded_asp) * sin(slope))
}

topo_vars <- c("c_aspect", "pahl", "elev", "lat", "lon", "slope", "topo_roughness_1", "topo_roughness_2", "topo_roughness_3", "topo_roughness_4")


samps$pahl <- pahl(samps$lat, samps$slope, samps$folded_aspect)

circular_doy <- function(doy) {
  
  new_doy <- ((doy - 218) / 365) * 2 * pi
  return(cos(new_doy))
  
}

samps$c_doy <- circular_doy(samps$ordinal_day)

timing_vars <- c("alarm_year", "alarm_month", "alarm_day", "alarm_date", "cont_date", "c_doy")

# This code is for the veg_vars when the GLCM-based texture values are part of the samples
veg_vars <-
  sapply(X = 1:4,
         FUN = function(i) paste(
           c(
             "focal_mean_ndvi",
             "het_ndvi"
           ), i, sep = "_")) %>%
  as.vector() %>%
  c("preFire_ndvi")


# This code is for when additional gridMET derivatives are part of the samples
fireWeather_vars <- c("erc", "fm100", "hdw")

all_vars <- c(topo_vars, timing_vars, veg_vars, fireWeather_vars)

mixed_con <- 
  samps %>%
  filter(conifer_forest == 1)

ss <-
  mixed_con %>%
  mutate_at(.vars = all_vars, .funs = list(s = scale))

model_summary <- read.csv(here::here("analyses/analyses_output/cbi-calibration-model-comparison.csv"), stringsAsFactors = FALSE)
target_model <- model_summary[model_summary$response == "RBR" &
                                model_summary$time_window == 48 &
                                model_summary$interpolation == "bicubic", ]

# Here are the other thresholds for the model using bicubic interpolation and a 48 day window:
#   
# target_model$unchanged equates to a CBI of 0
# target_model$low_sev equates to a CBI of 0.1 -- threshold between "unchanged" and "low"
# target_model$mod_se equates to a CBI of 1.25 -- threshold between "low" and "medium"
# target_model$hi_se equates to a CBI of 2.25 -- threshold between "medium" and "high"

ss$stand_replacing <- ifelse(ss$RBR > target_model$hi_sev, yes = 1, no = 0)

# Consider also subsetting to only places that burned to avoid any impact of unburned area that happens to be within the fire perimeter.

ss_burned <- ss %>% filter(ss$RBR > target_model$low_sev)

remoteSev_to_cbi <- function(data, response, a, b, c) {
  log((data[, response, drop = TRUE] - a) / b) / c
}

ss_burned$cbi <- remoteSev_to_cbi(data = ss_burned, response = "RBR", a = target_model$a, b = target_model$b, c = target_model$c)

sf::st_write(obj = ss, dsn = here::here("data/data_output/all-fire-samples_configured.geoJSON"))
sf::st_write(obj = ss_burned, dsn = here::here("data/data_output/burned-fire-samples_configured.geoJSON"))
