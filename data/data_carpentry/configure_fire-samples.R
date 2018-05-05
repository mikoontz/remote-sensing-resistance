# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

library(lubridate)
library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(brms)


fires <- st_read(dsn = here::here("/data/features/fire_perim/fire_perim_sn_16_1_shp/"),
                 stringsAsFactors = FALSE) %>% 
  st_transform(4326)
sn <- st_read(here::here("/data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp")) %>% 
  st_transform(4326)

if (!file.exists(here::here("data/data_output/all-fire-samples_texture.rds"))) {
  source(here::here("data/data_carpentry/merge_fire-samples.R"))
}

load(here::here("data/data_output/all-fire-samples_texture.rds"))

samps <- texture_samps

# Extract unique Fire ID from the sample point IDs
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)

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

# Potential 
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

samps$year_ <- as.numeric(as.character(samps$year_))

timing_vars <- c("year_", "alarm_date", "cont_date", "c_doy")

veg_vars <-
  sapply(X = 1:4, 
         FUN = function(i) paste(
           c(
             "focal_mean_ndvi",
             "focal_mean_ndwi",
             "het_ndvi",
             "het_ndwi",
             "ndvi_asm",
             "ndvi_contrast",
             "ndvi_corr",
             "ndvi_dent",
             "ndvi_diss",
             "ndvi_dvar",
             "ndvi_ent",
             "ndvi_idm",
             "ndvi_imcorr1",
             "ndvi_imcorr2",
             "ndvi_inertia",
             "ndvi_prom",
             "ndvi_savg",
             "ndvi_sent",
             "ndvi_shade",
             "ndvi_svar",
             "ndvi_var"
           ), i, sep = "_")) %>% 
  as.vector() %>% 
  c("preFire_ndvi", "preFire_ndwi")

fireWeather_vars <- c("erc", "fm100", "tmmx")

all_vars <- c(topo_vars, timing_vars, veg_vars, fireWeather_vars)

mixed_con <- 
  samps %>%
  filter(conifer_forest == 1)

ss <-
  mixed_con %>%
  mutate_at(.vars = all_vars, .funs = funs(s = as.numeric(scale(.))))

model_summary <- read.csv(here::here("data/data_output/cbi_calibration_model_comparison.csv"), stringsAsFactors = FALSE)
target_model <- model_summary[model_summary$response == "RBR" &
                                model_summary$time_window == 48 &
                                model_summary$interpolation == "bicubic", ]

ss$stand_replacing <- ifelse(ss$RBR > target_model$hi_sev, yes = 1, no = 0)

# Extreme percentiles correspeond to 80th, 90th, and 97.5th percentiles of 100 hour
# fuel moisture
extreme_fm100_percentiles <- c(7.7, 6.6, 4.2)
ss$extreme80_fm100 <- ifelse(ss$fm100 < 7.7, yes = 1, no = 0)
ss$extreme90_fm100 <- ifelse(ss$fm100 < 6.6, yes = 1, no = 0)
ss$extreme97.5_fm100 <- ifelse(ss$fm100 < 4.2, yes = 1, no = 0)

ss_burned <- ss %>% filter(ss$RBR > target_model$low_sev)

remoteSev_to_cbi <- function(data, response, a, b, c) {
  log((data[, response, drop = TRUE] - a) / b) / c
}

ss_burned$cbi <- remoteSev_to_cbi(data = ss_burned, response = "RBR", a = target_model$a, b = target_model$b, c = target_model$c)

save(ss, file = here::here("data/data_output/all-fire-samples_texture_configured.rds"))
save(ss_burned, file = here::here("data/data_output/burned-fire-samples_texture_configured.rds"))
