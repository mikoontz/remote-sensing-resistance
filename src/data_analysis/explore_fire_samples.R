library(rethinking)
library(sf)
library(lme4)

sn <- st_read("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% 
  st_transform(4326)
samps <- st_read("data/fire_samples/fires-strat-samples_1-month-window_L5_bicubic-interp.geojson")

glimpse(samps)
samps$fire_id <- substr(as.character(samps$id), start = 1, stop = 20)

plot(samps$geometry[samps$conifer_forest == 1], pch = 19)
plot(sn$geometry, add = TRUE)

# 1503 fires represented from the FRAP database with Landsat 5 imagery that can be processed
length(unique(samps$fire_id))

## Topography variables
# Adjust aspect to reflect "heat load index" (McCune and Keon, 2002) with southwest having a value of 1, and northeast having a value of -1
circular_aspect <- function(aspect) {
  
  new_aspect <- ((-1 * (aspect)) * pi / 180) - (3/4 * pi)
  return(cos(new_aspect))
  
}

samps$c_aspect <- circular_aspect(samps$aspect)

# Calculate predicted potential annual direct incident radiation (also from McCune and Keon, 2002)
# which combines slope, aspect, and latitude

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
pdir <- function(lat, slope, folded_asp) {
  lat <- lat * pi / 180
  slope <- slope * pi / 180
  exp(-1.467 + 1.582 * cos(lat) * cos(slope) - 1.5 * cos(folded_asp) * sin(slope) * sin(lat) - 0.262 * sin(lat) * sin(slope) + 0.607 * sin(folded_asp) * sin(slope))
}

samps$pdir <- pdir(samps$lat, samps$slope, samps$folded_aspect)

topo_vars <- c("c_aspect", "pdir", "elev", "lat", "lon", "slope", "topo_roughness_1", "topo_roughness_2", "topo_roughness_3", "topo_roughness_4")

## Timing variables
# Adjust ordinal day to reflect "middle of summer-ness" with August 6th (the 218th day of the year) getting 
# a 1 and February 4th (the 35th day of the year) getting a value of -1
circular_doy <- function(doy) {
  
  new_doy <- ((doy - 218) / 365) * 2 * pi
  return(cos(new_doy))
  
}

samps$c_doy <- circular_doy(samps$ordinal_day)

# Make year an integer
samps$year_ <- as.numeric(samps$year_)

timing_vars <- c("year_", "alarm_date", "cont_date", "c_doy")

## Vegetation variables
veg_vars <- c("focal_mean_evi_1", "focal_mean_evi_2", "focal_mean_evi_3", "focal_mean_evi_4", 
              "focal_mean_ndvi_1", "focal_mean_ndvi_2", "focal_mean_ndvi_3", "focal_mean_ndvi_4",
              "focal_mean_ndwi_1", "focal_mean_ndwi_2", "focal_mean_ndwi_3", "focal_mean_ndwi_4", 
              "het_evi_1", "het_evi_2", "het_evi_3", "het_evi_4", 
              "het_ndvi_1", "het_ndvi_2", "het_ndvi_3", "het_ndvi_4",
              "het_ndwi_1", "het_ndwi_2", "het_ndwi_3", "het_ndwi_4", 
              "preFire_evi",
              "preFire_ndvi",
              "preFire_ndwi")

## Fire weather variables
fireWeather_vars <- c("erc", "fm100", "tmmx")

## All vars to scale
all_vars <- c(topo_vars, timing_vars, veg_vars, fireWeather_vars)

# Just focus on the points from the mixed conifer/yellow pine forest
mixed_con <- 
  samps %>%
  filter(conifer_forest == 1)

# How many fires have samples from mixed conifer regions in the Sierra Nevada?
# 31981 points from 715 fires
mixed_con %>%
  group_by(fire_id) %>%
  count()

# What predictors could we use?
glimpse(mixed_con)

## Scale the predictors (ss = scaled samples)
ss <-
  mixed_con %>%
  mutate_at(.vars = all_vars, .funs = funs(s = as.numeric(scale(.))))

glimpse(ss)  

# We will use the Relative Burn Ratio (RBR) from Parks et al (2014) as our response variable
m1 <- lmer(RBR ~ het_ndvi_1_s * fm100_s + preFire_ndvi_s + topo_roughness_1_s + pdir_s + (1 | fire_id), data = ss)
m2 <- lmer(RBR ~ het_ndvi_2_s * fm100_s + preFire_ndvi_s + topo_roughness_2_s + pdir_s + (1 | fire_id), data = ss)
m3 <- lmer(RBR ~ het_ndvi_3_s * fm100_s + preFire_ndvi_s + topo_roughness_3_s + pdir_s + (1 | fire_id), data = ss)
m4 <- lmer(RBR ~ het_ndvi_4_s * fm100_s + preFire_ndvi_s + topo_roughness_4_s + pdir_s + (1 | fire_id), data = ss)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)

pairs(samps[, c("RdNBR", "RdNBR2", "RdNDVI", "RdEVI", "RBR"), drop = TRUE])

# Consider switching to a "high severity" / "not high severity" boolean response
# From the CBI calibration, we know that the 1-month window, bicubic interpolation of RBR
# is a "high severity" pixel when RBR > 0.2904600; Note that "high severity" is
# equivalent to "stand replacing"

# Many (all?) researchers use the categorical response because remotely-sensed severity
# metrics tend to have a non-linear relationship with on-the-ground severity

ss$stand_replacing <- ifelse(ss$RBR > 0.2904600, yes = 1, no = 0)

# Our modeling effort then becomes a generalized linear model with a logit link, estimating
# how different covariate affect the *probability* of a high severity fire

glm1 <- glmer(stand_replacing ~ het_ndvi_1_s * fm100_s + preFire_ndvi_s + topo_roughness_1_s + pdir_s + (1 | fire_id), 
              data = ss, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))

summary(glm1)

plot(ss$het_ndvi_1, ss$RBR)

# The theory that we'd **really** like to test is that heterogeneity matters under some fuel/weather conditions,
# but is overwhelmed by extreme conditions