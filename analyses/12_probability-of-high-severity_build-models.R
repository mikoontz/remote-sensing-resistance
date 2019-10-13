# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

library(lubridate)
library(sf)
library(tidyverse)
library(here)
library(lubridate)
library(brms)

if(!file.exists(here::here("data/data_output/burned-fire-samples_configured.csv"))) {
  source(here::here("data/data_carpentry/11_configure-fire-samples.R"))
}

# This .csv file represents the burned samples extracted from Earth Engine
# That is, the severity was over the threshold corresponding to a CBI of 0.1
ss_burned <- read_csv(here::here("data/data_output/burned-fire-samples_configured.csv"))

# Severe or not as bernoulli response, heterogeneity, preFire NDVI, prefire neighborhood mean NDVI, heterogeneity interacts with fm100, preFire NDVI, and neighborhood mean NDVI, only samples that were burned

(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- brm(stand_replacing ~ 
                                                                     het_ndvi_1_s +
                                                                     focal_mean_ndvi_1_s +
                                                                     preFire_ndvi_s +
                                                                     fm100_s +
                                                                     pahl_s + 
                                                                     topo_roughness_1_s + 
                                                                     focal_mean_ndvi_1_s:preFire_ndvi_s +
                                                                     het_ndvi_1_s:focal_mean_ndvi_1_s +
                                                                     het_ndvi_1_s:preFire_ndvi_s +
                                                                     het_ndvi_1_s:fm100_s +
                                                                     (1 | fire_id), 
                                                                   family = bernoulli(), 
                                                                   data = ss_burned, 
                                                                   sample_prior = TRUE, 
                                                                   iter = 4000, 
                                                                   chains = 4, 
                                                                   cores = 4,
                                                                   prior = c(
                                                                     prior(prior = normal(0, 1), class = b),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_1_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = topo_roughness_1_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_1_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:focal_mean_ndvi_1_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:fm100_s),
                                                                     prior(prior = student_t(3, 0, 10), class = Intercept),
                                                                     prior(prior = student_t(3, 0, 10), class = sd),
                                                                     prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                                   ))
(Sys.time() - start)

(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- brm(stand_replacing ~ 
                                                                     het_ndvi_2_s +
                                                                     focal_mean_ndvi_2_s +
                                                                     preFire_ndvi_s +
                                                                     fm100_s +
                                                                     pahl_s + 
                                                                     topo_roughness_2_s + 
                                                                     focal_mean_ndvi_2_s:preFire_ndvi_s +
                                                                     het_ndvi_2_s:focal_mean_ndvi_2_s +
                                                                     het_ndvi_2_s:preFire_ndvi_s +
                                                                     het_ndvi_2_s:fm100_s +
                                                                     (1 | fire_id), 
                                                                   family = bernoulli(), 
                                                                   data = ss_burned, 
                                                                   sample_prior = TRUE, 
                                                                   iter = 4000, 
                                                                   chains = 4,
                                                                   cores = 4,
                                                                   prior = c(
                                                                     prior(prior = normal(0, 1), class = b),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_2_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = topo_roughness_2_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_2_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:focal_mean_ndvi_2_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:fm100_s),
                                                                     prior(prior = student_t(3, 0, 10), class = Intercept),
                                                                     prior(prior = student_t(3, 0, 10), class = sd),
                                                                     prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                                   ))
(Sys.time() - start)

(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- brm(stand_replacing ~ 
                                                                     het_ndvi_3_s +
                                                                     focal_mean_ndvi_3_s +
                                                                     preFire_ndvi_s +
                                                                     fm100_s +
                                                                     pahl_s + 
                                                                     topo_roughness_3_s + 
                                                                     focal_mean_ndvi_3_s:preFire_ndvi_s +
                                                                     het_ndvi_3_s:focal_mean_ndvi_3_s +
                                                                     het_ndvi_3_s:preFire_ndvi_s +
                                                                     het_ndvi_3_s:fm100_s +
                                                                     (1 | fire_id), 
                                                                   family = bernoulli(), 
                                                                   data = ss_burned, 
                                                                   sample_prior = TRUE, 
                                                                   iter = 4000, 
                                                                   chains = 4, 
                                                                   cores = 4,
                                                                   prior = c(
                                                                     prior(prior = normal(0, 1), class = b),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_3_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = topo_roughness_3_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_3_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:focal_mean_ndvi_3_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:fm100_s),
                                                                     prior(prior = student_t(3, 0, 10), class = Intercept),
                                                                     prior(prior = student_t(3, 0, 10), class = sd),
                                                                     prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                                   ))
(Sys.time() - start)

(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- brm(stand_replacing ~ 
                                                                     het_ndvi_4_s +
                                                                     focal_mean_ndvi_4_s +
                                                                     preFire_ndvi_s +
                                                                     fm100_s +
                                                                     pahl_s + 
                                                                     topo_roughness_4_s + 
                                                                     focal_mean_ndvi_4_s:preFire_ndvi_s +
                                                                     het_ndvi_4_s:focal_mean_ndvi_4_s +
                                                                     het_ndvi_4_s:preFire_ndvi_s +
                                                                     het_ndvi_4_s:fm100_s +
                                                                     (1 | fire_id), 
                                                                   family = bernoulli(), 
                                                                   data = ss_burned, 
                                                                   sample_prior = TRUE, 
                                                                   iter = 4000, 
                                                                   chains = 4, 
                                                                   cores = 4,
                                                                   prior = c(
                                                                     prior(prior = normal(0, 1), class = b),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_4_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = topo_roughness_4_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = focal_mean_ndvi_4_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:focal_mean_ndvi_4_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:preFire_ndvi_s),
                                                                     prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:fm100_s),
                                                                     prior(prior = student_t(3, 0, 10), class = Intercept),
                                                                     prior(prior = student_t(3, 0, 10), class = sd),
                                                                     prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                                   ))                                                     
(Sys.time() - start)

# Save the models as .rds objects

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

# Add information criteria

# Reload the brms fit models into the environment if needed
# fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- readRDS("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds")
# fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- readRDS("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds")
# fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- readRDS("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds")
# fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- readRDS("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds")

# First, the 1-pixel window model
# Start by adding less computationally intensive measures, Bayesian R^2 and WAIC

(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, criterion = c("R2", "waic"))
# Save the updated model with the WAIC and Bayesian R^2 values attached
# saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))


# 2-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, criterion = c("R2", "waic"))
# Save the updated model with the WAIC and Bayesian R^2 values attached
# saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))

# 3-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, criterion = c("R2", "waic"))
# saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))

# 4-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, criterion = c("R2", "waic"))
# saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))
(Sys.time() - start)

compare_ic(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, 
           fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, 
           fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, 
           fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, ic = "waic")

# print method for the WAIC values suggest to try loo instead of WAIC because of p_waic estimates greater than 0.4
# Adding the LOO information criteria, which are much more computationally intensive
# I opt to try to calculate the log-likelihood of the whole matrix (# of observations [~55,000] X # of samples [6000]) at once
# This uses up the whole 64GB of RAM I have at my disposal, but it does work (takes about 1 hour and 20 minutes)
# Trying to use the pointwise = TRUE method to calculate values for each observation independently took >30 hours and didn't finish

# 1-pixel model
# We use reloo = TRUE to refit the model for problematic observations and
# get the pointwise ELPD value for those two observations
# This will refit the model XXX times to compute the ELPDs for the XXX problematic observations directly. 
(start <- Sys.time())
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, criterion = "loo", reloo = TRUE)
(Sys.time() - start)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))

# 2-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, criterion = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))

# 3-pixel model
# It is recommended to set 'reloo = TRUE' in order to calculate the ELPD without the assumption that these observations are negligible. 
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, criterion = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))

# 4-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- 
  add_criterion(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, criterion = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

