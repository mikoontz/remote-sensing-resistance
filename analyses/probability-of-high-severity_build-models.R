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


if(!file.exists(here::here("data/data_output/all-fire-samples_configured.rds"))) {
  source(here::here("data/data_carpentry/configure_fire-samples.R"))
}

# This .rds file has the R object name `ss_burned`
ss_burned <- readRDS(here::here("data/data_output/burned-fire-samples_configured.rds"))
# This .rds file has the R object name `ss`
ss <- readRDS(here::here("data/data_output/all-fire-samples_configured.rds"))

# Severe or not as bernoulli response, heterogeneity, preFire NDVI, prefire neighborhood mean NDVI, heterogeneity interacts with fm100, preFire NDVI, and neighborhood mean NDVI, only samples that were burned

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
                                                                   iter = 3000, 
                                                                   chains = 4, 
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
                                                                   iter = 3000, 
                                                                   chains = 4, 
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
                                                                   iter = 3000, 
                                                                   chains = 4, 
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
                                                                   iter = 3000, 
                                                                   chains = 4, 
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
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, ic = c("R2", "waic"))
# Save the updated model with the WAIC and Bayesian R^2 values attached
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))

# 2-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, ic = c("R2", "waic"))
# Save the updated model with the WAIC and Bayesian R^2 values attached
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))

# 3-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, ic = c("R2", "waic"))
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))

# 4-pixel window model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, ic = c("R2", "waic"))
saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

# print method for the WAIC values suggest to try loo instead of WAIC because of p_waic estimates greater than 0.4
# Adding the LOO information criteria, which are much more computationally intensive
# I opt to try to calculate the log-likelihood of the whole matrix (# of observations [~55,000] X # of samples [6000]) at once
# This uses up the whole 64GB of RAM I have at my disposal, but it does work (takes about 1 hour and 20 minutes)
# Trying to use the pointwise = TRUE method to calculate values for each observation independently took >30 hours and didn't finish

# 1-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, ic = "loo")

# Warning message about 2 observations with high Pareto K diagnostic value (> 0.7)
# so we use reloo = TRUE to refit the model 2 times (once for each bad observation) and
# get the pointwise ELPD value for those two observations
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, ic = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))

# 2-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, ic = "loo")
# Warning message about 3 observations with high Pareto K diagnostic value (> 0.7)
# so we use reloo = TRUE to refit the model 3 times (once for each bad observation) and
# get the pointwise ELPD value for those 3 observations
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, ic = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))

# 3-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, ic = "loo")
# Warning message about 5 observations with high Pareto K diagnostic value (> 0.7)
# so we use reloo = TRUE to refit the model 5 times (once for each bad observation) and
# get the pointwise ELPD value for those 5 observations
# Warning message:
# Found 5 observations with a pareto_k > 0.7 in model 'structure(list(formula = structure(list(formula = stand_replacing ~  het_ndvi_3_s + focal_mean_nd'. 
# It is recommended to set 'reloo = TRUE' in order to calculate the ELPD without the assumption that these observations are negligible. 
# This will refit the model 5 times to compute the ELPDs for the problematic observations directly. 
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, ic = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))

# 4-pixel model
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, ic = "loo")
# Warning message:
# Found 5 observations with a pareto_k > 0.7 in model 'structure(list(formula = structure(list(formula = stand_replacing ~ het_ndvi_4_s + focal_mean_nd'. 
# It is recommended to set 'reloo = TRUE' in order to calculate the ELPD without the assumption that these observations are negligible. 
# This will refit the model 5 times to compute the ELPDs for the problematic observations directly. 
fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm <- 
  add_ic(x = fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, ic = "loo", reloo = TRUE)

saveRDS(fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm, file = here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

