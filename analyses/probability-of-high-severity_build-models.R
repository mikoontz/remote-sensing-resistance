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


if(!file.exists(here::here("data/data_output/all-fire-samples_texture_configured.rds"))) {
  source(here::here("data/data_carpentry/configure_fire-samples.R"))
}

# This .rds file has the R object name `ss_burned`
load(here::here("data/data_output/burned-fire-samples_texture_configured.rds"))
# This .rds file has the R object name `ss`
load(here::here("data/data_output/all-fire-samples_texture_configured.rds"))

# Severe or not as bernoulli response, extreme or not dummy covariate, interaction with raw fm100, only samples that were burned

m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_1_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_1_s + pahl_s + (1 | fire_id), 
                                                         family = bernoulli(), 
                                                         data = ss_burned, 
                                                         sample_prior = TRUE, 
                                                         iter = 3000, 
                                                         chains = 4, 
                                                         prior = c(
                                                           prior(prior = normal(0, 1), class = b),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                           prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                           prior(prior = normal(0, 1), class = b, coef = topo_roughness_1_s),
                                                           prior(prior = student_t(3, 0, 10), class = Intercept),
                                                           prior(prior = student_t(3, 0, 10), class = sd),
                                                           prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                         ))

m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_2_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_2_s + pahl_s + (1 | fire_id), 
                                                         family = bernoulli(), 
                                                         data = ss_burned, 
                                                         sample_prior = TRUE, 
                                                         iter = 3000, 
                                                         chains = 4, 
                                                         prior = c(
                                                           prior(prior = normal(0, 1), class = b),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                           prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                           prior(prior = normal(0, 1), class = b, coef = topo_roughness_2_s),
                                                           prior(prior = student_t(3, 0, 10), class = Intercept),
                                                           prior(prior = student_t(3, 0, 10), class = sd),
                                                           prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                         ))

m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_3_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_3_s + pahl_s + (1 | fire_id), 
                                                         family = bernoulli(), 
                                                         data = ss_burned, 
                                                         sample_prior = TRUE, 
                                                         iter = 3000, 
                                                         chains = 4, 
                                                         prior = c(
                                                           prior(prior = normal(0, 1), class = b),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                           prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                           prior(prior = normal(0, 1), class = b, coef = topo_roughness_3_s),
                                                           prior(prior = student_t(3, 0, 10), class = Intercept),
                                                           prior(prior = student_t(3, 0, 10), class = sd),
                                                           prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                         ))

m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_4_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_4_s + pahl_s + (1 | fire_id), 
                                                         family = bernoulli(), 
                                                         data = ss_burned, 
                                                         sample_prior = TRUE, 
                                                         iter = 3000, 
                                                         chains = 4, 
                                                         prior = c(
                                                           prior(prior = normal(0, 1), class = b),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:extreme80_fm100),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:extreme80_fm100:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:fm100_s),
                                                           prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                                           prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                                           prior(prior = normal(0, 1), class = b, coef = topo_roughness_4_s),
                                                           prior(prior = student_t(3, 0, 10), class = Intercept),
                                                           prior(prior = student_t(3, 0, 10), class = sd),
                                                           prior(prior = normal(0, 1), class = sd, group = fire_id)
                                                         ))                                                     

# m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_2_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_2_s + pahl_s + (1 | fire_id), family = bernoulli(), data = ss_burned, sample_prior = TRUE, iter = 3000)
# 
# m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_3_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_3_s + pahl_s + (1 | fire_id), family = bernoulli(), data = ss_burned, sample_prior = TRUE, iter = 3000)
# 
# m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm <- brm(stand_replacing ~ het_ndvi_4_s * extreme80_fm100 * fm100_s + preFire_ndvi_s + topo_roughness_4_s + pahl_s + (1 | fire_id), family = bernoulli(), data = ss_burned, sample_prior = TRUE, iter = 3000)

# Save the models as .rds objects

save(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm, file = here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm.rds"))
save(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm, file = here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm.rds"))
save(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm, file = here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm.rds"))
save(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm, file = here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm.rds"))

