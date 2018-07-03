# Important: because of a significant 3-way interaction between "extreme
# or not fuel moisture", the 100 hour fuel moisture itself, and heterogeneity,
# we split the data and analyzed non-extreme and extreme fuel moisture
# conditions separately.

# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture,
# topographic ruggedness, a proxy for canopy density

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

# Split the data to analyze the extreme fuel moisture data separate from the
# non-extreme fuel moisture data.
# Need to rescale the included covariates: 
# het_ndvi_1, het_ndvi_2, het_ndvi_3, het_ndvi_4
# fm100
# preFire_ndvi
# topo_roughness_1, topo_roughness_2, topo_roughness_3, topo_roughness_4
# pahl

ss_burned_extreme <- 
  ss_burned %>%
  filter(extreme80_fm100 == 1) %>% 
  mutate(het_ndvi_1_s = as.numeric(scale(het_ndvi_1))) %>% 
  mutate(het_ndvi_2_s = as.numeric(scale(het_ndvi_2))) %>% 
  mutate(het_ndvi_3_s = as.numeric(scale(het_ndvi_3))) %>% 
  mutate(het_ndvi_4_s = as.numeric(scale(het_ndvi_4))) %>% 
  mutate(fm100_s = as.numeric(scale(fm100))) %>% 
  mutate(preFire_ndvi_s = as.numeric(scale(preFire_ndvi))) %>% 
  mutate(topo_roughness_1_s = as.numeric(scale(topo_roughness_1))) %>% 
  mutate(topo_roughness_2_s = as.numeric(scale(topo_roughness_2))) %>% 
  mutate(topo_roughness_3_s = as.numeric(scale(topo_roughness_3))) %>% 
  mutate(topo_roughness_4_s = as.numeric(scale(topo_roughness_4))) %>% 
  mutate(pahl_s = as.numeric(scale(pahl)))

ss_burned_nonextreme <-
  ss_burned %>% 
  filter(extreme80_fm100 == 0) %>% 
  mutate(het_ndvi_1_s = as.numeric(scale(het_ndvi_1))) %>% 
  mutate(het_ndvi_2_s = as.numeric(scale(het_ndvi_2))) %>% 
  mutate(het_ndvi_3_s = as.numeric(scale(het_ndvi_3))) %>% 
  mutate(het_ndvi_4_s = as.numeric(scale(het_ndvi_4))) %>% 
  mutate(fm100_s = as.numeric(scale(fm100))) %>% 
  mutate(preFire_ndvi_s = as.numeric(scale(preFire_ndvi))) %>% 
  mutate(topo_roughness_1_s = as.numeric(scale(topo_roughness_1))) %>% 
  mutate(topo_roughness_2_s = as.numeric(scale(topo_roughness_2))) %>% 
  mutate(topo_roughness_3_s = as.numeric(scale(topo_roughness_3))) %>% 
  mutate(topo_roughness_4_s = as.numeric(scale(topo_roughness_4))) %>% 
  mutate(pahl_s = as.numeric(scale(pahl)))

# Severe or not as bernoulli response, extreme or not dummy covariate, interaction with raw fm100, only samples that were burned

m_sevOrNot_extreme_1 <- brm(stand_replacing ~ het_ndvi_1_s * fm100_s + preFire_ndvi_s + topo_roughness_1_s + pahl_s + (1 | fire_id), 
                            family = bernoulli(), 
                            data = ss_burned_extreme, 
                            sample_prior = TRUE, 
                            iter = 3000, 
                            chains = 4,
                            cores = 2,
                            prior = c(
                              prior(prior = normal(0, 1), class = b),
                              prior(prior = normal(0, 1), class = b, coef = fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = pahl_s),
                              prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                              prior(prior = normal(0, 1), class = b, coef = topo_roughness_1_s),
                              prior(prior = student_t(3, 0, 10), class = Intercept),
                              prior(prior = student_t(3, 0, 10), class = sd),
                              prior(prior = normal(0, 1), class = sd, group = fire_id)
                            ))

m_sevOrNot_nonextreme_1 <- brm(stand_replacing ~ het_ndvi_1_s * fm100_s + preFire_ndvi_s + topo_roughness_1_s + pahl_s + (1 | fire_id), 
                            family = bernoulli(), 
                            data = ss_burned_nonextreme, 
                            sample_prior = TRUE, 
                            iter = 3000, 
                            chains = 4,
                            cores = 2,
                            prior = c(
                              prior(prior = normal(0, 1), class = b),
                              prior(prior = normal(0, 1), class = b, coef = fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_1_s:fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = pahl_s),
                              prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                              prior(prior = normal(0, 1), class = b, coef = topo_roughness_1_s),
                              prior(prior = student_t(3, 0, 10), class = Intercept),
                              prior(prior = student_t(3, 0, 10), class = sd),
                              prior(prior = normal(0, 1), class = sd, group = fire_id)
                            ))  

### 2-pixel neighborhood window

m_sevOrNot_extreme_2 <- brm(stand_replacing ~ het_ndvi_2_s * fm100_s + preFire_ndvi_s + topo_roughness_2_s + pahl_s + (1 | fire_id), 
                            family = bernoulli(), 
                            data = ss_burned_extreme, 
                            sample_prior = TRUE, 
                            iter = 3000, 
                            chains = 4,
                            cores = 2,
                            prior = c(
                              prior(prior = normal(0, 1), class = b),
                              prior(prior = normal(0, 1), class = b, coef = fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = pahl_s),
                              prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                              prior(prior = normal(0, 1), class = b, coef = topo_roughness_2_s),
                              prior(prior = student_t(3, 0, 10), class = Intercept),
                              prior(prior = student_t(3, 0, 10), class = sd),
                              prior(prior = normal(0, 1), class = sd, group = fire_id)
                            ))

m_sevOrNot_nonextreme_2 <- brm(stand_replacing ~ het_ndvi_2_s * fm100_s + preFire_ndvi_s + topo_roughness_2_s + pahl_s + (1 | fire_id), 
                               family = bernoulli(), 
                               data = ss_burned_nonextreme, 
                               sample_prior = TRUE, 
                               iter = 3000, 
                               chains = 4,
                               cores = 2,
                               prior = c(
                                 prior(prior = normal(0, 1), class = b),
                                 prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_2_s:fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                 prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                 prior(prior = normal(0, 1), class = b, coef = topo_roughness_2_s),
                                 prior(prior = student_t(3, 0, 10), class = Intercept),
                                 prior(prior = student_t(3, 0, 10), class = sd),
                                 prior(prior = normal(0, 1), class = sd, group = fire_id)
                               ))  


### 3-pixel neighborhood window

m_sevOrNot_extreme_3 <- brm(stand_replacing ~ het_ndvi_3_s * fm100_s + preFire_ndvi_s + topo_roughness_3_s + pahl_s + (1 | fire_id), 
                            family = bernoulli(), 
                            data = ss_burned_extreme, 
                            sample_prior = TRUE, 
                            iter = 3000, 
                            chains = 4,
                            cores = 2,
                            prior = c(
                              prior(prior = normal(0, 1), class = b),
                              prior(prior = normal(0, 1), class = b, coef = fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = pahl_s),
                              prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                              prior(prior = normal(0, 1), class = b, coef = topo_roughness_3_s),
                              prior(prior = student_t(3, 0, 10), class = Intercept),
                              prior(prior = student_t(3, 0, 10), class = sd),
                              prior(prior = normal(0, 1), class = sd, group = fire_id)
                            ))

m_sevOrNot_nonextreme_3 <- brm(stand_replacing ~ het_ndvi_3_s * fm100_s + preFire_ndvi_s + topo_roughness_3_s + pahl_s + (1 | fire_id), 
                               family = bernoulli(), 
                               data = ss_burned_nonextreme, 
                               sample_prior = TRUE, 
                               iter = 3000, 
                               chains = 4,
                               cores = 2,
                               prior = c(
                                 prior(prior = normal(0, 1), class = b),
                                 prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_3_s:fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                 prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                 prior(prior = normal(0, 1), class = b, coef = topo_roughness_3_s),
                                 prior(prior = student_t(3, 0, 10), class = Intercept),
                                 prior(prior = student_t(3, 0, 10), class = sd),
                                 prior(prior = normal(0, 1), class = sd, group = fire_id)
                               ))  

### 4-pixel neighborhood window

m_sevOrNot_extreme_4 <- brm(stand_replacing ~ het_ndvi_4_s * fm100_s + preFire_ndvi_s + topo_roughness_4_s + pahl_s + (1 | fire_id), 
                            family = bernoulli(), 
                            data = ss_burned_extreme, 
                            sample_prior = TRUE, 
                            iter = 3000, 
                            chains = 4,
                            cores = 2,
                            prior = c(
                              prior(prior = normal(0, 1), class = b),
                              prior(prior = normal(0, 1), class = b, coef = fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s),
                              prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:fm100_s),
                              prior(prior = normal(0, 1), class = b, coef = pahl_s),
                              prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                              prior(prior = normal(0, 1), class = b, coef = topo_roughness_4_s),
                              prior(prior = student_t(3, 0, 10), class = Intercept),
                              prior(prior = student_t(3, 0, 10), class = sd),
                              prior(prior = normal(0, 1), class = sd, group = fire_id)
                            ))

m_sevOrNot_nonextreme_4 <- brm(stand_replacing ~ het_ndvi_4_s * fm100_s + preFire_ndvi_s + topo_roughness_4_s + pahl_s + (1 | fire_id), 
                               family = bernoulli(), 
                               data = ss_burned_nonextreme, 
                               sample_prior = TRUE, 
                               iter = 3000, 
                               chains = 4,
                               cores = 2,
                               prior = c(
                                 prior(prior = normal(0, 1), class = b),
                                 prior(prior = normal(0, 1), class = b, coef = fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s),
                                 prior(prior = normal(0, 1), class = b, coef = het_ndvi_4_s:fm100_s),
                                 prior(prior = normal(0, 1), class = b, coef = pahl_s),
                                 prior(prior = normal(0, 1), class = b, coef = preFire_ndvi_s),
                                 prior(prior = normal(0, 1), class = b, coef = topo_roughness_4_s),
                                 prior(prior = student_t(3, 0, 10), class = Intercept),
                                 prior(prior = student_t(3, 0, 10), class = sd),
                                 prior(prior = normal(0, 1), class = sd, group = fire_id)
                               ))  

# Save the models as .rds objects

save(m_sevOrNot_extreme_1, file = here::here("analyses/analyses_output/m_sevOrNot_extreme_1.rds"))
save(m_sevOrNot_nonextreme_1, file = here::here("analyses/analyses_output/m_sevOrNot_nonextreme_1.rds"))

save(m_sevOrNot_extreme_2, file = here::here("analyses/analyses_output/m_sevOrNot_extreme_2.rds"))
save(m_sevOrNot_nonextreme_2, file = here::here("analyses/analyses_output/m_sevOrNot_nonextreme_2.rds"))

save(m_sevOrNot_extreme_3, file = here::here("analyses/analyses_output/m_sevOrNot_extreme_3.rds"))
save(m_sevOrNot_nonextreme_3, file = here::here("analyses/analyses_output/m_sevOrNot_nonextreme_3.rds"))

save(m_sevOrNot_extreme_4, file = here::here("analyses/analyses_output/m_sevOrNot_extreme_4.rds"))
save(m_sevOrNot_nonextreme_4, file = here::here("analyses/analyses_output/m_sevOrNot_nonextreme_4.rds"))


load(here::here("analyses/analyses_output/m_sevOrNot_extreme_1.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extreme_2.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extreme_3.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extreme_4.rds"))

pp_check(m_sevOrNot_extreme_1)
loo(m_sevOrNot_extreme_1, m_sevOrNot_extreme_2)
