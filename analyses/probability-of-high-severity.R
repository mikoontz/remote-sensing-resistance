# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

library(ggplot2)
library(brms)
library(tidyr)
library(lubridate)
library(sf)
library(dplyr)
library(here)

if (!file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm.rds"))) {
  source(here::here("analyses/probability-of-high-severity_build-models.R"))
}

load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm.rds"))
load(here::here("analyses/analyses_output/m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm.rds"))

if(!file.exists(here::here("data/data_output/all-fire-samples_texture_configured.rds"))) {
  source(here::here("data/data_carpentry/configure_fire-samples.R"))
}

# This .rds file has the R object name `ss_burned`
load(here::here("data/data_output/burned-fire-samples_texture_configured.rds"))

# What is the mean fm100_s for non-extreme and extreme conditions?
fm100_s_means <- 
  ss_burned %>% 
  as.data.frame() %>% 
  group_by(extreme80_fm100) %>% 
  summarize(mean_fm100 = mean(fm100_s))


get_beta_coef <- function(m, fm100_s_means) {
  # Output will be a distribution for Intercept, heterogeneity in normal conditions, heterogeneity in extreme conditions, fm100 in normal conditions, fm100 in extreme conditions, preFire_ndvi, topo_roughness, and pahl
  samps <- posterior_samples(m)
  prior_samps <- prior_samples(m)
  
  betas <- samps[, 1:11]
  betas_prior <- prior_samps[, 1:11]
  
  het_normal <- c(0, 1, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0, 0, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0)
  
  het_xtreme <- c(0, 1, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , 0, 0, 0, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                  , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100)))
  
  fm100_xtreme <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)
  
  # coefficients
  
  b_intercept <- betas$b_Intercept
  b_het_normal <- apply(betas, 1, function(x) x %*% het_normal)
  b_het_xtreme <- apply(betas, 1, function(x) x %*% het_xtreme)
  b_fm100_normal <- betas$b_fm100_s
  b_fm100_xtreme <- apply(betas, 1, function(x) x %*% fm100_xtreme)
  b_preFire_ndvi <- betas$b_preFire_ndvi_s
  b_topo_roughness <- dplyr::select(betas, starts_with("b_topo_roughness")) %>% pull()
  b_pahl <- betas$b_pahl_s
  
  b_intercept_prior <- betas_prior$b_Intercept
  b_het_normal_prior <- apply(betas_prior, 1, function(x) x %*% het_normal)
  b_het_xtreme_prior <- apply(betas_prior, 1, function(x) x %*% het_xtreme)
  b_fm100_normal_prior <- betas_prior$b_fm100_s
  b_fm100_xtreme_prior <- apply(betas_prior, 1, function(x) x %*% fm100_xtreme)
  b_preFire_ndvi_prior <- betas_prior$b_preFire_ndvi_s
  b_topo_roughness_prior <- dplyr::select(betas_prior, starts_with("b_topo_roughness")) %>% pull()
  b_pahl_prior <- betas_prior$b_pahl_s
  
  data.frame(b_intercept, b_het_normal, b_het_xtreme, b_fm100_normal, b_fm100_xtreme, b_preFire_ndvi, b_topo_roughness, b_pahl, b_intercept_prior, b_het_normal_prior, b_het_xtreme_prior, b_fm100_normal_prior, b_fm100_xtreme_prior, b_preFire_ndvi_prior, b_topo_roughness_prior, b_pahl_prior)
}

betas_1 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm, fm100_s_means)
betas_2 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm, fm100_s_means)
betas_3 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm, fm100_s_means)
betas_4 <- get_beta_coef(m = m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm, fm100_s_means)

