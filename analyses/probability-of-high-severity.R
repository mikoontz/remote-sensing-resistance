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

het_normal <- c(0, 1, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0, 0, 0, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 0), mean_fm100)), 0, 0)

het_xtreme <- c(0, 1, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                , 0, 0, 0, 1, pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
                , pull(dplyr::select(filter(fm100_s_means, extreme80_fm100 == 1), mean_fm100))
)


# one_pixel_neighbornood
### Posterior distributions
# Extract all samples and then just select the ones for the beta coefficients
m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_samps[, 1:11]

# Matrix multiply the samples by the desired combination of Betas to assess effect of heterogeneity in each fuel moisture condition
het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

### Prior distributions
# Get samples from the priors for plotting purposes
m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior[, -c(1, 3)]

het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned)(0)

hist(het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned, breaks = 100)
ecdf(het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned)(0)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned)(0)

# two_pixel_neighbornood
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

### Prior distributions
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned)(0)

hist(het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned, breaks = 100)
ecdf(het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned)(0)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned)(0)


# three_pixel_neighbornood
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned)(0)

hist(het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned, breaks = 100)
ecdf(het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned)(0)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned)(0)


# four_pixel_neighborhood
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned)(0)

hist(het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned, breaks = 100)
ecdf(het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned)(0)

hist(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned, breaks = 100)
ecdf(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned - het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned)(0)
