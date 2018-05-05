# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

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
m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)


# two_pixel_neighbornood
# Posterior distributions of estimates
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

### Prior distributions
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

# three_pixel_neighbornood
# Posterior distributions
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

# Prior distributions
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)

# four_pixel_neighborhood
# Posterior distributions
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_samps <- posterior_samples(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas <- m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_samps[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_betas, 1, function(x) x %*% het_xtreme)

# Prior distributions
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior <- prior_samples(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm)
m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas <- m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior[, 1:11]

het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas, 1, function(x) x %*% het_normal)
het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior <- apply(m_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_brm_prior_betas, 1, function(x) x %*% het_xtreme)


# Probability density plots of the effect of heterogeneity under normal and extreme 100-hour fuel moisture conditions
par(mfrow = c(1, 4))
# 1 pixel neighborhood
het_norm_1_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned)
het_xtrm_1_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned)
het_norm_1_prior_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior)
het_xtrm_1_prior_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned_prior)

het_1_dens_xlim <- range(c(het_norm_1_dens$x, het_xtrm_1_dens$x))
plot(het_norm_1_dens, xlim = het_1_dens_xlim, col = "blue", xlab = "Effect size of heterogeneity of neighborhood NDVI on fire severity", ylab = "Probability density", las = 1, main = NA)
abline(v = mean(het_normal_sevOrNot_extremeFm100AndRawFm100_1_ssBurned), col = "blue", lwd = 2)
lines(het_xtrm_1_dens, col = "red")
abline(v = mean(het_xtreme_sevOrNot_extremeFm100AndRawFm100_1_ssBurned), col = "red", lwd = 2)

# Plot priors
lines(het_norm_1_prior_dens, lty = 2, col = "blue")
lines(het_xtrm_1_prior_dens, lty = 2, col = "red")

abline(v = 0, lwd = 2, lty = 1)
legend("topright", legend = c("Normal fuel moisture", "Extreme fuel moisture"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 3)

# 2 pixel neighborhood
het_norm_2_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned)
het_xtrm_2_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned)
het_norm_2_prior_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior)
het_xtrm_2_prior_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned_prior)

het_2_dens_xlim <- range(c(het_norm_2_dens$x, het_xtrm_2_dens$x))
plot(het_norm_2_dens, xlim = het_2_dens_xlim, col = "blue", xlab = "Effect size of heterogeneity of neighborhood NDVI on fire severity", ylab = "Probability density", las = 1, main = NA)
abline(v = mean(het_normal_sevOrNot_extremeFm100AndRawFm100_2_ssBurned), col = "blue", lwd = 2)
lines(het_xtrm_2_dens, col = "red")
abline(v = mean(het_xtreme_sevOrNot_extremeFm100AndRawFm100_2_ssBurned), col = "red", lwd = 2)

# Plot priors
lines(het_norm_2_prior_dens)
lines(het_xtrm_2_prior_dens)

abline(v = 0, lwd = 2)
legend("topright", legend = c("Normal fuel moisture", "Extreme fuel moisture"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 3)

# 3 pixel neighborhood
het_norm_3_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned)
het_xtrm_3_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned)
het_norm_3_prior_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior)
het_xtrm_3_prior_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned_prior)

het_3_dens_xlim <- range(c(het_norm_3_dens$x, het_xtrm_3_dens$x))
plot(het_norm_3_dens, xlim = het_3_dens_xlim, col = "blue", xlab = "Effect size of heterogeneity of neighborhood NDVI on fire severity", ylab = "Probability density", las = 1, main = NA)
abline(v = mean(het_normal_sevOrNot_extremeFm100AndRawFm100_3_ssBurned), col = "blue", lwd = 2)
lines(het_xtrm_3_dens, col = "red")
abline(v = mean(het_xtreme_sevOrNot_extremeFm100AndRawFm100_3_ssBurned), col = "red", lwd = 2)

# Plot priors
lines(het_norm_3_prior_dens)
lines(het_xtrm_3_prior_dens)

abline(v = 0, lwd = 2)
legend("topright", legend = c("Normal fuel moisture", "Extreme fuel moisture"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 3)

# 4 pixel neighborhood
het_norm_4_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned)
het_xtrm_4_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned)
het_norm_4_prior_dens <- density(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior)
het_xtrm_4_prior_dens <- density(het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned_prior)

het_4_dens_xlim <- range(c(het_norm_4_dens$x, het_xtrm_4_dens$x))
plot(het_norm_4_dens, xlim = het_4_dens_xlim, col = "blue", xlab = "Effect size of heterogeneity of neighborhood NDVI on fire severity", ylab = "Probability density", las = 1, main = NA)
abline(v = mean(het_normal_sevOrNot_extremeFm100AndRawFm100_4_ssBurned), col = "blue", lwd = 2)
lines(het_xtrm_4_dens, col = "red")
abline(v = mean(het_xtreme_sevOrNot_extremeFm100AndRawFm100_4_ssBurned), col = "red", lwd = 2)

# Plot priors
lines(het_norm_4_prior_dens)
lines(het_xtrm_4_prior_dens)

abline(v = 0, lwd = 2)
legend("topright", legend = c("Normal fuel moisture", "Extreme fuel moisture"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 3)

