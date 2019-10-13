# predicting probability of high severity fire (using a threshold of RBR)
# based on heterogeneity, 100 hour fuel moisture, whether fuel moisture 
# conditions are extreme, topographic ruggedness, a proxy for canopy density

library(tidyverse)
library(brms)
library(sf)
library(here)

if (!file.exists(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds")) |
    !file.exists(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))) {
  source(here::here("analyses/probability-of-high-severity_build-models.R"))
}

fm1 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))
fm2 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))
fm3 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))
fm4 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

if(!file.exists(here::here("data/data_output/burned-fire-samples_configured.csv"))) {
  source(here::here("data/data_carpentry/configure-fire-samples.R"))
}
# This .rds file has the R object name `ss_burned`
ss_burned <- readr::read_csv(here::here("data/data_output/burned-fire-samples_configured.csv"))

get_beta_coef <- function(m) {
  # Output will be a distribution for Intercept, heterogeneity in normal conditions, heterogeneity in extreme conditions, fm100 in normal conditions, fm100 in extreme conditions, preFire_ndvi, topo_roughness, and pahl
  betas <- posterior_samples(m)
  betas_prior <- prior_samples(m)
  
    # coefficients
  
  b_intercept <- betas$b_Intercept
  b_het <-  dplyr::select(betas, matches("^b_het_ndvi_._s$")) %>% pull()
  b_topo_roughness <- dplyr::select(betas, matches("^b_topo_roughness_._s$")) %>% pull()
  b_neighborhood_mean <- dplyr::select(betas, matches("^b_focal_mean_ndvi_._s$")) %>% pull()
  b_preFire_ndvi <- betas$b_preFire_ndvi_s
  b_fm100 <- betas$b_fm100_s
  b_pahl <- betas$b_pahl_s
  b_neighborhood_mean_ndvi.preFire_ndvi <- dplyr::select(betas, matches("^b_focal_mean_ndvi_._s:preFire_ndvi_s")) %>% pull()
  b_het_ndvi.neighborhood_mean_ndvi <- dplyr::select(betas, matches("^b_het_ndvi_._s:focal_mean_ndvi_._s")) %>% pull()
  b_het_ndvi.preFire_ndvi <- dplyr::select(betas, matches("^b_het_ndvi_._s:preFire_ndvi_s")) %>% pull()
  b_het_ndvi.fm100 <- dplyr::select(betas, matches("^b_het_ndvi_._s:fm100_s")) %>% pull()
  
  b_intercept_prior <- betas_prior$Intercept
  b_het_prior <-  dplyr::select(betas_prior, matches("b_het_ndvi_._s$")) %>% pull()
  b_topo_roughness_prior <- dplyr::select(betas_prior, matches("b_topo_roughness_._s$")) %>% pull()
  b_neighborhood_mean_prior <- dplyr::select(betas_prior, matches("b_focal_mean_ndvi_._s$")) %>% pull()
  b_preFire_ndvi_prior <- betas_prior$b_preFire_ndvi_s
  b_fm100_prior <- betas_prior$b_fm100_s
  b_pahl_prior <- betas_prior$b_pahl_s
  b_neighborhood_mean_ndvi.preFire_ndvi_prior <- dplyr::select(betas_prior, matches("b_focal_mean_ndvi_._s:preFire_ndvi_s")) %>% pull()
  b_het_ndvi.neighborhood_mean_ndvi_prior <- dplyr::select(betas_prior, matches("b_het_ndvi_._s:focal_mean_ndvi_._s")) %>% pull()
  b_het_ndvi.preFire_ndvi_prior <- dplyr::select(betas_prior, matches("b_het_ndvi_._s:preFire_ndvi_s")) %>% pull()
  b_het_ndvi.fm100_prior <- dplyr::select(betas_prior, matches("b_het_ndvi_._s:fm100_s")) %>% pull()
  
  data.frame(b_intercept, b_het, b_topo_roughness, b_neighborhood_mean, b_preFire_ndvi, b_fm100, b_pahl, 
             b_neighborhood_mean_ndvi.preFire_ndvi, b_het_ndvi.neighborhood_mean_ndvi, b_het_ndvi.preFire_ndvi, b_het_ndvi.fm100,
             b_intercept_prior, b_het_prior, b_topo_roughness_prior, b_neighborhood_mean_prior, b_preFire_ndvi_prior, b_fm100_prior, b_pahl_prior,
             b_neighborhood_mean_ndvi.preFire_ndvi_prior, b_het_ndvi.neighborhood_mean_ndvi_prior, b_het_ndvi.preFire_ndvi_prior, b_het_ndvi.fm100_prior)
}

betas_1 <- get_beta_coef(m = fm1)
betas_2 <- get_beta_coef(m = fm2)
betas_3 <- get_beta_coef(m = fm3)
betas_4 <- get_beta_coef(m = fm4)

ci_betas_1 <- as.data.frame(t(apply(betas_1, MARGIN = 2, FUN = function(x) c(lwr = quantile(x, probs = 0.025), mean = mean(x), upr = quantile(x, probs = 0.975)))))
colnames(ci_betas_1) <- c("lwr", "mean", "upr")
ci_betas_1$param <- row.names(ci_betas_1)
ci_betas_1$radius <- 1

ci_betas_2 <- as.data.frame(t(apply(betas_2, MARGIN = 2, FUN = function(x) c(lwr = quantile(x, probs = 0.025), mean = mean(x), upr = quantile(x, probs = 0.975)))))
colnames(ci_betas_2) <- c("lwr", "mean", "upr")
ci_betas_2$param <- row.names(ci_betas_2)
ci_betas_2$radius <- 2

ci_betas_3 <- as.data.frame(t(apply(betas_3, MARGIN = 2, FUN = function(x) c(lwr = quantile(x, probs = 0.025), mean = mean(x), upr = quantile(x, probs = 0.975)))))
colnames(ci_betas_3) <- c("lwr", "mean", "upr")
ci_betas_3$param <- row.names(ci_betas_3)
ci_betas_3$radius <- 3

ci_betas_4 <- as.data.frame(t(apply(betas_4, MARGIN = 2, FUN = function(x) c(lwr = quantile(x, probs = 0.025), mean = mean(x), upr = quantile(x, probs = 0.975)))))
colnames(ci_betas_4) <- c("lwr", "mean", "upr")
ci_betas_4$param <- row.names(ci_betas_4)
ci_betas_4$radius <- 4

ci_betas <- rbind(ci_betas_1, ci_betas_2, ci_betas_3, ci_betas_4)
ci_betas

write_csv(ci_betas, here::here("analyses/analyses_output/ci-betas.csv"))

ci_betas_print_table_simple <-
  ci_betas %>% 
  dplyr::mutate_at(.vars = vars(lwr, mean, upr), .funs = funs(round), 3) %>% 
  dplyr::mutate(print_col = paste0(mean, " (", lwr, ", ", upr, ")")) %>% 
  dplyr::select(param, radius, print_col) %>% 
  dplyr::filter(!str_detect(param, "prior")) %>% 
  tidyr::pivot_wider(names_from = radius, values_from = print_col) %>% 
  dplyr::left_join(param_order, by = "param") %>% 
  dplyr::arrange(order) %>% 
  dplyr::select(print_param, `1`, `2`, `3`, `4`)

write_csv(ci_betas_print_table_simple, here::here("analyses/analyses_output/ci-betas-print-table-simple.csv"))
