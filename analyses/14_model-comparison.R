# Purpose: Perform model comparison between the four models representing
# heterogeneity at different scales

library(tidyverse)
library(here)
library(brms)

if(!file.exists(here::here("data/data_output/ic-print-table.csv"))) {
  
  fm1 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))
  fm2 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))
  fm3 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))
  fm4 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))
  
  ic_table <-
    loo_compare(fm1, fm2, fm3, fm4, criterion = "loo") %>% 
    as.data.frame() %>% 
    mutate(model_comparison = str_extract_all(rownames(.), pattern = "[0-9]") %>% 
             map(.f = function(x) paste0("fm1 - fm", x[1])) %>% 
             do.call("rbind", .) %>% 
             c())
  
  neighborhood_size <- c("90 x 90m", "150 x 150m", "210 x 210m", "270 x 270m")
  
  loo_estimates <-
    c(fm1$loo$estimates["looic", "Estimate"],
      fm2$loo$estimates["looic", "Estimate"],
      fm3$loo$estimates["looic", "Estimate"],
      fm4$loo$estimates["looic", "Estimate"])
  
  loo_delta <- 
    c(fm1$loo$estimates["looic", "Estimate"],
      fm2$loo$estimates["looic", "Estimate"],
      fm3$loo$estimates["looic", "Estimate"],
      fm4$loo$estimates["looic", "Estimate"]) -
    fm1$loo$estimates["looic", "Estimate"]
  
  R2_estimates <-
    c(mean(fm1$R2),
      mean(fm2$R2),
      mean(fm3$R2),
      mean(fm4$R2))
  
  loo_model_weights <- model_weights(fm1, fm2, fm3, fm4, weights = "loo")
  ic_print_table <- data.frame(model = 1:4, 
                               neighborhood_size = neighborhood_size, 
                               loo = round(ic_table$elpd_loo, 3), 
                               delta = round(-1 * ic_table$elpd_diff, 3), 
                               se = c("", as.character(round(ic_table$se_diff[2:4], 3))), 
                               loo_model_weights_pct = round(100 * loo_model_weights, 2),
                               R2 = round(R2_estimates, 3))
  
  write_csv(ic_print_table, here::here("data/data_output/ic-print-table.csv"))
  
}