library(tidyverse)
library(here)

ss_burned <- readRDS(here::here("data/data_output/burned-fire-samples_configured.rds"))

preFire_ndvi.neighborhood_mean_NDVI_correlation <- 
  cor(ss_burned$preFire_ndvi_s, ss_burned$focal_mean_ndvi_1_s, method = "spearman")

