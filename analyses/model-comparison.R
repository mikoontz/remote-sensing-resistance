library(tidyverse)
library(here)
library(brms)

m1 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))
m2 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))
m3 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))
m4 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

compare_ic(m1, m2, m3, m4, ic = "loo")
