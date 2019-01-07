# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

library(tidyverse)
library(here)
library(brms)
library(cowplot)

m1 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))
# m2 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_2_ssBurned_brm.rds"))
# m3 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_3_ssBurned_brm.rds"))
# m4 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_4_ssBurned_brm.rds"))

covariate_newdata <- seq(-2, 2, length.out = 1001)
num_of_preds <- 6

df_newdata_1 <- data.frame(
  het_ndvi_1_s = c(covariate_newdata, 
                   rep(0, (num_of_preds - 1) * length(covariate_newdata))),
  focal_mean_ndvi_1_s = c(rep(0, 1 * length(covariate_newdata)), 
                          covariate_newdata, 
                          rep(0, (num_of_preds - 2) * length(covariate_newdata))),
  preFire_ndvi_s = c(rep(0, 2 * length(covariate_newdata)), 
                     covariate_newdata, 
                     rep(0, (num_of_preds - 3) * length(covariate_newdata))),
  fm100_s = c(rep(0, 3 * length(covariate_newdata)), 
              covariate_newdata, 
              rep(0, (num_of_preds - 4) * length(covariate_newdata))),
  pahl_s = c(rep(0, 4 * length(covariate_newdata)), 
             covariate_newdata, 
             rep(0, (num_of_preds - 5) * length(covariate_newdata))),
  topo_roughness_1_s = c(rep(0, 5 * length(covariate_newdata)), 
                         covariate_newdata))

# df_newdata_2 <-
#   df_newdata_1 %>% 
#   rename(het_ndvi_2_s = het_ndvi_1_s,
#          focal_mean_ndvi_2_s = focal_mean_ndvi_1_s,
#          topo_roughness_2_s = topo_roughness_1_s)
# 
# df_newdata_3 <-
#   df_newdata_1 %>% 
#   rename(het_ndvi_3_s = het_ndvi_1_s,
#          focal_mean_ndvi_3_s = focal_mean_ndvi_1_s,
#          topo_roughness_3_s = topo_roughness_1_s)
# 
# df_newdata_4 <-
#   df_newdata_1 %>% 
#   rename(het_ndvi_4_s = het_ndvi_1_s,
#          focal_mean_ndvi_4_s = focal_mean_ndvi_1_s,
#          topo_roughness_4_s = topo_roughness_1_s)


p1 <- fitted(m1, newdata = df_newdata_1, re_formula = NA)
# p2 <- fitted(m2, newdata = df_newdata_2, re_formula = NA)
# p3 <- fitted(m3, newdata = df_newdata_3, re_formula = NA)
# p4 <- fitted(m4, newdata = df_newdata_4, re_formula = NA)

preds_1 <- 
  data.frame(df_newdata_1, p1) %>% 
  rename(expectation = Estimate, lwr = X2.5.ile, upr = X97.5.ile) %>% 
  mutate(neighborhood_size = 1)

# preds_2 <- 
#   data.frame(df_newdata_2, p2) %>% 
#   rename(expectation = Estimate, lwr = X2.5.ile, upr = X97.5.ile) %>% 
#   mutate(neighborhood_size = 2)
# 
# preds_3 <- 
#   data.frame(df_newdata_3, p3) %>% 
#   rename(expectation = Estimate, lwr = X2.5.ile, upr = X97.5.ile) %>% 
#   mutate(neighborhood_size = 3)
# 
# preds_4 <- 
#   data.frame(df_newdata_4, p4) %>% 
#   rename(expectation = Estimate, lwr = X2.5.ile, upr = X97.5.ile) %>% 
#   mutate(neighborhood_size = 4)
# 
# preds_all <-
#   rbind(rename(preds_1, het_ndvi_s = het_ndvi_1_s, focal_mean_ndvi_s = focal_mean_ndvi_1_s, topo_roughness_s = topo_roughness_1_s),
#         rename(preds_2, het_ndvi_s = het_ndvi_2_s, focal_mean_ndvi_s = focal_mean_ndvi_2_s, topo_roughness_s = topo_roughness_2_s),
#         rename(preds_3, het_ndvi_s = het_ndvi_3_s, focal_mean_ndvi_s = focal_mean_ndvi_3_s, topo_roughness_s = topo_roughness_3_s),
#         rename(preds_4, het_ndvi_s = het_ndvi_4_s, focal_mean_ndvi_s = focal_mean_ndvi_4_s, topo_roughness_s = topo_roughness_4_s))

# Individual plots --------------------------------------------------------

# heterogeneity effect ----------------------------------------------------

# all neighborhoods on same plot ------------------------------------------

# het_ndvi_all_s_effect <-
#   preds_all %>% 
#   filter(focal_mean_ndvi_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_s == 0) %>% 
#   ggplot(aes(x = het_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Heterogeneity") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap(~ neighborhood_size, nrow = 2, ncol = 2)

# different neighborhood sizes --------------------------------------------

het_ndvi_1_s_effect <-
  preds_1 %>% 
  filter(focal_mean_ndvi_1_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = het_ndvi_1_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Standard deviation of NDVI\n(90 x 90 m neighborhood; scaled)") +
  ylab(label = NULL)

# het_ndvi_2_s_effect <-
#   preds_2 %>% 
#   filter(focal_mean_ndvi_2_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_2_s == 0) %>% 
#   ggplot(aes(x = het_ndvi_2_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Heterogeneity") +
#   ylab(label = "Pr(HSF)")
# 
# het_ndvi_2_s_effect
# 
# het_ndvi_3_s_effect <-
#   preds_3 %>% 
#   filter(focal_mean_ndvi_3_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_3_s == 0) %>% 
#   ggplot(aes(x = het_ndvi_3_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Heterogeneity") +
#   ylab(label = "Pr(HSF)")
# 
# het_ndvi_3_s_effect
# 
# het_ndvi_4_s_effect <-
#   preds_4 %>% 
#   filter(focal_mean_ndvi_4_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_4_s == 0) %>% 
#   ggplot(aes(x = het_ndvi_4_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Heterogeneity") +
#   ylab(label = "Pr(HSF)")
# 
# het_ndvi_4_s_effect

# preFire NDVI ------------------------------------------------------------


# all neighborhoods on same plot ------------------------------------------
# preFire_ndvi_all_s_effect <-
#   preds_all %>% 
#   filter(focal_mean_ndvi_s == 0 & het_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_s == 0) %>% 
#   ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Prefire NDVI") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap(~ neighborhood_size, nrow = 2, ncol = 2)

# different neighborhood sizes --------------------------------------------

preFire_ndvi_1_s_effect <-
  preds_1 %>% 
  filter(focal_mean_ndvi_1_s == 0 & het_ndvi_1_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Prefire NDVI\n(scaled)") +
  ylab(label = NULL)

preFire_ndvi_1_s_effect

# preFire_ndvi_2_s_effect <-
#   preds_2 %>% 
#   filter(focal_mean_ndvi_2_s == 0 & het_ndvi_2_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_2_s == 0) %>% 
#   ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Prefire NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# preFire_ndvi_2_s_effect
# 
# preFire_ndvi_3_s_effect <-
#   preds_3 %>% 
#   filter(focal_mean_ndvi_3_s == 0 & het_ndvi_3_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_3_s == 0) %>% 
#   ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Prefire NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# preFire_ndvi_3_s_effect
# 
# preFire_ndvi_4_s_effect <-
#   preds_4 %>% 
#   filter(focal_mean_ndvi_4_s == 0 & het_ndvi_4_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_4_s == 0) %>% 
#   ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Prefire NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# preFire_ndvi_4_s_effect


# fuel moisture effect ----------------------------------------------------


# all neighborhood sizes together -----------------------------------------

# fm100_all_s_effect <-
#   preds_all %>% 
#   filter(focal_mean_ndvi_s == 0 & het_ndvi_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_s == 0) %>% 
#   ggplot(aes(x = fm100_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "100-hour fuel moisture") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap( ~ neighborhood_size, nrow = 2, ncol = 2)

# different neighborhood sizes --------------------------------------------


fm100_1_s_effect <-
  preds_1 %>% 
  filter(focal_mean_ndvi_1_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = fm100_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "100-hour fuel moisture\n(scaled)") +
  ylab(label = NULL)

fm100_1_s_effect

# fm100_2_s_effect <-
#   preds_2 %>% 
#   filter(focal_mean_ndvi_2_s == 0 & het_ndvi_2_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_2_s == 0) %>% 
#   ggplot(aes(x = fm100_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "100-hour fuel moisture") +
#   ylab(label = "Pr(HSF)")
# 
# fm100_2_s_effect
# 
# fm100_3_s_effect <-
#   preds_3 %>% 
#   filter(focal_mean_ndvi_3_s == 0 & het_ndvi_3_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_3_s == 0) %>% 
#   ggplot(aes(x = fm100_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "100-hour fuel moisture") +
#   ylab(label = "Pr(HSF)")
# 
# fm100_3_s_effect
# 
# fm100_4_s_effect <-
#   preds_4 %>% 
#   filter(focal_mean_ndvi_4_s == 0 & het_ndvi_4_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_4_s == 0) %>% 
#   ggplot(aes(x = fm100_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "100-hour fuel moisture") +
#   ylab(label = "Pr(HSF)")
# 
# fm100_4_s_effect

# neighborhood mean NDVI effect -------------------------------------------


# all neighborhoods together ----------------------------------------------

# focal_mean_ndvi_all_s_effect <-
#   preds_all %>% 
#   filter(fm100_s == 0 & het_ndvi_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_s == 0) %>% 
#   ggplot(aes(x = focal_mean_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Neighborhood mean NDVI") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap( ~ neighborhood_size, nrow = 2, ncol = 2)
# 
# focal_mean_ndvi_all_s_effect

# separate neighborhood sizes ---------------------------------------------

focal_mean_ndvi_1_s_effect <-
  preds_1 %>% 
  filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = focal_mean_ndvi_1_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Mean NDVI\n(90 x 90 m neighborhood; scaled)") +
  ylab(label = NULL)

focal_mean_ndvi_1_s_effect


# focal_mean_ndvi_2_s_effect <-
#   preds_2 %>% 
#   filter(fm100_s == 0 & het_ndvi_2_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_2_s == 0) %>% 
#   ggplot(aes(x = focal_mean_ndvi_2_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Neighborhood mean NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# focal_mean_ndvi_2_s_effect
# 
# 
# focal_mean_ndvi_3_s_effect <-
#   preds_3 %>% 
#   filter(fm100_s == 0 & het_ndvi_3_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_3_s == 0) %>% 
#   ggplot(aes(x = focal_mean_ndvi_3_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Neighborhood mean NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# focal_mean_ndvi_3_s_effect
# 
# 
# focal_mean_ndvi_4_s_effect <-
#   preds_4 %>% 
#   filter(fm100_s == 0 & het_ndvi_4_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_4_s == 0) %>% 
#   ggplot(aes(x = focal_mean_ndvi_4_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Neighborhood mean NDVI") +
#   ylab(label = "Pr(HSF)")
# 
# focal_mean_ndvi_4_s_effect

# heat load effect --------------------------------------------------------


# all neighborhood sizes together -----------------------------------------
# pahl_all_s_effect <-
#   preds_all %>% 
#   filter(fm100_s == 0 & het_ndvi_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_s == 0 & topo_roughness_s == 0) %>% 
#   ggplot(aes(x = pahl_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Potential annual heat load") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap( ~ neighborhood_size, nrow = 2, ncol = 2)
# 
# pahl_all_s_effect

# different neighborhood sizes --------------------------------------------

pahl_1_s_effect <-
  preds_1 %>% 
  filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_1_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = pahl_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Potential annual heat load\n(scaled)") +
  ylab(label = NULL)

pahl_1_s_effect

# pahl_2_s_effect <-
#   preds_2 %>% 
#   filter(fm100_s == 0 & het_ndvi_2_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_2_s == 0 & topo_roughness_2_s == 0) %>% 
#   ggplot(aes(x = pahl_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Potential annual heat load") +
#   ylab(label = "Pr(HSF)")
# 
# pahl_2_s_effect
# 
# pahl_3_s_effect <-
#   preds_3 %>% 
#   filter(fm100_s == 0 & het_ndvi_3_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_3_s == 0 & topo_roughness_3_s == 0) %>% 
#   ggplot(aes(x = pahl_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Potential annual heat load") +
#   ylab(label = "Pr(HSF)")
# 
# pahl_3_s_effect
# 
# pahl_4_s_effect <-
#   preds_4 %>% 
#   filter(fm100_s == 0 & het_ndvi_4_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_4_s == 0 & topo_roughness_4_s == 0) %>% 
#   ggplot(aes(x = pahl_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Potential annual heat load") +
#   ylab(label = "Pr(HSF)")
# 
# pahl_4_s_effect

# topographic roughness effect --------------------------------------------

# all neighborhood sizes together -----------------------------------------
# topo_roughness_all_s_effect <-
#   preds_all %>% 
#   filter(fm100_s == 0 & het_ndvi_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_s == 0 & pahl_s == 0) %>% 
#   ggplot(aes(x = topo_roughness_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Topographic roughness") +
#   ylab(label = "Pr(HSF)") +
#   facet_wrap( ~ neighborhood_size, nrow = 2, ncol = 2)

# individual neighborhood sizes -------------------------------------------

topo_roughness_1_s_effect <-
  preds_1 %>% 
  filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_1_s == 0 & pahl_s == 0) %>% 
  ggplot(aes(x = topo_roughness_1_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Topographic roughness\n(90 x 90 m neighborhood; scaled)") +
  ylab(label = NULL)

topo_roughness_1_s_effect

# topo_roughness_2_s_effect <-
#   preds_2 %>% 
#   filter(fm100_s == 0 & het_ndvi_2_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_2_s == 0 & pahl_s == 0) %>% 
#   ggplot(aes(x = topo_roughness_2_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Topographic roughness") +
#   ylab(label = "Pr(HSF)")
# 
# topo_roughness_2_s_effect
# 
# topo_roughness_3_s_effect <-
#   preds_3 %>% 
#   filter(fm100_s == 0 & het_ndvi_3_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_3_s == 0 & pahl_s == 0) %>% 
#   ggplot(aes(x = topo_roughness_3_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Topographic roughness") +
#   ylab(label = "Pr(HSF)")
# 
# topo_roughness_3_s_effect
# 
# topo_roughness_4_s_effect <-
#   preds_4 %>% 
#   filter(fm100_s == 0 & het_ndvi_4_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_4_s == 0 & pahl_s == 0) %>% 
#   ggplot(aes(x = topo_roughness_4_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Topographic roughness") +
#   ylab(label = "Pr(HSF)")
# 
# topo_roughness_4_s_effect


# publication plot (multi-panel; just neighborhood size of 1) ------------------------------------------

panel_plot_a <- plot_grid(NULL, preFire_ndvi_1_s_effect, fm100_1_s_effect, NULL, pahl_1_s_effect, topo_roughness_1_s_effect, ncol=3, nrow = 2, rel_widths = c(0.2, 1, 1))

panel_plot_b <- plot_grid(NULL, preFire_ndvi_1_s_effect, fm100_1_s_effect, NULL, pahl_1_s_effect, het_ndvi_1_s_effect, ncol=3, nrow = 2, rel_widths = c(0.1, 1, 1))

panel_plot_labelled_b <- 
  panel_plot_b + 
  draw_label(label = "Probability of high severity fire", x = 0.02, angle = 90, size = 12)

ggsave(panel_plot_labelled_b, filename = here::here("figures/prob-hi-sev-main-effects-credible-intervals.pdf"), width = 17.3, height = 17.3, units = "cm")

# publication plot (multi-panel; neighborhood size of 1; ------------------
# Consistent y-axis

# het_ndvi_1_s_effect <-
#   preds_1 %>% 
#   filter(focal_mean_ndvi_1_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
#   ggplot(aes(x = het_ndvi_1_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Standard deviation of NDVI\n(90 x 90 m neighborhood; scaled)") +
#   ylab(label = NULL) +
#   scale_y_continuous(limits = c(0, 0.5))
# 
# preFire_ndvi_1_s_effect <-
#   preds_1 %>%
#   filter(focal_mean_ndvi_1_s == 0 & het_ndvi_1_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>%
#   ggplot(aes(x = preFire_ndvi_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() +
#   theme_bw() +
#   xlab(label = "Prefire NDVI\n(scaled)") +
#   ylab(label = NULL) +
#   scale_y_continuous(limits = c(0, 0.5))
# 
# fm100_1_s_effect <-
#   preds_1 %>% 
#   filter(focal_mean_ndvi_1_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
#   ggplot(aes(x = fm100_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "100-hour fuel moisture\n(scaled)") +
#   ylab(label = NULL) +
#   scale_y_continuous(limits = c(0, 0.5))
# 
# pahl_1_s_effect <-
#   preds_1 %>% 
#   filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_1_s == 0 & topo_roughness_1_s == 0) %>% 
#   ggplot(aes(x = pahl_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Potential annual heat load\n(scaled)") +
#   ylab(label = NULL) +
#   scale_y_continuous(limits = c(0, 0.5))
# 
# panel_plot_b <- plot_grid(NULL, preFire_ndvi_1_s_effect, fm100_1_s_effect, NULL, pahl_1_s_effect, het_ndvi_1_s_effect, ncol=3, nrow = 2, rel_widths = c(0.1, 1, 1))
# 
# panel_plot_labelled_b <- 
#   panel_plot_b + 
#   draw_label(label = "Probability of high severity fire", x = 0.02, angle = 90, size = 12)
# 
# panel_plot_labelled_b
