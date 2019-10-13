# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

library(tidyverse)
library(here)
library(brms)
library(cowplot)

m1 <- readRDS(here::here("analyses/analyses_output/fm_sevOrNot_het_neighborhoodMean_preFireNDVI_1_ssBurned_brm.rds"))

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

p1 <- fitted(m1, newdata = df_newdata_1, re_formula = NA)

preds_1 <- 
  data.frame(df_newdata_1, p1) %>% 
rename(expectation = Estimate, lwr = Q2.5, upr = Q97.5) %>% 
  mutate(neighborhood_size = 1)

# prefire NDVI ------------------------------------------------------------

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


# fm100 -------------------------------------------------------------------

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

# pahl --------------------------------------------------------------------

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

# heterogeneity of NDVI ---------------------------------------------------

het_ndvi_1_s_effect <-
  preds_1 %>% 
  filter(focal_mean_ndvi_1_s == 0 & preFire_ndvi_s == 0 & fm100_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
  ggplot(aes(x = het_ndvi_1_s, y = expectation)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_line() + 
  theme_bw() +
  xlab(label = "Standard deviation of NDVI\n(90 x 90 m neighborhood; scaled)") +
  ylab(label = NULL)


### Unused

# focal mean NDVI ---------------------------------------------------------

# focal_mean_ndvi_1_s_effect <-
#   preds_1 %>% 
#   filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & pahl_s == 0 & topo_roughness_1_s == 0) %>% 
#   ggplot(aes(x = focal_mean_ndvi_1_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Mean NDVI\n(90 x 90 m neighborhood; scaled)") +
#   ylab(label = NULL)
# 
# focal_mean_ndvi_1_s_effect
# 
# # topographic roughness ---------------------------------------------------
# 
# 
# topo_roughness_1_s_effect <-
#   preds_1 %>% 
#   filter(fm100_s == 0 & het_ndvi_1_s == 0 & preFire_ndvi_s == 0 & focal_mean_ndvi_1_s == 0 & pahl_s == 0) %>% 
#   ggplot(aes(x = topo_roughness_1_s, y = expectation)) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
#   geom_line() + 
#   theme_bw() +
#   xlab(label = "Topographic roughness\n(90 x 90 m neighborhood; scaled)") +
#   ylab(label = NULL)
# 
# topo_roughness_1_s_effect

# publication plot (multi-panel; just neighborhood size of 1) ------------------------------------------

panel_plot <- plot_grid(NULL, preFire_ndvi_1_s_effect, fm100_1_s_effect, NULL, pahl_1_s_effect, het_ndvi_1_s_effect, ncol=3, nrow = 2, rel_widths = c(0.1, 1, 1))

panel_plot_labeled <- 
  panel_plot + 
  draw_label(label = "Probability of high severity fire", x = 0.02, angle = 90, size = 12)

ggsave(panel_plot_labeled, filename = here::here("figures/prob-hi-sev-main-effects-credible-intervals.pdf"), width = 17.3, height = 17.3, units = "cm")
