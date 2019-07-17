# Generate figures of real fires to demonstrate variables

library(raster)
library(here)
library(dplyr)
library(viridis)

model_summary <- read.csv(here::here("data/data_output/cbi_calibration_model_comparison.csv"), stringsAsFactors = FALSE)
target_model <- model_summary[model_summary$response == "RBR" &
                                model_summary$time_window == 48 &
                                model_summary$interpolation == "bicubic", ]

target_model$unchanged # equates to a CBI of 0
target_model$low_sev # equates to a CBI of 0.1 -- threshold between "unchanged" and "low"
target_model$mod_sev # equates to a CBI of 1.25 -- threshold between "low" and "medium"
target_model$hi_sev # equates to a CBI of 2.25 -- threshold between "medium" and "high"

hamm <- brick(here::here("data/ee_fire-images/hamm-fire_1987_visualize_med-res.tif"))
american <- brick(here::here("data/ee_fire-images/american-fire_2013_visualize_med-res.tif"))

bandNames <- c('RdNBR',
               'preFire_nbr',
               'postFire_nbr',
               'RdNDVI',
               'RBR',
               'preFire_ndvi',
               'postFire_ndvi',
               'het_ndvi_1',
               'focal_mean_ndvi_1',
               'het_ndvi_2',
               'focal_mean_ndvi_2',
               'het_ndvi_3',
               'focal_mean_ndvi_3',
               'het_ndvi_4',
               'focal_mean_ndvi_4',
               'date', 
               'ordinal_day',
               'alarm_year',
               'alarm_month',
               'alarm_day',
               'lon', 
               'lat', 
               'conifer_forest',
               'slope', 
               'aspect',
               'topo_roughness_1',
               'topo_roughness_2',
               'topo_roughness_3',
               'topo_roughness_4',
               'elev',
               "B1_pre",
               "B2_pre",
               "B3_pre",
               "B4_pre",
               "B5_pre",
               "B6_pre",
               "B7_pre",
               "B1_post",
               "B2_post",
               "B3_post",
               "B4_post",
               "B5_post",
               "B6_post",
               "B7_post",
               "erc",
               "fm100",
               "vpd",
               "vs",
               "hdw")

names(hamm) <- bandNames
names(american) <- bandNames

# Green to red palette
RBR_cols <- c('#008000', '#ffff00', '#ffA500', '#ff0000') 
# Colorblind-friendly palette
RBR_cols <- viridis(4)
RBR_cols <- inferno(4)
?viridis

plot_pre_post_rbr <- function(r, rgb_bands = c("B3", "B2", "B1")) {
  rgb_bands_pre <- paste0(rgb_bands, "_pre")
  rgb_bands_post <- paste0(rgb_bands, "_post")
  
  max_val_pre <- max(subset(r, rgb_bands_pre)[], na.rm = TRUE)
  plotRGB(x = subset(r, rgb_bands_pre), scale = max_val_pre)
  
  max_val_post <- max(subset(r, rgb_bands_post)[], na.rm = TRUE)
  plotRGB(x = subset(r, rgb_bands_post), scale = max_val_post)
  
  RBR_breaks <- c(min(r$RBR[], na.rm = TRUE),
                  target_model$low_sev, 
                  target_model$mod_sev, 
                  target_model$hi_sev,
                  max(r$RBR[], na.rm = TRUE))
  
  plot(x = r, y = "RBR", box = FALSE, legend = FALSE, col = RBR_cols, breaks = RBR_breaks, main = NA, axes = FALSE)
}

pdf("figures/pre-post-rbr.pdf", width = 17.3 / 2.54, height = 17.3 / 2.54 * (2/3))
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0))
plot_pre_post_rbr(hamm)
plot_pre_post_rbr(american)

legend("topleft", xpd = NA, legend = c("Unburned", "Low severity", "Moderate severity", "High severity"), 
       col = RBR_cols, pch = 15, bty = "n", cex = 1, inset = c(0, -0.2))
dev.off()

# var natVizParamsPre = {
#   bands: ['B3_pre', 'B2_pre', 'B1_pre'],
#   min: 0,
#   max: 2000,
#   gamma: [0.9, 1.1, 1]
# };
# 
# var natVizParamsPost = {
#   bands: ['B3_post', 'B2_post', 'B1_post'],
#   min: 0,
#   max: 2000,
#   gamma: [0.9, 1.1, 1]
# };
# 
# // Define the visualization parameters.
# var falseVizParamsPre = {
#   bands: ['B4_pre', 'B3_pre', 'B2_pre'],
#   min: 0,
#   max: 10000,
#   gamma: [1, 1, 1]
# };
# 
# var falseVizParamsPost = {
#   bands: ['B4_post', 'B3_post', 'B2_post'],
#   min: 0,
#   max: 10000,
#   gamma: [1, 1, 1]
# };
# 
# var fireVizParamsPre = {
#   bands: ['B7_pre', 'B4_pre', 'B2_pre'],
#   min: 0,
#   max: 5000,
#   gamma: [0.9, 0.8, 1]
# };
# 
# var fireVizParamsPost = {
#   bands: ['B7_post', 'B4_post', 'B2_post'],
#   min: 0,
#   max: 5000,
#   gamma: [0.9, 0.8, 1]
# };

