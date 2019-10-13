# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

# 4-panel plot depicting geographic extent of study
# Panel A: all the fires included in the analysis
# Panel B: the points designated as "mixed conifer"
# Panel C: the points from the CBI plots used
# Panel D: the samples from all the fires
# merged C and D

# Setup -------------------------------------------------------------------

library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(raster)
library(viridis)
library(USAboundaries)


# Get the fire samples ----------------------------------------------------
# object is called 'ss_burned'

if (!file.exists("data/data_output/burned-fire-samples_configured.geoJSON")) {
  source("data/data_carpentry/11_configure-fire-samples.R")
}

ss_burned <- 
  sf::st_read(here::here("data/data_output/burned-fire-samples_configured.geoJSON"))

# Get conifer forest ------------------------------------------------------

mixed_con <- raster("data/data_output/landcover_PFR/ypmc-mask_10-fold-res.tif")

# Get frap fires --------------------------------------------------
frap <- raster("figures/frap-fire-extent-image.tif")

# mask out the non-conifer areas ------------------------------------------
frap_mixed_con <- 
  frap %>% 
  mask(mixed_con, maskvalue = 0)

# Get the outline of the Sierra Nevada ------------------------------------
sn <- st_read("data/data_output/SierraEcoregion_Jepson/")

# Get California outline --------------------------------------------------

ca_nv_or_az <- raster::getData(name = "GADM",country = "USA",level = 1, path = "data/data_raw") %>%
  st_as_sf() %>%
  filter(NAME_1 %in% c("California", "Nevada", "Oregon", "Arizona")) %>%
  st_transform(st_crs(sn))

conus <- 
  us_boundaries(type = "state", resolution = "low") %>% 
  dplyr::filter(!(name %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>% 
  st_transform(102003)

# Get the CBI plot locations

cbi_48_bicubic <- st_read("data/data_output/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE) %>% 
  mutate(alarm_date = as.POSIXct(alarm_date / 1000, origin = "1970-01-01")) %>% 
  mutate(alarm_date = floor_date(alarm_date, unit = "days")) %>% 
  mutate(year = year(alarm_date))

glimpse(cbi_48_bicubic)
# Filter CBI plot data for just conifer forest ----------------------------

cbi_conifer_only <- subset(cbi_48_bicubic, subset = conifer_forest == 1)
cbi_conifer_only

# Different years that the CBI plots came from
years <- sort(unique(cbi_conifer_only$year))
year_colors <- (viridis(n_distinct(cbi_conifer_only$year)))
# year_colors <- sf.colors(n_distinct(cbi_conifer_only$year))
plot_colors <- year_colors[match(cbi_conifer_only$year, years)]


# tripanel plot -----------------------------------------------------------


pdf("figures/study-geographic-setting.pdf", width = 17.3 / 2.54, height = 11 / 2.54)
par(mfrow = c(1, 3), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xpd = NA)
mar_old <- par()$mar

# Panel A with the Sierra Nevada outline and yellow pine/mixed-conifer raster
plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(mixed_con, col = c("white", "darkgreen"), add = TRUE, legend = FALSE)

text(x = -119, y = 40, labels = "A", cex = 3)

# Panel B with the number of fires in the FRAP database (and their overlapping
# extent)
max_num_of_fires <- 3
frap_mixed_con[frap_mixed_con > max_num_of_fires] <- max_num_of_fires

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(frap_mixed_con, add = TRUE, col = (viridis(max_num_of_fires)), legend = FALSE)
plot(frap_mixed_con, 
     smallplot = c(0.32, 0.335, 0.2, 0.425), 
     legend.only = TRUE, 
     col = (viridis(max_num_of_fires)), 
     legend.args = list(text = "Number\nof fires", 
                        side = 1, 
                        line = 3),
     axis.args = list(line = 0,
                      tcl = 1,
                      mgp = c(0, -2, 0),
                      at = 1:max_num_of_fires,
                      labels = c("1", "2", "3+")))

text(x = -119, y = 40, labels = "B", cex = 3)

# Panel C with the remote samples in black and the CBI plot data in red
plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
ypmc_ss_burned <- 
  ss_burned %>% 
  filter(conifer_forest == 1)

ypmc_ss_burned <- 
  ss_burned %>% 
  filter(conifer_forest == 1)

plot(ypmc_ss_burned$geometry, add = TRUE, pch = 19, cex = 0.1)

# Add the CBI plot locations
plot(cbi_conifer_only$geometry, add = TRUE, pch = 19, cex = 0.2, col = "red")
legend(x = "bottomleft", legend = c("CBI plot", "Remote\nsample"), pch = 19, col = 2:1, bty = "n", inset = c(0.1, 0.15), cex = 1.5, y.intersp = 1.4)

text(x = -119, y = 40, labels = "C", cex = 3)

# Inset in bottom left of figure showing the geographic context of the Sierra
# Nevada
par(fig = c(0, 0.24, 0, 0.27), new = TRUE, mar = rep(0, 4))
plot(conus$geometry, axes = FALSE, outer = TRUE, lwd = 0.5)
plot(st_transform(sn, 102003)$geometry, add = TRUE, col = "lightgrey", outer = TRUE, lwd = 0.5)

dev.off()