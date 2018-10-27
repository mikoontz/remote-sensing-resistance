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



# Get the fire samples ----------------------------------------------------
# object is called 'ss_burned'

if (!file.exists("data/data_output/burned-fire-samples_configured.rds")) {
  source("data/data_carpentry/configure_fire-samples.R")
}

ss_burned <- readRDS(here::here("data/data_output/burned-fire-samples_configured.rds"))

# Get conifer forest ------------------------------------------------------

mixed_con <- raster("data/data_output/landcover_PFR/mixed_conifer_sn-mask_10-fold-res.tif")
# plot(mixed_con) 

# Get frap fires --------------------------------------------------
frap <- raster("data/data_output/frap-fire-extent-image.tif")

# mask out the non-conifer areas ------------------------------------------
frap_mixed_con <- 
  frap %>% 
  mask(mixed_con, maskvalue = 0)

# Get the outline of the Sierra Nevada ------------------------------------
sn <- st_read("data/data_output/SierraEcoregion_Jepson/") %>% st_transform(proj4string(frap))

# Get California outline --------------------------------------------------

ca <- raster::getData(name = "GADM",country = "USA",level = 1, path = "data/features") %>% 
  st_as_sf() %>% 
  filter(NAME_1 == "California") %>% 
  st_transform(st_crs(sn))

# A lighter-weight California outline (not a multipolygon, not as detailed)
usa <-
  ggplot2::map_data("state") %>%
  dplyr::select(-order, -subregion) %>%
  dplyr::group_by(region, group) %>%
  tidyr::nest() %>%
  dplyr::mutate(geometry = purrr::map(.x = data, .f = function(x) st_polygon(list(with(x, cbind(long, lat)))))) %>%
  dplyr::select(-data) %>%
  dplyr::mutate(geometry = st_sfc(geometry)) %>%
  st_sf(crs = 4326)

ca2 <- usa %>%
  filter(region == "california")

# Get the CBI plot locations

cbi_48_bicubic <- st_read("data/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE) %>% 
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

pdf("figures/study-geographic-setting-2-x-2-panel.pdf", width = 17.3 / 2.54, height = 17.3 / 2.54)
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(0, 3, 0, 0), xpd = NA)
mar_old <- par()$mar

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(frap_mixed_con, add = TRUE, col = viridis(6), legend = FALSE)
plot(frap_mixed_con, 
     smallplot = c(0.085, 0.1, 0.45, 0.85), 
     legend.only = TRUE, 
     col = viridis(6), 
     legend.args = list(text = "Number\nof fires", 
                        side = 1, 
                        line = 3),
     axis.args = list(line = 0,
                      tcl = 1,
                      mgp = c(0, -2, 0)))

text(x = -119, y = 40, labels = "A", cex = 3)

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(mixed_con, col = c("white", "darkgreen"), add = TRUE, legend = FALSE)

text(x = -119, y = 40, labels = "B", cex = 3)

# Add the CBI plot locations
plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(cbi_conifer_only$geometry, add = TRUE, pch = 19, cex = 0.75, col = plot_colors)

legend("topleft", legend = years, pch = 19, col = year_colors, bty = "n", title = "Year of fire")

text(x = -119, y = 40, labels = "C", cex = 3)


plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
used_ss_burned <- ss_burned %>% 
  filter(conifer_forest == 1)
plot(ss_burned$geometry, add = TRUE, pch = 19, cex = 0.1)

text(x = -119, y = 40, labels = "D", cex = 3)

# par(fig = c((0.5 - 0.15/2), (0.5 + 0.15/2), (0.5 - 0.35/2), (0.5 + 0.35/2)), new = TRUE, mar = rep(0, 4))
par(fig = c(0, 0.15, 0, 0.35), new = TRUE, mar = rep(0, 4))

plot(ca$geometry, axes = FALSE, outer = TRUE)
plot(sn$geometry, add = TRUE, col = "lightgrey", outer = TRUE)

dev.off()


# Try a 1x4 arrangement?

pdf("figures/study-geographic-setting-1-x-4-panel.pdf", width = 17.3 / 2.54, height = 6.5 / 2.54)
par(mfrow = c(1, 5), mar = c(0, 0.5, 0, 0), oma = c(0, 0, 0, 0))
plot.new()
# plot(ca$geometry, axes = FALSE)
# plot(sn$geometry, add = TRUE, col = "lightgrey")

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(frap_mixed_con, add = TRUE, col = viridis(6), legend = FALSE)

text(x = -119, y = 40, labels = "A", cex = 3)

plot(frap_mixed_con, 
     smallplot = c(0.005, 0.025, 0.45, 0.75), 
     legend.only = TRUE, 
     col = viridis(6), 
     legend.args = list(text = "Number\nof fires",
                        cex = 1,
                        side = 1, 
                        line = 2.5),
     axis.args = list(line = 0,
                      tcl = 1,
                      mgp = c(0, -2, 0)))


plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(mixed_con, col = c("white", "darkgreen"), add = TRUE, legend = FALSE)

text(x = -119, y = 40, labels = "B", cex = 3)

# Add the CBI plot locations
plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
plot(cbi_conifer_only$geometry, add = TRUE, pch = ".", col = plot_colors)

legend(x = "bottomleft", inset = c(0, 0.05), legend = years, pch = 19, col = year_colors, bty = "n", title = "Year of fire", cex = 1.15)

text(x = -119, y = 40, labels = "C", cex = 3)

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
used_ss_burned <- ss_burned %>% 
  filter(conifer_forest == 1)
plot(ss_burned$geometry, add = TRUE, pch = 19, cex = 0.1)

text(x = -119, y = 40, labels = "D", cex = 3)

par(fig = c(0, 0.25, 0, 0.7), new = TRUE, mar = c(0, 1, 0, 3))
# plot(sn$geometry, col = "lightgrey")
plot(ca2$geometry, axes = FALSE)
plot(sn$geometry, add = TRUE, col = "lightgrey")

mtext(side = 3, text = "Sierra Nevada\nCalifornia\nUSA", line = 0, adj = 0, cex = 1)

dev.off()