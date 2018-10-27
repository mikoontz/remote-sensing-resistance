# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

# This script generates a figure that depicts the extent of the CBI plots used for validating the remote sensed
# severity
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(raster)
library(viridis)

# Get the outline of California -------------------------------------------

ca <- raster::getData(name = "GADM",country = "USA",level = 1, path = "data/features") %>% 
  st_as_sf() %>% 
  filter(NAME_1 == "California")

# Less detailed CA outline in case first version makes things too slow ---------------------------------------

# A lighter-weight California outline (not a multipolygon, not as detailed)
# usa <-
#   ggplot2::map_data("state") %>% 
#   dplyr::select(-order, -subregion) %>% 
#   dplyr::group_by(region, group) %>% 
#   tidyr::nest() %>% 
#   dplyr::mutate(geometry = purrr::map(.x = data, .f = function(x) st_polygon(list(with(x, cbind(long, lat)))))) %>% 
#   dplyr::select(-data) %>% 
#   dplyr::mutate(geometry = st_sfc(geometry)) %>% 
#   st_sf(crs = 4326)
# 
# ca2 <- usa %>% 
#   filter(region == "california")




# Get the outline of the Sierra Nevada ------------------------------------
sn <- st_read("data/data_output/SierraEcoregion_Jepson/") %>% st_transform(4326)

# Get 48-day window, bicubic interpolation data ---------------------------
cbi_48_bicubic <- st_read("data/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE) %>% 
  mutate(alarm_date = as.POSIXct(alarm_date / 1000, origin = "1970-01-01")) %>% 
  mutate(alarm_date = floor_date(alarm_date, unit = "days")) %>% 
  mutate(year = year(alarm_date))

glimpse(cbi_48_bicubic)
# Filter CBI plot data for just conifer forest ----------------------------

conifer_only <- subset(cbi_48_bicubic, subset = conifer_forest == 1)
conifer_only

# Build plot --------------------------------------------------------------

# plot(ca2$geometry, border = "red", add = TRUE)
years <- sort(unique(conifer_only$year))
year_colors <- viridis(n_distinct(conifer_only$year))
# year_colors <- sf.colors(n_distinct(conifer_only$year))
plot_colors <- year_colors[match(conifer_only$year, years)]

### Version 1
# pdf("figures/cbi-extent.pdf", width = 8.2 / 2.54, height = 10.2 /2.54)
# 
# plot(ca$geometry, axes = TRUE, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0))
# plot(sn$geometry, add = TRUE)
# plot(conifer_only$geometry, add = TRUE, pch = 19, col = plot_colors, cex = 0.25)
# legend("topright", legend = years, pch = 19, title = "Years", col = year_colors, bty = "n", cex = 0.5)
# 
# dev.off()
###

pdf("figures/cbi-extent.pdf", width = 11 / 2.54, height = 11 /2.54)

par(mar = rep(0, 4))
plot(sn$geometry, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0))
plot(conifer_only$geometry, add = TRUE, pch = 19, cex = 0.25, col = plot_colors)
legend("topright", legend = years, pch = 19, title = "Years", col = year_colors, bty = "n", cex = 0.5)

par(fig = c(0, 0.4, 0, 0.4), new = TRUE)
plot(ca$geometry, axes = FALSE)
box(which = "figure")
plot(sn$geometry, add = TRUE)

dev.off()

