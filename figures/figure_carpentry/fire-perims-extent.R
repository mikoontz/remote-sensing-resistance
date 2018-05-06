# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)


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

if (!file.exists("data/data_output/burned-fire-samples_texture_configured.rds")) {
  source("data/data_carpentry/configure_fire-samples.R")
}

load(here::here("data/data_output/burned-fire-samples_texture_configured.rds"))

# Get conifer forest ------------------------------------------------------

mixed_con <- raster("data/data_output/landcover_PFR/mixed_conifer.tif")
# plot(mixed_con) 

# Get frap fires --------------------------------------------------
frap <- raster("data/data_output/frap-fire-extent-image.tif")

# mask out the non-conifer areas ------------------------------------------
frap_mixed_con <- 
  frap %>% 
  mask(mixed_con, maskvalue = 0)

# Get the outline of the Sierra Nevada ------------------------------------
sn <- st_read("data/features/SierraEcoregion_Jepson/") %>% st_transform(proj4string(frap))

# Get California outline --------------------------------------------------

ca <- raster::getData(name = "GADM",country = "USA",level = 1, path = "data/features") %>% 
  st_as_sf() %>% 
  filter(NAME_1 == "California") %>% 
  st_transform(st_crs(sn))

# Less_burned detailed CA outline in case first version makes things too slow ---------------------------------------

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

#  Build the plot! --------------------------------------------------------

# pdf("figures/frap-extent.pdf", width = 8.2 / 2.54, height = 10.2 /2.54)
mar_old <- par()$mar
fig_old <- par()$fig

pdf("figures/frap-extent.pdf", width = 11 / 2.54, height = 11 /2.54)

par(mar = rep(0, 4))
plot(sn$geometry, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0), col = "lightgrey")
plot(frap_mixed_con, add = TRUE, col = viridis(6))

par(fig = c(0, 0.4, 0, 0.4), new = TRUE)
plot(ca$geometry, axes = FALSE)
box(which = "figure")
plot(sn$geometry, add = TRUE, col = "lightgrey")

par(fig = fig_old, new = FALSE, mar = mar_old)

dev.off()


# pdf("figures/fire-samples-extent.pdf", width = 8.2 / 2.54, height = 10.2 /2.54)
pdf("figures/fire-samples-extent.pdf", width = 11 / 2.54, height = 11 /2.54)

par(mar = rep(0, 4))
plot(sn$geometry, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0), col = "lightgrey")
plot(ss_burned$geometry, add = TRUE, pch = 19, cex = 0.01)

par(fig = c(0, 0.4, 0, 0.4), new = TRUE)
plot(ca$geometry, axes = FALSE)
box(which = "figure")
plot(sn$geometry, add = TRUE, col = "lightgrey")

par(fig = fig_old, new = FALSE, mar = mar_old)

dev.off()

# Create a conifer plot

pdf("figures/mixed-conifer.pdf", width = 11 / 2.54, height = 11 / 2.54)
mixed_con <- raster("data/data_output/landcover_PFR/mixed_conifer_sn-mask_10-fold-res.tif")

par(mar = rep(0, 4))
plot(sn$geometry, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0), col = "lightgrey")
plot(mixed_con, col = c("white", "darkgreen"), add = TRUE, legend = FALSE)

par(fig = c(0, 0.4, 0, 0.4), new = TRUE)
plot(ca$geometry, axes = FALSE)
box(which = "figure")
plot(sn$geometry, add = TRUE, col = "lightgrey")

par(fig = fig_old, new = FALSE, mar = mar_old)

dev.off()


# Tripanel plot of fire extent, mixed conifer extent, and samples from those fires used in analysis
pdf("figures/study-geographic-setting.pdf", width = 17.3 / 2.54, height = 11 / 2.54)
par(mfrow = c(1, 3), mar = c(0, 2, 0, 0), oma = c(0, 3, 0, 0), xpd = NA)
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

plot(sn$geometry, cex.axis = 0.5, las = 1, col = "lightgrey")
used_ss_burned <- ss_burned %>% 
  filter(conifer_forest == 1)
plot(ss_burned$geometry, add = TRUE, pch = 19, cex = 0.1)

text(x = -119, y = 40, labels = "C", cex = 3)

par(fig = c(0, 0.15, 0, 0.35), new = TRUE, mar = rep(0, 4))
plot(ca$geometry, axes = FALSE, outer = TRUE)
plot(sn$geometry, add = TRUE, col = "lightgrey", outer = TRUE)
# box(which = "figure")

dev.off()

