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
# object is called 'samps'

if (file.exists("data/data_output/all-fire-samples.rds")) {
  load("data/data_output/all-fire-samples.rds")
} else {
  source("data/data_carpentry/merge_fire_samples.R")
}

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

#  Build the plot! --------------------------------------------------------

pdf("figures/frap-extent.pdf", width = 8.2 / 2.54, height = 10.2 /2.54)

plot(ca$geometry, axes = TRUE, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0))
plot(sn$geometry, add = TRUE)
plot(frap_mixed_con, add = TRUE, col = viridis(6))

dev.off()

pdf("figures/fire-samples-extent.pdf", width = 8.2 / 2.54, height = 10.2 /2.54)

plot(ca$geometry, axes = TRUE, cex.axis = 0.5, las = 1, mgp = c(3, 0.75, 0))
plot(sn$geometry, add = TRUE)
plot(samps$geometry, add = TRUE, pch = 19, cex = 0.01)

dev.off()
