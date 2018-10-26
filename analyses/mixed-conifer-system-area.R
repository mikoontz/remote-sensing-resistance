# Calculate the total area of yellow pine/mixed conifer forest

library(sf)
library(units)

mixed_con <- st_read("data/data_output/landcover_PFR/mixed_conifer/mixed_conifer.shp")
area <- st_area(mixed_con)

total_area <- sum(area)

units(total_area) <- ud_units$ha
total_area
