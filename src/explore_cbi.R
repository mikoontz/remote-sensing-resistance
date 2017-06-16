library(sf)

cbi <- st_read("data/features/cbi_data/cbi_sn/")
sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(4326)

plot(st_geometry(cbi))
plot(st_geometry(sn), add = TRUE)
str(cbi)
