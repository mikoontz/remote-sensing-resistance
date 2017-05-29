library(sf)

cbi <- st_read("data/sierra_nevada_cbi/")
sn <- st_read("data/SierraEcoregion_TNC/")

plot(cbi$geometry)
plot(sn$geometry, add = TRUE)
str(cbi)
