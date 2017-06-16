library(sf)

sn <- st_read("data/features/SierraEcoregion_Jepson/") %>% st_transform(4326)
s <- read.csv("data/fire_samples/L5_20100401_20110504_remote_resistance_0_001_percent.csv")
coordinates(s) <- ~ lon + lat
crs(s) <- "+init=epsg:4236"
s <- st_as_sf(s)

pairs(s[, c("RdNBR", "RdNBR2", "RdNDVI", "RdEVI")])
pairs(s[, c("het_ndvi_1", "het_ndwi_1", "het_evi_1")])
pairs(s[, c("het_evi_1", "het_evi_2", "het_evi_3", "het_evi_4")])

plot(s$het_ndvi_1, s$RdNBR)
plot(s$median_ndvi, s$RdNBR)
plot(s$focal_mean_evi_1, s$RdNBR)

plot(st_geometry(sn))
plot(st_geometry(s), pch = 19, col = 1, asp = 1, add = TRUE)

