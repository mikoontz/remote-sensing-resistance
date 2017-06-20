library(sf)
library(lme4)

sn <- st_read("data/features/SierraEcoregion_Jepson/") %>% st_transform(4326)
s <- read.csv("data/fire_samples/L5_19840401_20110504_remote_resistance_0_001_percent.csv",
              stringsAsFactors = FALSE)
coordinates(s) <- ~ lon + lat
crs(s) <- "+init=epsg:4236"
s <- st_as_sf(s)

dim(s)
colnames(s)
head(s)

system_id <- do.call(rbind, strsplit(x = s$system.index, split = "_", fixed = TRUE))[, 1]
s$system_id <- system_id
s$summer_burn <- ifelse(s$julian_day >= 172 & s$julian_day < 265, yes = 1, no = 0)
s$aspect <- cos(-1 * s$aspect + 225)

# Only 332 fires represented out of 1652 -- too much subsampling?
unique(s$system_id)

# Scale the predictors
scaled_s <-
  s %>%
  as.data.frame() %>%
  select(-system.index, -RdEVI, -RdNBR, -RdNBR2, -RdNDVI, -date, -.geo, -geometry, -system_id, -summer_burn, -satellite, -julian_day) %>%
  apply(MARGIN = 2, scale)

colnames(scaled_s) <- paste0("s_", colnames(scaled_s))
ss <- cbind(s, scaled_s)
head(ss)

m1 <- lmer(RdNBR ~ summer_burn * (s_het_ndvi_1 + s_aspect + I(s_slope^2)) + (1 | system_id), data = ss)
m2 <- lmer(RdNBR ~ s_het_ndvi_2 + s_aspect + s_slope + summer_burn + (1 | system_id), data = ss)
m3 <- lmer(RdNBR ~ s_het_ndvi_3 + s_aspect + s_slope + summer_burn + (1 | system_id), data = ss)
m4 <- lmer(RdNBR ~ s_het_ndvi_4 + s_aspect + s_slope + summer_burn + (1 | system_id), data = ss)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)


pairs(s[, c("RdNBR", "RdNBR2", "RdNDVI", "RdEVI")])
pairs(s[, c("het_ndvi_1", "het_ndwi_1", "het_evi_1")])
pairs(s[, c("het_evi_1", "het_evi_2", "het_evi_3", "het_evi_4")])

plot(s$het_ndvi_1, s$RdNBR)
plot(s$median_ndvi, s$RdNBR)
plot(s$focal_mean_evi_1, s$RdNBR)

plot(st_geometry(sn))
plot(st_geometry(s), pch = 19, col = 1, asp = 1, add = TRUE)

