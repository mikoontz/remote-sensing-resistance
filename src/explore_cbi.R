library(sf)
library(dplyr)
library(mgcv)

cbi_data <- read.csv("data/cbi_calibration/cbi_calibration.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
  cbi_metadata <- read.csv("data/cbi_calibration/remote_resistance_cbi_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(4326)

head(cbi_data)
head(cbi_metadata)

cbi <- merge(cbi_data, cbi_metadata)
head(cbi)
cbind(cbi_data$system.index, cbi_metadata$system.index)
cbi_data$system.index %in% cbi_metadata$system.index

m1 <- lm(RdNBR ~ cbi_over, data = cbi)
m1b <- lm(RdNBR ~ cbi_tot, data = cbi)
m1c <- gam(RdNBR ~ s(cbi_over), data = cbi)
m2 <- lm(RdNBR2 ~ cbi_over, data = cbi)
m3 <- lm(RdNDVI ~ cbi_over, data = cbi)
m4 <- lm(RdEVI ~ cbi_over, data = cbi)

m5 <- lm(cbi_over ~ het_ndvi_1, data = cbi)
plot(cbi$het_ndvi_4, cbi$cbi_over)
abline(m5)
summary(m5)
summary(m1)
summary(m1b)
summary(m2)
summary(m3)
summary(m4)

cbi[which.min(cbi$RdEVI), ]
plot(cbi$cbi_over, cbi$RdNBR)
plot(m1b, residuals = TRUE)

?mgcv
?plot.gam
curve(expr = coef(m1)[1] + coef(m2)[2]*(x + x^2), add = TRUE)
plot(cbi$cbi_tot, cbi$RdNBR)
plot(cbi$cbi_over, cbi$RdNBR2)
plot(cbi$cbi_over, cbi$RdNDVI)
plot(cbi$cbi_over, cbi$RdEVI)

str(cbi)
