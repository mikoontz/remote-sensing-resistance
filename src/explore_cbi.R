library(ggplot2)
library(plotly)
library(sf)
library(dplyr)
library(mgcv)

cbi_data <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_bicubic-interp.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-.geo)
cbi_metadata <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(-.geo)
cbi <- merge(cbi_data, cbi_metadata)

sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(4326)

m1a <- lm(dNBR ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$dNBR)
summary(m1a)

m1b <- lm(RdNBR ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$RdNBR)
summary(m1b)

m2a <- lm(dNDVI ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$dNDVI)
summary(m2a)

m2b <- lm(RdNDVI ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$RdNDVI)
summary(m2b)

m <- lm(RdNBR ~ het_ndvi_1, data = cbi)
plot(cbi$het_ndvi_1, cbi$RdNBR)
summary(m)
lines(abline(m))

m1b <- lm(RdNBR ~ cbi_tot, data = cbi)
m1c <- gam(RdNBR ~ s(cbi_over), data = cbi)
m2 <- lm(RdNBR2 ~ cbi_over, data = cbi)
m3 <- lm(RdNDVI ~ cbi_over, data = cbi)
m4 <- lm(RdEVI ~ cbi_over, data = cbi)

m5 <- lm(cbi_over ~ het_ndvi_1, data = cbi)
plot(cbi$het_ndvi_4, cbi$cbi_over)
abline(m5)
summary(m5)
summary(m1b)
summary(m2)
summary(m3)
summary(m4)

cbi[which.min(cbi$RdEVI), ]

plot(m1b, residuals = TRUE)
??plotly

g <- ggplot(subset(cbi, subset = cbi$fire_name != "Lost"), aes(x = cbi_over, y = RdNBR, text = fire_name)) +
  geom_point()
ggplotly(g)

f <- ggplot(cbi, aes(x = cbi_over, y = RdNBR, text = fire_name)) +
  geom_point()
ggplotly(f)
curve(expr = coef(m1)[1] + coef(m2)[2]*(x + x^2), add = TRUE)
plot(cbi$cbi_tot, cbi$RdNBR)
plot(cbi$cbi_over, cbi$RdNBR2)
plot(cbi$cbi_over, cbi$RdNDVI)
plot(cbi$cbi_over, cbi$RdEVI)

str(cbi)
