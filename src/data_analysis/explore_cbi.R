### This script will help us deterimine what moving window to use for our algorithm (how long before the fire (1, 2, or 3 months)) should
### we gather Landsat imagery in order to take the median spectral values across those available images.
### This is a tradeoff: the further back we look, the more likely that we'll capture non-cloudy pixels and perhaps reduce the
### influence of sensor anomolies from any particular image, but we also blur the phenology by looking too far back.
### We also want to determine whether bilinear or bicubic interpolation works better. Bicubic interpolation is more computationally challengin,
### so has seldom been used for this purpose, but it also incorporates data from a larger neighborhood window which may be inappropriate.

### We use cross-fold validation and R^2 to make this determination. Note that R^2 doesn't represent the same thing in non-linear regression
### as it does for linear regression, so it shouldn't be interpreted in the way it is in linear regression (i.e. "the percent of total variation
### explained by the model" <-- it's not that.)

library(ggplot2)
library(plotly)
library(sf)
library(dplyr)
library(mgcv)
library(purrr)
library(broom)
library(modelr)
library(tidyr)

###
### Bilinear interpolation
###

# Get 1-month window, bilinear interpolation data
cbi_metadata_1_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_1-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_1_bilinear <- st_read("data/cbi_calibration/cbi-calibration_1-month-window_L5_bilinear-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_1_bilinear <- merge(cbi_data_1_bilinear, cbi_metadata_1_bilinear)

# Get 2-month window, bilinear interpolation data
cbi_metadata_2_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_2_bilinear <- st_read("data/cbi_calibration/cbi-calibration_2-month-window_L5_bilinear-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_2_bilinear <- merge(cbi_data_2_bilinear, cbi_metadata_2_bilinear)

# Get 3-month window, bilinear interpolation data
cbi_metadata_3_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_3-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_3_bilinear <- st_read("data/cbi_calibration/cbi-calibration_3-month-window_L5_bilinear-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_3_bilinear <- merge(cbi_data_3_bilinear, cbi_metadata_3_bilinear)

### 
### Bicubic interpolation
###

# Get 1-month window, bicubic interpolation data
cbi_metadata_1_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_1_bicubic <- st_read("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_1_bicubic <- merge(cbi_data_1_bicubic, cbi_metadata_1_bicubic)

# Get 2-month window, bicubic interpolation data
cbi_metadata_2_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_2_bicubic <- st_read("data/cbi_calibration/cbi-calibration_2-month-window_L5_bicubic-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_2_bicubic <- merge(cbi_data_2_bicubic, cbi_metadata_2_bicubic)

# Get 3-month window, bicubic interpolation data
cbi_metadata_3_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_3-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_3_bicubic <- st_read("data/cbi_calibration/cbi-calibration_3-month-window_L5_bicubic-interp.geojson.json", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_3_bicubic <- merge(cbi_data_3_bicubic, cbi_metadata_3_bicubic)

###
### Read in Sierra Nevada shapefile
###

sn <- st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(4326)

# Conveient function to get coefficient of determination from a non-linear model. Note this value (R^2)
# does NOT have the same meaning in a non-linear context as it does in a a linear context. Thus
# it shouldn't be used as an estimate of how much variation in the data is explained by the model.
# Here, I'm using it to at least have some comparison to severity models in the literature.

r2 <- function(m) {
  r2 <- 1 - (sum(residuals(m)^2)) / sum(((m$model[[1]]) - mean(m$model[[1]], na.rm = TRUE))^2, na.rm = TRUE)
  r2
}

# Non-linear models (of the form used by Miller and Thode (2007) and Parks et al. (2014))
m1a <- nls(RdNBR ~ a + b * exp(cbi_over * c), 
            data = cbi_1_bilinear,
            start = list(a = 0, b = 1, c = 1),
            model = TRUE)

r2(m1a)

###
### K-fold cross validation
###

###
### START 1-month window, bicubic interpolation
###

RdNBR_5fold_1_bicubic <- 
  cbi_1_bicubic %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_1_bicubic$mse)
mean(RdNBR_5fold_1_bicubic$r.squared)

###
### END 1-month window, bicubic interpolation
###

###
### START 2-month window, bicubic interpolation
###

RdNBR_5fold_2_bicubic <- 
  cbi_2_bicubic %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_2_bicubic$mse)
mean(RdNBR_5fold_2_bicubic$r.squared)

###
### END 2-month window, bicubic interpolation
###

###
### START 3-month window, bicubic interpolation
###

RdNBR_5fold_3_bicubic <- 
  cbi_3_bicubic %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_3_bicubic$mse)
mean(RdNBR_5fold_3_bicubic$r.squared)

###
### END 3-month window, bicubic interpolation
###

###
### START 1-month window, bilinear interpolation
###

RdNBR_5fold_1_bilinear <- 
  cbi_1_bilinear %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_1_bilinear$mse)
mean(RdNBR_5fold_1_bilinear$r.squared)

###
### END 1-month window, bilinear interpolation
###

###
### START 2-month window, bilinear interpolation
###

RdNBR_5fold_2_bilinear <- 
  cbi_2_bilinear %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_2_bilinear$mse)
mean(RdNBR_5fold_2_bilinear$r.squared)

###
### END 2-month window, bilinear interpolation
###

###
### START 3-month window, bilinear interpolation
###

RdNBR_5fold_3_bilinear <- 
  cbi_3_bilinear %>%
  as.data.frame() %>%
  select(-geometry) %>%
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ nls(RdNBR ~ a + b * exp(cbi_over * c), 
                                  data = as.data.frame(.),
                                  start = list(a = 0, b = 1, c = 1)))) %>%
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>%
  unnest(predicted) %>%
  mutate(residual = .fitted - RdNBR) %>%
  group_by(.id) %>%
  summarize(
    sst = sum((RdNBR - mean(RdNBR, na.rm = TRUE)) ^ 2, na.rm = TRUE),
    sse = sum(residual ^ 2, na.rm = TRUE),
    mse = mean(residual ^ 2, na.rm = TRUE),
    r.squared = 1 - sse / sst
  )

mean(RdNBR_5fold_3_bilinear$mse)
mean(RdNBR_5fold_3_bilinear$r.squared)

###
### END 3-month window, bilinear interpolation
###


m1b <- nls(RdNBR ~ a + b * exp(cbi_over * c), 
           data = cbi_2_bicubic,
           start = list(a = 0, b = 1, c = 1),
           model = TRUE)

r2(m1a)
r2(m1b)

plot(cbi_2_bicubic$cbi_over, cbi_2_bicubic$RdNBR, pch = 19)
lines(seq(0, 3, by = 0.01), predict(m1b, newdata = data.frame(cbi_over = seq(0, 3, by = 0.01))))

# Where would the cutoff for "high severity" be? CBI of 2.25 or greater translates to an RdNBR of...
severity_thresholds <- predict(m1b, newdata = data.frame(cbi_over = c(0.1, 1.25, 2.25)))


mean(RdNBR_5fold_1_bilinear$mse)
mean(RdNBR_5fold_2_bilinear$mse)
mean(RdNBR_5fold_3_bilinear$mse)
mean(RdNBR_5fold_1_bicubic$mse)
mean(RdNBR_5fold_2_bicubic$mse)
mean(RdNBR_5fold_3_bicubic$mse)

mean(RdNBR_5fold_1_bilinear$r.squared)
mean(RdNBR_5fold_2_bilinear$r.squared)
mean(RdNBR_5fold_3_bilinear$r.squared)
mean(RdNBR_5fold_1_bicubic$r.squared)
mean(RdNBR_5fold_2_bicubic$r.squared)
mean(RdNBR_5fold_3_bicubic$r.squared)

head(cbi_2_bilinear)
summary(cbi_2_bilinear)

### Linear models
m1a <- lm(dNBR ~ cbi_over, data = cbi_2_bicubic)
summary(m1a)
plot(cbi_2_bicubic$cbi_over, cbi_2_bicubic$dNBR)
lines(abline(m1a))

m1b <- lm(RdNBR ~ cbi_over, data = cbi_2_bicubic)
summary(m1b)
plot(cbi_2_bicubic$cbi_over, cbi_2_bicubic$RdNBR)
lines(abline(m1b))


m2a <- lm(dNDVI ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$dNDVI)
summary(m2a)

m2b <- lm(RdNDVI ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$RdNDVI)
summary(m2b)

m <- lm(RBR ~ cbi_over, data = cbi)
plot(cbi$cbi_over, cbi$RBR)
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
