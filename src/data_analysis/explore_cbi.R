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
library(rlang)
library(sf)
library(dplyr)
library(mgcv)
library(purrr)
library(broom)
library(modelr)
library(tidyr)
library(lazyeval)

###
### Bilinear interpolation
###

# Get 1-month window, bilinear interpolation data
cbi_metadata_1_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_1-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_1_bilinear <- st_read("data/cbi_calibration/cbi-calibration_1-month-window_L5_bilinear-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_1_bilinear <- merge(cbi_data_1_bilinear, cbi_metadata_1_bilinear)

# Get 2-month window, bilinear interpolation data
cbi_metadata_2_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_2_bilinear <- st_read("data/cbi_calibration/cbi-calibration_2-month-window_L5_bilinear-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_2_bilinear <- merge(cbi_data_2_bilinear, cbi_metadata_2_bilinear)

# Get 3-month window, bilinear interpolation data
cbi_metadata_3_bilinear <- read.csv("data/cbi_calibration/cbi-calibration_3-month-window_L5_bilinear-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_3_bilinear <- st_read("data/cbi_calibration/cbi-calibration_3-month-window_L5_bilinear-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_3_bilinear <- merge(cbi_data_3_bilinear, cbi_metadata_3_bilinear)

### 
### Bicubic interpolation
###

# Get 1-month window, bicubic interpolation data
cbi_metadata_1_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_1_bicubic <- st_read("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_1_bicubic <- merge(cbi_data_1_bicubic, cbi_metadata_1_bicubic)

# Get 2-month window, bicubic interpolation data
cbi_metadata_2_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_2-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_2_bicubic <- st_read("data/cbi_calibration/cbi-calibration_2-month-window_L5_bicubic-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_2_bicubic <- merge(cbi_data_2_bicubic, cbi_metadata_2_bicubic)

# Get 3-month window, bicubic interpolation data
cbi_metadata_3_bicubic <- read.csv("data/cbi_calibration/cbi-calibration_3-month-window_L5_bicubic-interp_metadata.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
cbi_data_3_bicubic <- st_read("data/cbi_calibration/cbi-calibration_3-month-window_L5_bicubic-interp.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

cbi_3_bicubic <- merge(cbi_data_3_bicubic, cbi_metadata_3_bicubic)

cbi_list <- list(bilinear_1 = cbi_1_bilinear,
                 bilinear_2 = cbi_2_bilinear,
                 bilinear_3 = cbi_3_bilinear,
                 bicubic_1 = cbi_1_bicubic,
                 bicubic_2 = cbi_2_bicubic,
                 bicubic_3 = cbi_3_bicubic)

# Conveient function to get coefficient of determination from a non-linear model. Note this value (R^2)
# does NOT have the same meaning in a non-linear context as it does in a a linear context. Thus
# it shouldn't be used as an estimate of how much variation in the data is explained by the model.
# Here, I'm using it to at least have some comparison to severity models in the literature.

r2 <- function(m) {
  r2 <- 1 - (sum(residuals(m)^2)) / sum(((m$model[[1]]) - mean(m$model[[1]], na.rm = TRUE))^2, na.rm = TRUE)
  r2
}

# Non-linear models (of the form used by Miller and Thode (2007) and Parks et al. (2014))
### Example of overall R^2
m1a <- nls(RdNBR ~ a + b * exp(cbi_over * c), 
            data = cbi_2_bilinear,
            start = list(a = 0, b = 1, c = 1),
            model = TRUE)

r2(m1a)

plot(cbi_2_bilinear$cbi_over, cbi_2_bilinear$RdNBR, pch = 19)
lines(seq(0, 3, by = 0.01), predict(m1a, newdata = data.frame(cbi_over = seq(0, 3, by = 0.01))))

# Where would the cutoff for "high severity" be? CBI of 2.25 or greater translates to an RdNBR of...
severity_thresholds <- predict(m1a, newdata = data.frame(cbi_over = c(0.1, 1.25, 2.25)))
severity_thresholds

###
### K-fold cross validation
###

severity_kfold <- function(data, response, k) {
  # Let the response variable be flexible and turn the string into a full formula for the nls() function
  formula <- as.formula(paste0(response, " ~ a + b * exp(cbi_over * c)"))

  data %>%
    as.data.frame() %>%
    select(-geometry) %>% # Remove the geometry column
    crossv_kfold(k = k) %>% # Divide the data into k folds with a test and training set associated with each of the k sets
    mutate(model = map(train, ~ nls(formula = formula, 
                                    data = as.data.frame(.),
                                    start = list(a = 0, b = 1, c = 1)))) %>%
    mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>% # parallel mapping of the model and test data.frames to add, as a new column, the predicted severity values for the test data ffrom a model fit to the training data
    unnest(predicted) %>%
    mutate_(residual = interp(~.fitted - response, response = as.name(response))) %>% # calculate residuals (we use the interp() function to allow the response variable to be a flexible argument passed by the user)
    group_by(.id) %>%
    summarize_(
      sst = interp(~ sum( (response - mean(response, na.rm = TRUE)) ^ 2, na.rm = TRUE), response = as.name(response)), # Total sum of squares
      sse = interp(~sum(residual ^ 2, na.rm = TRUE)), # sum of squares error
      mse = interp(~mean(residual ^ 2, na.rm = TRUE))) %>% # mean square error
    mutate(r.squared = 1 - sse / sst) # calculate r.squared value (note this is *not* the same interpretation as the R^2 in a linear model)
}

### Leave out dEVI and RdEVI because they have some weirdness.
d <- cbi_list[[1]]
plot(d$cbi_over, d$dEVI)
plot(d$cbi_over, d$RdEVI)

g <- ggplot(d, aes(x = cbi_over, y = dEVI)) +
  geom_point(aes(text = system.index))

ggplotly(g)

###
### Include just conifer forest data, as determined by pre-EuroAmerican-settlement fire regime types from the Fire Return Interval Departure database
###

# Response values (9): RdNBR, dNBR, RdNBR2, dNBR2, RdNDVI, dNDVI, RBR (we leave out dEVI and RdEVI because they are just bad model fits)
# Interpolation (2): bilinear, bicubic
# Window period (3): 1 month, 2 month, 3 month
# Model fit variables: a, b, c, R^2

# Data frame should be 8 columns (including an id column) and 54 (9*2*3) rows
response_vars <- c("RdNBR", "dNBR", "RdNBR2", "dNBR2", "RdNDVI","dNDVI", "RBR") # not including dEVI and RdEVI
interpolation_vars <- c("bilinear", "bicubic")
time_window_vars <- 1:3
conifer_filter <- TRUE # Just use CBI plots found in yellow pine/mixed conifer forest

num_rows <- length(response_vars) * length(interpolation) * length(time_window)

set.seed(1826) # Set seed at current time for reproducibility

# Create data structure to hold results
model_summary <- data.frame(id = 1:num_rows, 
                            expand.grid(response = response_vars, 
                                        time_window = time_window_vars, 
                                        interpolation = interpolation_vars,
                                        stringsAsFactors = FALSE), 
                            a = numeric(num_rows), 
                            b = numeric(num_rows), 
                            c = numeric(num_rows), 
                            r2_kfold = numeric(num_rows),
                            r2_all = numeric(num_rows),
                            unchanged = rep(0, num_rows),
                            low_sev = rep(1, num_rows),
                            mod_sev = rep(2, num_rows),
                            hi_sev = rep(3, num_rows))

# Iterate through all data.frames (each represents a time_window/interpolation type combination)
for (i in seq_along(cbi_list)) {
  # Iterate through the different remotely-sensed wildfire severity metrics
  for (j in seq_along(response_vars)) {
    
    parse_list_name <- unlist(strsplit(names(cbi_list[i]), "_")) # Get what time_window/interpolation type from the list element name
    interp <- parse_list_name[1]
    time_window <- as.numeric(parse_list_name[2])
    
    if( conifer_filter) { # check whether only PFR defined yellow pine/mixed conifer should be used
      data <- subset(cbi_list[[i]], subset = conifer_forest == 1)
    } else {
      data <- cbi_list[[i]]
    } 
    
    model <- as.formula(paste0(response_vars[j], " ~ a + b * exp(cbi_over * c)"))
    fitted_model <- try(nls(formula = model, 
                            data = data,
                            start = list(a = 0, b = 1, c = 1),
                            model = TRUE))
    
    r2_kfold <- try(severity_kfold(data = data, response = response_vars[j], k = 5) %>% summarize(mean(r.squared)) %>% as.numeric())
    r2_all <- try(r2(fitted_model))
    idx <- which(model_summary$response == response_vars[j] & model_summary$interpolation == interp & model_summary$time_window == time_window)
    
    if(class(fitted_model) != "try-error") {
      model_summary[idx, c("a", "b", "c")] <- coef(fitted_model)
 
      thresholds <- predict(fitted_model, newdata = data.frame(cbi_over = c(0, 0.1, 1.25, 2.25)))
      model_summary[idx, c("unchanged", "low_sev", "mod_sev", "hi_sev")] <- thresholds
     }
    if(class(r2_kfold) != "try-error") {
      model_summary[idx, "r2_kfold"] <- r2_kfold
    }
    if(class(r2_all) != "try-error") {
      model_summary[idx, "r2_all"] <- r2_all
    }
  }     
}

model_summary
model_summary[order(model_summary$r2_all, decreasing = TRUE), ]

# For conifer forest, it appears that the bicubic interpolation of RdNBR and using a 1-month window prior to the fire results in the best fit to on-the-ground severity. The bicubic interpolation of RBR using a 1-month window prior to the fire results in the best fit when all data are used.
# But we get pretty darn good fits for RBR and RdNDVI in both 1, 2, and 3 month windows and using both bilinear and bicubic interpolation.


degToRad <- function(deg) {
  deg * pi / 180
}

d <- cbi_1_bicubic
d$aspect <- cos(degToRad(d$aspect - 135))

plot(d$cbi_over[d$conifer_forest == 1], d$RBR[d$conifer_forest == 1], pch = 19)
plot(d$fm100[d$conifer_forest == 1], d$RBR[d$conifer_forest == 1], pch = 19)
m1 <- nls(RBR ~ a + b * exp(cbi_over * c), 
                 data = subset(d, conifer_forest == 1),
                 start = list(a = 0, b = 1, c = 1),
                 model = TRUE) 
lines(seq(0, 3, by = 0.01), predict(m1, newdata = data.frame(cbi_over = seq(0, 3, by = 0.01))))  
summary(m1)  
r2(m1)

fm1 <- lm(RdNBR ~ preFire_ndvi + het_ndvi_1 + topo_roughness_1, data = d[d$conifer_forest == 1, ])
summary(fm1)


# Get 1-month window, bicubic interpolation data
d_meta <- read.csv("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp_metadata_test.csv", stringsAsFactors = FALSE) %>%
  select(-.geo)
d_data <- st_read("data/cbi_calibration/cbi-calibration_1-month-window_L5_bicubic-interp_test.geojson", stringsAsFactors = FALSE) %>%
  rename(system.index = id)

d <- merge(d_data, d_meta)
head(d_meta)
head(d_data)
head(d)

d_data$conifer_forest
cbi_data_1_bicubic$conifer_forest
head(cbi_data_2_bicubic)
head(cbi_1_bicubic)

d_data %>%
  filter(as.logical(conifer_forest)) %>%
  ggplot(aes(x = cbi_over, y = RBR)) +
  geom_point()

plot(d$geometry)
dim(d)
dim(d_data)
