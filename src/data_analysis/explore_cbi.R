### This script will help us deterimine what moving window to use for our algorithm (how long before the fire (16, 32, 48, or 64 days)) should
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
library(lme4)

###
### Bilinear interpolation
###

# Get 16-day window, bilinear interpolation data
cbi_16_bilinear <- st_read("data/cbi_calibration/cbi-calibration_16-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

# Get 32-day window, bilinear interpolation data
cbi_32_bilinear <- st_read("data/cbi_calibration/cbi-calibration_32-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

# Get 48-day window, bilinear interpolation data
cbi_48_bilinear <- st_read("data/cbi_calibration/cbi-calibration_48-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

# Get 64-day window, bilinear interpolation data
cbi_64_bilinear <- st_read("data/cbi_calibration/cbi-calibration_64-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

### 
### Bicubic interpolation
###

# Get 16-day window, bicubic interpolation data
cbi_16_bicubic <- st_read("data/cbi_calibration/cbi-calibration_16-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)

# Get 32-day window, bicubic interpolation data
cbi_32_bicubic <- st_read("data/cbi_calibration/cbi-calibration_32-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)

# Get 48-day window, bicubic interpolation data
cbi_48_bicubic <- st_read("data/cbi_calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)

# Get 64-day window, bicubic interpolation data
cbi_64_bicubic <- st_read("data/cbi_calibration/cbi-calibration_64-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)

cbi_list <- list(bilinear_16 = cbi_16_bilinear,
                 bilinear_32 = cbi_32_bilinear,
                 bilinear_48 = cbi_48_bilinear,
                 bilinear_64 = cbi_64_bilinear,
                 bicubic_16 = cbi_16_bicubic,
                 bicubic_32 = cbi_32_bicubic,
                 bicubic_48 = cbi_48_bicubic,
                 bicubic_64 = cbi_64_bicubic)

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
m1a <- nls(RBR ~ a + b * exp(cbi_over * c), 
            data = cbi_16_bicubic[cbi_16_bicubic$conifer_forest == 1, ],
            start = list(a = 0, b = 1, c = 1),
            model = TRUE)

r2(m1a)

plot(cbi_16_bicubic$cbi_over[cbi_16_bicubic$conifer_forest == 1], 
     cbi_16_bicubic$RBR[cbi_16_bicubic$conifer_forest == 1], 
     pch = 19)
lines(seq(0, 3, by = 0.01), predict(m1a, newdata = data.frame(cbi_over = seq(0, 3, by = 0.01))))

# Where would the cutoff for "high severity" be? CBI of 2.25 or greater translates to an RdNBR of...
severity_thresholds <- predict(m1a, newdata = data.frame(cbi_over = c(0, 0.1, 1.25, 2.25)))
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

###
### Include just conifer forest data, as determined by pre-EuroAmerican-settlement fire regime types from the Fire Return Interval Departure database
###

# Response values (9): RdNBR, dNBR, RdNBR2, dNBR2, RdNDVI, dNDVI, RBR (we leave out dEVI and RdEVI because they are just bad model fits)
# Interpolation (2): bilinear, bicubic
# Window period (4): 16 day, 32 day, 48 day, 64 day
# Model fit variables: a, b, c, R^2

# Data frame should be 8 columns (including an id column) and 54 (9*2*3) rows
response_vars <- c("RdNBR", "dNBR", "RdNBR2", "dNBR2", "RdNDVI","dNDVI", "RBR") # not including dEVI and RdEVI
interpolation_vars <- c("bilinear", "bicubic")
time_window_vars <- c(16, 32, 48, 64)
conifer_filter <- TRUE # Just use CBI plots found in yellow pine/mixed conifer forest

num_rows <- length(response_vars) * length(interpolation_vars) * length(time_window_vars)

set.seed(1117) # Set seed at current time for reproducibility

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
                            data = data[drop = TRUE],
                            start = list(a = 0, b = 1, c = 1),
                            model = TRUE))
    
    r2_kfold <- try(severity_kfold(data = data[drop = TRUE], 
                                   response = response_vars[j], 
                                   k = 5) %>% 
                      summarize(mean(r.squared)) 
                    %>% as.numeric())
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
model_summary[order(model_summary$r2_kfold, decreasing = TRUE), ]
model_summary[order(model_summary$r2_all, decreasing = TRUE), ]

write.csv(model_summary, "data/cbi_calibration_model_comparison.csv", row.names = FALSE)
# For conifer forest, it appears that the bicubic interpolation of RBR and using 
# a 48-day window prior to the fire results in the best fit to on-the-ground severity. 
# Best model using all the data is RBR, bicubic, 32-day window
# The ranking of models is fairly variable, with many taking the top spot
# depending on how the random assignment of training and test data go.
# For instance, RdNDVI ends up on top pretty often.

# How many missing values for each set of data?
nrow(cbi_16_bicubic) # 401 total points
nrow(cbi_16_bicubic[is.na(cbi_16_bicubic$RBR), ]) # 44 missing points
nrow(cbi_16_bicubic[is.na(cbi_16_bicubic$RBR), ]) / nrow(cbi_16_bicubic)

# Just a check, should be the same regardless of interpolation method
nrow(cbi_16_bilinear[is.na(cbi_16_bilinear$RBR), ])

nrow(cbi_32_bicubic[is.na(cbi_32_bicubic$RBR), ]) / nrow(cbi_32_bicubic)
nrow(cbi_48_bicubic[is.na(cbi_48_bicubic$RBR), ]) / nrow(cbi_48_bicubic)
nrow(cbi_64_bicubic[is.na(cbi_64_bicubic$RBR), ]) / nrow(cbi_64_bicubic)


d <- cbi_32_bicubic

plot(d$cbi_over[d$conifer_forest == 1], d$RBR[d$conifer_forest == 1], pch = 19)
m1 <- nls(RBR ~ a + b * exp(cbi_over * c), 
                 data = subset(d, conifer_forest == 1),
                 start = list(a = 0, b = 1, c = 1),
                 model = TRUE) 
lines(seq(0, 3, by = 0.01), predict(m1, newdata = data.frame(cbi_over = seq(0, 3, by = 0.01))))  
summary(m1)  
r2(m1)
