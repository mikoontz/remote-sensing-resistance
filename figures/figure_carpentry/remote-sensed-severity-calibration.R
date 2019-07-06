# From Ecology Letters figure guidelines, to give an example:
# single column (82 mm), two-thirds page width (110 mm) or full page width (173 mm)

library(sf)
library(dplyr)
library(readr)

# Get 48-day window, bicubic interpolation data
# For the RBR severity metric
cbi_48_bicubic <- st_read("data/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bicubic-interp.geojson", stringsAsFactors = FALSE)

# Get 32-day window, bilinear interpolation data
# For the RdNBR severity metric
cbi_32_bilinear <- st_read("data/ee_cbi-calibration/cbi-calibration_32-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

# Get 48-day window, bilinear interpolation data
# For the RdNDVI severity metric
cbi_48_bilinear <- st_read("data/ee_cbi-calibration/cbi-calibration_48-day-window_L57_bilinear-interp.geojson", stringsAsFactors = FALSE)

cbi_table <- read_csv("data/data_output/cbi_calibration_model_comparison.csv")


cbi_mask_conifer <- function(data) {
    data <- filter(data, conifer_forest == 1)
}

cbi_fit <- function(data, response) {

  colnames(data)[colnames(data) == response] <- "response"
  
  fitted_model <- nls(response ~ a + b * exp(cbi_over * c), 
                      data = data,
                      start = list(a = 0, b = 1, c = 1),
                      model = TRUE)
}

cbi_calibration_stats <- function(cbi_table, response, time_window, interpolation) {

    cbi_table[cbi_table$response == response & cbi_table$time_window == time_window & cbi_table$interpolation == interpolation, , drop = TRUE]

  }

cbi_over <- seq(0, 3, by = 0.1)

# Fit nonlinear models and get predictions
fm_rbr <- cbi_fit(data = cbi_mask_conifer(cbi_48_bicubic), response = "RBR")
yhat_rbr <- predict(object = fm_rbr, newdata = data.frame(cbi_over))
cbi_rbr <- cbi_calibration_stats(cbi_table = cbi_table, 
                                 response = "RBR", 
                                 time_window = 48, 
                                 interpolation = "bicubic")

fm_rdnbr <- cbi_fit(data = cbi_mask_conifer(cbi_32_bilinear), response = "RdNBR")
yhat_rdnbr <- predict(object = fm_rdnbr, newdata = data.frame(cbi_over))
cbi_rdnbr <- cbi_calibration_stats(cbi_table = cbi_table, 
                                   response = "RBR", 
                                   time_window = 32, 
                                   interpolation = "bilinear")

fm_rdndvi <- cbi_fit(data = cbi_mask_conifer(cbi_48_bilinear), response = "RdNDVI")
yhat_rdndvi <- predict(object = fm_rdndvi, newdata = data.frame(cbi_over))
cbi_rdndvi <- cbi_calibration_stats(cbi_table = cbi_table, 
                                    response = "RdNDVI", 
                                    time_window = 48, 
                                    interpolation = "bilinear")

# Build the plots!
cex_in_panel_text <- 1
pos_in_panel_text <- c(0, 0.05, 0.10, 0.15, 0.20)
pch_color <- "darkgrey"

pdf("figures/remote-sensed-severity-calibration.pdf", width = 17.3 / 2.54, height = (17.3 / 2.54) * 0.6)
par(mfrow = c(1, 3), oma = c(2, 2, 0, 0.5), mar = c(3, 3, 2, 0.25))

# first panel: RBR 48 day window ------------------------------------------

plot(x = cbi_mask_conifer(cbi_48_bicubic)$cbi_over, y = cbi_mask_conifer(cbi_48_bicubic)$RBR, 
     pch = 19, col = pch_color, xlab = NA, ylab = NA, las = 1, main = "RBR (48-day window)")
lines(cbi_over, yhat_rbr)
# segments(x0 = 0, x1 = 3, y0 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][1, "hi_sev"]), y1 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][1, "hi_sev"]))

rbr_labs <-
  c(bquote(R^2 ~ "(k-fold)" ~ "=" ~ .(round(cbi_rbr$r2_kfold, 5))),
  bquote("RBR=" ~ beta[0] ~ "+" ~ beta[1] ~ "*" ~ e^beta[2] ~ "*cbi"),
  bquote(beta[0] ~ "=" ~ .(round(cbi_rbr$a, 5))),
  bquote(beta[1] ~ "=" ~ .(round(cbi_rbr$b, 5))),
  bquote(beta[2] ~ "=" ~ .(round(cbi_rbr$c, 5))))

lapply(seq_along(rbr_labs),
       FUN = function(i) {
         max_val <- max(cbi_mask_conifer(cbi_48_bicubic)$RBR, na.rm = TRUE)
         min_val <- min(cbi_mask_conifer(cbi_48_bicubic)$RBR, na.rm = TRUE)
         
         text(x = 0, y = max_val - (max_val - min_val) * pos_in_panel_text[i],
              labels = rbr_labs[[i]],
              pos = 4,
              cex = cex_in_panel_text)
         invisible()})

# Second panel RdNBR 32 day window ----------------------------------------

plot(x = cbi_mask_conifer(cbi_32_bilinear)$cbi_over, y = cbi_mask_conifer(cbi_32_bilinear)$RdNBR, 
     pch = 19, col = pch_color, xlab = NA, ylab = NA, las = 1, main = "RdNBR (32-day window)")
lines(cbi_over, yhat_rdnbr)
# segments(x0 = 0, x1 = 3, y0 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][2, "hi_sev"]), y1 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][2, "hi_sev"]))

rdnbr_labs <- c(bquote(R^2 ~ "(k-fold)" ~ "=" ~ .(round(cbi_rdnbr$r2_kfold, 5))),
                bquote("RdNBR = " ~ beta[0] ~ "+" ~ beta[1] ~ "*" ~ e^beta[2] ~ "*cbi"),
                bquote(beta[0] ~ "=" ~ .(round(cbi_rdnbr$a, 5))),
                bquote(beta[1] ~ "=" ~ .(round(cbi_rdnbr$b, 5))),
                bquote(beta[2] ~ "=" ~ .(round(cbi_rdnbr$c, 5))))

lapply(seq_along(rdnbr_labs),
       FUN = function(i) {
         max_val <- max(cbi_mask_conifer(cbi_32_bilinear)$RdNBR, na.rm = TRUE)
         min_val <- min(cbi_mask_conifer(cbi_32_bilinear)$RdNBR, na.rm = TRUE)
         
         text(x = 0, y = max_val - (max_val - min_val) * pos_in_panel_text[i], 
              labels = rdnbr_labs[[i]],
              pos = 4, 
              cex = cex_in_panel_text) 
         invisible()})


# Third panel: RdNDVI 48 day window ---------------------------------------

plot(x = cbi_mask_conifer(cbi_48_bilinear)$cbi_over, y = cbi_mask_conifer(cbi_48_bilinear)$RdNDVI, 
     pch = 19, col = pch_color, xlab = NA, ylab = NA, las = 1, main = "RdNDVI (48-day window)")
lines(cbi_over, yhat_rdndvi)
# segments(x0 = 0, x1 = 3, y0 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][3, "hi_sev"]), y1 = pull(cbi_table[order(cbi_table$r2_kfold, decreasing = TRUE), ][3, "hi_sev"]))

rdndvi_labs <- list(bquote(R^2 ~ "(k-fold)" ~ "=" ~ .(round(cbi_rdndvi$r2_kfold, 5))),
          bquote("RdNDVI = " ~ beta[0] ~ "+" ~ beta[1] ~ "*" ~ e^beta[2] ~ "*cbi"),
          bquote(beta[0] ~ "=" ~ .(round(cbi_rdndvi$a, 5))),
          bquote(beta[1] ~ "=" ~ .(round(cbi_rdndvi$b, 5))),
          bquote(beta[2] ~ "=" ~ .(round(cbi_rdndvi$c, 5))))

lapply(seq_along(rdndvi_labs),
       FUN = function(i) {
         max_val <- max(cbi_mask_conifer(cbi_48_bilinear)$RdNDVI, na.rm = TRUE)
         min_val <- min(cbi_mask_conifer(cbi_48_bilinear)$RdNDVI, na.rm = TRUE)
         
         text(x = 0, y = max_val - (max_val - min_val) * pos_in_panel_text[i], 
              labels = rdndvi_labs[[i]],
              pos = 4, 
              cex = cex_in_panel_text)
       invisible()})

mtext(text = "Composite Burn Index (CBI)", side = 1, outer = TRUE, line = 0)
mtext(text = "Remotely measured severity", side = 2, outer = TRUE, line = 0)
dev.off()