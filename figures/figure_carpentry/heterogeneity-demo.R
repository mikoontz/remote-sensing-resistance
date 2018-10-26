library(png)
library(sf)
library(dplyr)
library(viridis)
library(here)

# Where were some of the highest and lowest heterogeneity samples?
s <- readRDS(here::here("data/data_output/all-fire-samples_configured.rds"))
d <- st_read(here::here("data/data_output/heterogeneity-demo.geojson"))

sup1 <- readPNG(here::here("data/heterogeneity-demo/suppressed_1-pixel.png"))
sup2 <- readPNG(here::here("data/heterogeneity-demo/suppressed_2-pixel.png"))
sup3 <- readPNG(here::here("data/heterogeneity-demo/suppressed_3-pixel.png"))
sup4 <- readPNG(here::here("data/heterogeneity-demo/suppressed_4-pixel.png"))
man1 <- readPNG(here::here("data/heterogeneity-demo/fire-use_1-pixel.png"))
man2 <- readPNG(here::here("data/heterogeneity-demo/fire-use_2-pixel.png"))
man3 <- readPNG(here::here("data/heterogeneity-demo/fire-use_3-pixel.png"))
man4 <- readPNG(here::here("data/heterogeneity-demo/fire-use_4-pixel.png"))

img_mar <- c(0.5, 0.5, 0.5, 0.5)
histo_mar <- c(4, 3, 0.5, 0)
box_width <- 4
het_measure <- "het_ndwi"

# pdf(here::here("figures/heterogeneity-imagery-histograms.pdf"), width = 17.3 / 2.54, height = 0.75 * 17.3 / 2.54)
par(mfrow = c(3, 4), mar = img_mar, xaxs = 'i', yaxs='i', oma = c(2.5, 2.5, 2.5, 2.5))
my_colors <- viridis(2)

#### Plot fire use images 
plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(man1, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[1])
mtext(c("90m x 90m", "150m x 150m", "210m x 210m", "270m x 270m"), 
      side = 3, outer = TRUE, at = c(1/8, 3/8, 5/8, 7/8))

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(man2, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[1])

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(man3, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[1])

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(man4, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[1])

#### Plot suppressed images ####

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(sup1, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[2])

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(sup2, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[2])

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(sup3, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[2])

plot(x = 0, y = 0, xlim = c(0, 10), ylim = c(0, 10), type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
rasterImage(sup4, xleft = 0, ybottom = 0, xright = 10, ytop = 10)
box(which = "plot", lwd = box_width, col = my_colors[2])

#### Plot histograms ####
par(mar = histo_mar)

for (i in 1:4) {
  radius <- i
  plot(density(s[, paste0(het_measure, "_", radius), drop = TRUE]), main = NA, xaxt = "n", las = 1, xlab = NA, ylab = NA)
  
  axis(side = 1, 
       at = d[, paste0(het_measure, "_", radius), drop = TRUE], 
       labels = round(d[, paste0(het_measure, "_", radius), drop = TRUE], 2), las = 2, font = 2)
  
  axis(side = 1, 
       at = c(0, 
              max(s[, paste0(het_measure, "_", radius), drop = TRUE] / 2),
              max(s[, paste0(het_measure, "_", radius), drop = TRUE])),
       labels = c(0, 
                  round(max(s[, paste0(het_measure, "_", radius), drop = TRUE] / 2), 2), 
                  round(max(s[, paste0(het_measure, "_", radius), drop = TRUE]), 2)), 
       las = 2)
  
  abline(v = d[d$type == "fire_use", paste0(het_measure, "_", radius)], 
         col = my_colors[1], 
         lwd = 2)
  
  abline(v = d[d$type == "suppressed", paste0(het_measure, "_", radius)], 
         col = my_colors[2], 
         lwd = 2)
  
}


legend(x = "topright", 
       legend = c("Groupy-gappy forest", "Dense, homogenous forest"), 
       col = my_colors, 
       lty = 1, 
       lwd = 3, 
       bty = "n")

mtext(side = 2, 
      text = "Density", 
      outer = TRUE, 
      at = 1/6, 
      cex = 1)

mtext(side = 1, 
      text = het_measure, 
      outer = TRUE, 
      line = 0, 
      cex = 1)

# dev.off()