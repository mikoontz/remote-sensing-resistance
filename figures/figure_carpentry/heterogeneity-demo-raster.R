# Generate example figure showing low and high heterogeneity in raster version

library(raster)

not_center_pixel <- function(r) {
  cell_index <- rep(TRUE, ncell(r))
  cell_index[ceiling(ncell(r) / 2)] <- FALSE
  return(cell_index)
}

make_r_hom <- function(radius = 1, mean_ndvi = 0.6) {
  radius <- radius + 0.5
  r_side_length <- 2 * radius
  r_homogenous <- raster(nrows = r_side_length, ncols = r_side_length, xmn = -radius, xmx = radius, ymn = -radius, ymx = radius)
  r_not_center <- not_center_pixel(r_homogenous)
  r_homogenous[r_not_center] <- mean_ndvi
  
  return(r_homogenous)  
}


make_r_het <- function(radius = 1, mean_ndvi = 0.6, sd_ndvi_heterogenous = 0.5, seed = 127, tolerance = 0.01) {
  radius <- radius + 0.5
  r_side_length <- 2 * radius
  
  r_heterogenous <- raster(nrows = r_side_length, ncols = r_side_length, xmn = -radius, xmx = radius, ymn = -radius, ymx = radius)
  r_not_center <- not_center_pixel(r_heterogenous)
  
  set.seed(seed)
  
  r_heterogenous_vals <- rnorm(ncell(r_heterogenous) - 1, 
                               mean = mean_ndvi, sd = sd_ndvi_heterogenous)
  while(any(r_heterogenous_vals > 1) | 
        any(r_heterogenous_vals < 0) | 
        (mean(r_heterogenous_vals, na.rm = TRUE) > (mean_ndvi + tolerance) &
         mean(r_heterogenous_vals, na.rm = TRUE) < (mean_ndvi - tolerance))) {
    r_heterogenous_vals <- rnorm(ncell(r_heterogenous) - 1, 
                                 mean = mean_ndvi, 
                                 sd = sd_ndvi_heterogenous)
  }
  
  r_heterogenous[r_not_center] <- r_heterogenous_vals
  
  return(r_heterogenous)
}

mean_ndvi <- 0.6
sd_ndvi_heterogenous <- 0.2

r1_homogenous <- make_r_hom(radius = 1, mean_ndvi = mean_ndvi)
r1_heterogenous <- make_r_het(radius = 1, mean_ndvi = mean_ndvi, sd_ndvi_heterogenous = sd_ndvi_heterogenous, tolerance = 0.01)

r2_homogenous <- make_r_hom(radius = 2, mean_ndvi = mean_ndvi)
r2_heterogenous <- make_r_het(radius = 2, mean_ndvi = mean_ndvi, sd_ndvi_heterogenous = sd_ndvi_heterogenous, tolerance = 0.02)

r3_homogenous <- make_r_hom(radius = 3, mean_ndvi = mean_ndvi)
r3_heterogenous <- make_r_het(radius = 3, mean_ndvi = mean_ndvi, sd_ndvi_heterogenous = sd_ndvi_heterogenous, tolerance = 0.03)

r4_homogenous <- make_r_hom(radius = 4, mean_ndvi = mean_ndvi)
r4_heterogenous <- make_r_het(radius = 4, mean_ndvi = mean_ndvi, sd_ndvi_heterogenous = sd_ndvi_heterogenous, tolerance = 0.04)


color_fnc <- colorRampPalette(colors = c("white", "forestgreen"))

#### These plots will zoom to fit each plot window.

par(mfrow = c(2, 4))
n_colors_hom <- 3
n_colors_het <- 1000

plot(r1_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))

plot(r2_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))

plot(r3_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))

plot(r4_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))


# Heterogenous plots

plot(r1_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))

plot(r2_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))

plot(r3_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))

plot(r4_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))

#### End "fit to plot window" version

#### Start "keep a constant resolution" version

r_template <- make_r_hom(radius = 4, mean_ndvi = NA)

r1_homogenous <- mosaic(r1_homogenous, r_template, fun = sum)
r1_heterogenous <- mosaic(r1_heterogenous, r_template, fun = sum)

r2_homogenous <- mosaic(r2_homogenous, r_template, fun = sum)
r2_heterogenous <- mosaic(r2_heterogenous, r_template, fun = sum)

r3_homogenous <- mosaic(r3_homogenous, r_template, fun = sum)
r3_heterogenous <- mosaic(r3_heterogenous, r_template, fun = sum)

r4_homogenous <- mosaic(r4_homogenous, r_template, fun = sum)
r4_heterogenous <- mosaic(r4_heterogenous, r_template, fun = sum)

pdf(here::here("figures/heterogeneity-demo-raster.pdf"), width = 8, height = 4)
par(mfrow = c(2, 4), mar = c(0.5, 0.5, 0.5, 0.5), oma = c(3, 4, 0, 0))
n_colors_hom <- 3
n_colors_het <- 1000
box_width <- 5

plot(r1_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1), xlim = c(-4.5, 4.5))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)
mtext(side = 2, text = "Homogenous\nneighborhood", line = 1)

plot(r2_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)

plot(r3_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)

plot(r4_homogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_hom),
     breaks = seq(0, 1, length.out = n_colors_hom + 1))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)


# Heterogenous plots

plot(r1_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)
mtext(side = 2, text = "Heterogenous\nneighborhood", line = 1)
mtext(side = 1, text = "90m x 90m\n0.81ha", line = 2)

plot(r2_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)
mtext(side = 1, text = "150m x 150m\n2.25ha", line = 2)

plot(r3_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)
mtext(side = 1, text = "210m x 210m\n4.41ha", line = 2)

plot(r4_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", 
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))
rect(xleft = -4.5, xright = 4.5, ybottom = -4.5, ytop = 4.5, lwd = box_width)
mtext(side = 1, text = "270m x 270m\n7.29ha", line = 2)
dev.off()