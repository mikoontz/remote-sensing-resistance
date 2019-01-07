# Purpose: figure to show conceptual model of "hole-in-the-forest" versus "isolated vegetation patch" decoupling
# of central pixel NDVI and neighborhood mean NDVI, both scenarios of which lead to higher-than-expected probabilities
# of high severity fire

# PNAS one-column figure: 8.7cm
# PNAS >one-column figure: 11.4 or 17.8cm
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

r1_hole_in_forest_heterogenous <- make_r_het(radius = 1, mean_ndvi = 0.8, sd_ndvi_heterogenous = 0.2, tolerance = 0.01)
r1_isolated_veg_patch_heterogenous <- make_r_het(radius = 1, mean_ndvi = 0.1, sd_ndvi_heterogenous = 0.2, tolerance = 0.01)
r1_isolated_veg_patch_heterogenous[2,2] <- 1

color_fnc <- colorRampPalette(colors = c("white", "forestgreen"))

#### These plots will zoom to fit each plot window.
png(here::here("figures/decoupling-center-neighborhood-ndvi.png"), width = 8.7, height = 8.7 / 2, res = 300, units = "in")
par(mfrow = c(1, 2), mar = c(0, 0, 0.5, 0), oma = c(0, 2, 0, 0))

n_colors_hom <- 3
n_colors_het <- 1000
box_width <- 5

plot(r1_hole_in_forest_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n", col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het))
rect(xleft = -1.5, xright = 1.5, ybottom = -1.5, ytop = 1.5, lwd = box_width)
mtext(side = 3, text = "\"Hole in the forest\"", line = -0.5)

plot(r1_isolated_veg_patch_heterogenous, asp = 1, axes = FALSE, box = FALSE, legend = FALSE, bty = "n",
     col = color_fnc(n_colors_het),
     breaks = seq(0, 1, length.out = n_colors_het),
     interpolate = FALSE)
rect(xleft = -1.5, xright = 1.5, ybottom = -1.5, ytop = 1.5, lwd = box_width)
mtext(side = 3, text = "\"Isolated patch\"", line = -0.5)

dev.off()


plot(r1_hole_in_forest_heterogenous)

forest_hole_ggplot <-
  ggplot(r1_hole_in_forest_heterogenous %>% as.data.frame(xy = TRUE), aes(x = x, y = y)) +
  geom_raster(aes(fill = layer), show.legend = FALSE) +
  coord_equal() +
  scale_fill_gradient(low = "#FFFFFF", high = "#228B22", na.value = "#FFFFFF", limits = c(0, 1)) +
  theme_void() +
  ggtitle("'Hole in the forest'") +
  theme(plot.title = element_text(hjust = 0.5))

isolated_patch_ggplot <-
  ggplot(r1_isolated_veg_patch_heterogenous %>% as.data.frame(xy = TRUE), aes(x = x, y = y)) +
  geom_raster(aes(fill = layer), show.legend = FALSE) +
  coord_equal() +
  scale_fill_gradient(low = "#FFFFFF", high = "#228B22", na.value = "#FFFFFF", limits = c(0, 1)) +
  theme_void() +
  ggtitle("'Isolated patch'") +
  theme(plot.title = element_text(hjust = 0.5))

panel_plot <- plot_grid(forest_hole_ggplot, isolated_patch_ggplot, ncol = 2, nrow = 1)

ggsave(panel_plot, filename = here::here("figures/decoupling-center-neighborhood-ndvi.png"), width = 8.7, height = 8.7 / 2, units = "cm")
