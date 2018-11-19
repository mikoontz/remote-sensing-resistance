# Script to rename bands of the Python raster exports
library(raster)
library(rasterVis)

band_names <- c('rdnbr', 'nbr', 'nbr_1', 'rdndvi', 'rbr', 'ndvi', 'ndvi_1', 'het_ndvi_1', 'nbhd_ndvi_1', 'het_ndvi_2', 'nbhd_ndvi_2', 'het_ndvi_3', 'nbhd_ndvi_3', 'het_ndvi_4', 'nbhd_ndvi_4', 'constant', 'constant_1', 'constant_2', 'constant_3', 'constant_4', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_pre', 'B2_pre', 'B3_pre', 'B4_pre', 'B5_pre', 'B6_pre', 'B7_pre', 'B1_post', 'B2_post', 'B3_post', 'B4_post', 'B5_post', 'B6_post', 'B7_post', 'preFerc', 'preFfm100', 'preFvpd', 'earlyFvs', 'earlyFhdw')

fire1 <- raster::stack("data/ee_fire-images/19880704_00017_0000395195b1272a72a0_epsg3310.tif")

names(fire1) <- band_names
# Overall RBR values from the model
min_rbr <- 0.041191844
max_rbr <- 0.2836425
# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

# rbr_stats <- cellStats(fire1[["rbr"]], range)
breaks <- seq(min_rbr, max_rbr, length.out = 100)
cols <- colorRampPalette(RBR_viz)(length(breaks)-1)

levelplot(fire1[["rbr"]], col.regions = cols)
