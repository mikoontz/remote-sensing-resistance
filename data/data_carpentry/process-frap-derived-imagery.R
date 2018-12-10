# Script to rename bands of the Python raster exports
library(raster)
library(rasterVis)

band_names <- c('rdnbr', 'nbr', 'nbr_1', 'rdndvi', 'rbr', 'ndvi', 'ndvi_1', 'het_ndvi_1', 'nbhd_ndvi_1', 'het_ndvi_2', 'nbhd_ndvi_2', 'het_ndvi_3', 'nbhd_ndvi_3', 'het_ndvi_4', 'nbhd_ndvi_4', 'constant', 'constant_1', 'constant_2', 'constant_3', 'constant_4', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_pre', 'B2_pre', 'B3_pre', 'B4_pre', 'B5_pre', 'B6_pre', 'B7_pre', 'B1_post', 'B2_post', 'B3_post', 'B4_post', 'B5_post', 'B6_post', 'B7_post', 'preFerc', 'preFfm100', 'preFvpd', 'earlyFvs', 'earlyFhdw')

fire <- raster::stack("data/ee_fire-images/19880704_00017_0000395195b1272a72a0_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870829_00059_0000de101e16afb630c0_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870830_00045_0000bafc4812e81fdc1f_epsg3310.tif") # Hamm Fire
fire <- raster::stack("data/ee_fire-images/19870830_00177_0000f8d62e28cf5fa43c_epsg3310.tif")
fire <- raster::stack("data/ee_fire-images/19870830_00421_00003a70ebf6fedca59e_epsg3310.tif")

names(fire) <- band_names
# Overall RBR values from the model
burned_unburned_rbr_threshold <- 0.041191844
high_sev_low_sev_rbr_threshold <- 0.2836425

# A severity palette
RBR_viz <- c('#008000', '#ffff00', '#ffA500', '#ff0000')

breaks <- c(cellStats(fire[["rbr"]], min), seq(burned_unburned_rbr_threshold, high_sev_low_sev_rbr_threshold, length.out = 100), cellStats(fire[["rbr"]], max))
# cols <- colorRampPalette(c('#008000', RBR_viz, '#ff0000'))
cols <- colorRampPalette(RBR_viz)
levelplot(fire[["rbr"]], at = breaks, col.regions = cols)

breaks <- c(cellStats(fire[["rbr"]], min), seq(burned_unburned_rbr_threshold, cellStats(fire[["rbr"]], max), length.out = 100))
cols <- colorRampPalette(RBR_viz)

levelplot(fire[["rbr"]], at = breaks, col.regions = cols)
