library(raster)
library(dplyr)
library(sf)
library(fasterize)
library(rgdal)

r <- raster("data/features/sierra_nevada_30m_landsat_template.tif")
sn <- st_read("data/data_output/SierraEcoregion_Jepson/") %>%
  st_transform(proj4string(r))

nsn <- st_read("data/features/fire_return_interval_departure/FRID_NorthSierra15_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

ssn <- st_read("data/features/fire_return_interval_departure/FRID_SouthSierra15_1.gdb/") %>%
  st_transform(proj4string(r)) %>%
  filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

# Directly combine the vector shapes into a single, large vector shape
sn_v <- rbind(nsn, ssn)
st_write(obj = sn_v, dsn = "data/data_output/landcover_PFR/mixed_conifer/mixed_conifer.shp", driver = "ESRI Shapefile")
# Successful write; some warnings about truncated column names and truncated
# values in the shape_area column (we can always recalculate these)
# This shapefile was uploaded to Google Earth Engine and is available here:
#


# Use fasterize to turn the vector shapes into rasters
nsn_r <- fasterize(sf = nsn, raster = r)
ssn_r <- fasterize(sf = ssn, raster = r)

sn_r <- raster::merge(nsn_r, ssn_r)

plot(sn_r)
plot(sn$geometry, add = TRUE)

writeRaster(x = sn_r, filename = "data/data_output/landcover_PFR/mixed_conifer.tif")

# Create a masked version of the conifer_mask (clipped to the Sierra Nevada geometry)
# Good for visualization only, as it isn't the same resolution of the Landsat data
plot(sn_v)

r_masked <- raster(sn, resolution = res(r) * 10)
nsn_r_masked <- fasterize(sf = nsn, raster = r_masked)
ssn_r_masked <- fasterize(sf = ssn, raster = r_masked)
sn_r_masked <- merge(nsn_r_masked, ssn_r_masked)
plot(sn_r_masked)

writeRaster(x = sn_r_masked, filename = "data/data_output/landcover_PFR/mixed_conifer_sn-mask_10-fold-res.tif")

# Turn raster image of mixed conifer into a more simple multipolygon
# Code from https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

ypmc <- raster("data/data_output/landcover_PFR/mixed_conifer.tif")
# This process takes ages, so comment out the polygonizing code!
# ypmc_vector <- gdal_polygonizeR(ypmc, readpoly = FALSE)

###

# Find temporary file written to disk and rename it, essentially. These lines won't work in the future.
# ypmc_vector <- st_read("data/data_output/landcover_PFR/mixed_conifer_simple/file571241732d7a.shp")
# plot(ypmc_vector$geometry)
# st_write(obj = ypmc_vector, dsn = "data/data_output/landcover_PFR/mixed_conifer_simple_vector", driver = "ESRI Shapefile")
