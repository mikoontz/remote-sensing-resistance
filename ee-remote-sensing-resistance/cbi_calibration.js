/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var cbi_sn = ee.FeatureCollection("users/mkoontz/cbi_sn"),
    perim = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    sn = ee.FeatureCollection("ft:1vdDUTu09Rkw5qKR_DSfmFX-b_7kqy4E-pjxg9Sq6"),
    mixed_conifer = ee.Image("users/mkoontz/mixed_conifer"),
    elev = ee.Image("USGS/SRTMGL1_003"),
    gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET"),
    l5sr = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR"),
    l7sr = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR"),
    l8sr = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
//
// HELPER FUNCTIONS
// Generally for gathering and masking imagery, and not for
// calculating modeling variables
//

//
// START maskClouds
//

// This function masks any clouds in the images by only using
// pixels that have a 0 in the cfmask band of the Surface Reflectance
// product, which indicates no cloud cover for that pixel

var maskClouds = function(img)
{
  // Each pixel gets a 1 if it is NOT a cloud, and a 0 if it IS a cloud
  var cloudMask = img.select('pixel_qa').bitwiseAnd(32).eq(0);
  // Each pixel gets a 1 if it is NOT a cloud shadow, and a 0 if it IS a cloud shadow
  var cloudShadowMask = img.select('pixel_qa').bitwiseAnd(8).eq(0);
  // For the combined mask (1 is a pixel we want to keep, and a 0 is one we want to mask),
  // the pixel must both NOT be a cloud AND NOT be a cloud shadow (a 1 for both of the
  // above masks) in order to be a valid pixel.
  var mask = cloudMask.and(cloudShadowMask);
  
  // Return an interpolated image with all cloud and cloud shadow pixels masked.
  // Use interpolation because CBI on-the-ground plots are unlikely to
  // lie exactly at the center of a pixel. See Cansler MSc thesis (2011)
  // and Parks et al. (2014)
  return(img.resample(resample_method).updateMask(mask)); 
};

//
// END maskClouds
//

//
// START maskNonForest
//

// Mask out all nonforest pixels from an image based "Presettlement Fire Regime"
// designations which represent what vegetation could be there and is less
// sensitive to recent disturbances that might turn a conifer forest into a
// bare patch temporarily even though that area should generally be thought of
// as a conifer forest

var maskNonForest = function(img) {
  img = ee.Image(img);
  
  var coverMask = mixed_conifer.select('b1');
  var export_img = img.updateMask(coverMask);
    
  return export_img;
};

//
// END maskNonForest
//

//
// START get_preFireRaw
//

// This function will take in a feature (which has the fire perimeter
// and the fire date) and will return the collection of pre-fire images
// to use for calculating all pre-fire metrics

var get_preFireRaw = function(feature) {
  
  var fireDate = ee.Date(feature.get('alarm_date'));
  var firePerim = feature.geometry();

  // Prefire image collection derivecd by gathering all images "timeWindow"
  // (a global variable) months before the fire. 
  // These variables define the time period to grab those images. 
  var prestart = fireDate.advance(timeWindow * -1, 'month');
  var preend = fireDate.advance(-1, 'day');

  // Here is where we subset the Landsat imagery. We filter the whole collection
  // to just the images that were taken between "timeWindow" months before the 
  // fire started and 1 day before the fire started.
  var preFireCollection = 
    raw
      .filterDate(prestart, preend)
      .filterBounds(firePerim);
  
  // We apply the cloud mask over each of those images
  var preFire = preFireCollection.map(maskClouds);

  // Return the preFire image collection
  return ee.ImageCollection(preFire);
};

//
// END get_preFireRaw
//

//
// START get_postFireRaw
//

// Returns a post fire raw image collection. This time we look 1 year after the 
// prefire image collection.
var get_postFireRaw = function(feature) {
  
  var fireDate = ee.Date(feature.get('alarm_date'));
  var firePerim = feature.geometry();
  
  // Need to get pre fire reference date in order to advance to 1 year later
  var prestart = fireDate.advance(timeWindow * -1, 'month');
  var preend = fireDate.advance(-1, 'day');

  // Post fire image comes from median of images exactly 1 year after pre-fire images
  var poststart = prestart.advance(1, 'year');
  var postend = preend.advance(1, 'year');
  
  // Subset Landsat imagery;  We filter the whole collection
  // to just the images that were taken *one year after* between 
  // "timeWindow" months before the fire started and *one year
  // after* 1 day before the fire started.
  var postFireCollection = 
    raw
      .filterDate(poststart, postend)
      .filterBounds(firePerim);
 
   // We apply the cloud mask over each of those images
  var postFire = 
    postFireCollection
      .map(maskClouds);

  return ee.ImageCollection(postFire);
};

// get_preFireGridmet() returns a collection of raw daily GRIDMET images for gridmet_timeWindow number
// of days before the fire. This collection can then be used to calculate ERC just before the fire
// and temperature/precipitation accumulation for a bit longer before the fire
var get_preFireGridmet = function(feature, gridmet_timeWindow) {
  
  var fireDate = ee.Date(feature.get('alarm_date'));
  var firePerim = feature.geometry();

  // Prefire image collection derivecd by gathering all images "timeWindow"
  // (a global variable) months before the fire. 
  // These variables define the time period to grab those images. 
  var prestart = fireDate.advance(gridmet_timeWindow * -1, 'day');
  var preend = fireDate.advance(-1, 'day');

  // Here is where we subset the Landsat imagery. We filter the whole collection
  // to just the images that were taken between "timeWindow" months before the 
  // fire started and 1 day before the fire started.
  var preFireGridmetCol = 
    gridmet
      .filterDate(prestart, preend)
      .filterBounds(firePerim);
  
  // Return the preFire image collection
  return ee.ImageCollection(preFireGridmetCol);
};


// create_kernel() returns an equally-weighted square ee.kernel with the specified number of pixel radius
//
// Create a kernel of a given pixel radius (number of concentric rings around focal pixel)
// Importantly, we also give 0 weight to the focal pixel.
// A radius of 1 pixel yields a 3x3 pixel kernel with a weight of 0 at the center pixel and
// a weight of 1/8 at every other pixel
// A radius of 2 pixels yields a 5x5 pixel kernel with a weight of 0 at the center pixel and
// a weight of 1/24 at every other pixel
var create_kernel = function(pixel_radius) {
  var pixel_diameter = 2 * pixel_radius + 1;
  var weight_val = 1 / ((pixel_diameter * pixel_diameter) - 1);
  var weights = ee.List.repeat(ee.List.repeat(weight_val, pixel_diameter), pixel_diameter);
  
  var mid_row = ee.List.repeat(weight_val, pixel_radius)
    .cat([0])
    .cat(ee.List.repeat(weight_val, pixel_radius));
  
  weights = weights.set(pixel_radius, mid_row);

  var kernel = ee.Kernel.fixed({
    height: pixel_diameter,
    width: pixel_diameter,
    weights: weights
  });
  
  return kernel;
};

// get_samps() samples points from within an image at a specified scale and density
var get_samps = function(img) {
  
  img = ee.Image(img);
  var s = img.sample({
          scale: 30,
          // factor: s_factor,
          numPixels: 100,
          seed: 727,
          dropNulls: true
  });

  return s;
};

//
// END get_samps
//

///////////////////
/* NDVI */
///////////////////

// get_NDVI() returns the normalized difference vegetation index (NDVI) for each pixel of an image
var get_NDVI = function(img) {
  var ndvi = img.normalizedDifference(['B4', 'B3']);
  
  return ee.Image(ndvi);
};

// get_preFndvi() maps over the collection of pre-fire images, calculates NDVI on each, and takes the median for each pixel
var get_preFndvi = function(feature) {
  
  var preFndvi = get_preFireRaw(feature).map(get_NDVI).median();
  
  preFndvi = ee.Algorithms.If( preFndvi.bandNames(),
                                    ee.Image(preFndvi), 
                                    null);

  return ee.Image(preFndvi);
};

// get_postFndvi() maps over the collection of post-fire images, calculates NDVI on each, and takes the median for each pixel
var get_postFndvi = function(feature) {
  
  var postFndvi = get_postFireRaw(feature).map(get_NDVI).median();
  
  postFndvi = ee.Algorithms.If( postFndvi.bandNames(),
                                    ee.Image(postFndvi), 
                                    null);

  return ee.Image(postFndvi);
};

/////////////////////
/* NDWI */
/////////////////////

// get_NDWI() returns the normalized difference water index (NDWI) for each pixel of an image
var get_NDWI = function(img) {
  var ndwi = img.normalizedDifference(['B4', 'B5']);
  
  return ee.Image(ndwi);
};

// get_preFndwi() maps over the collection of pre-fire images, calculates NDWI on each, and takes the median for each pixel
var get_preFndwi = function(feature) {
  
  var preFndwi = get_preFireRaw(feature).map(get_NDWI).median();
  
  preFndwi = ee.Algorithms.If( preFndwi.bandNames(),
                                    ee.Image(preFndwi), 
                                    null);

  return ee.Image(preFndwi);
};

// get_postFndwi() maps over the collection of post-fire images, calculates NDWI on each, and takes the median for each pixel
var get_postFndwi = function(feature) {
  
  var postFndwi = get_postFireRaw(feature).map(get_NDWI).median();
  
  postFndwi = ee.Algorithms.If( postFndwi.bandNames(),
                                    ee.Image(postFndwi), 
                                    null);

  return ee.Image(postFndwi);
};

/////////////////////
/* EVI */
/////////////////////

// get_EVI() returns the enhanced vegetation index (EVI) at each pixel of an image
var get_EVI = function (img) {
   
  var evi_numerator = 
    img.select('B4')
      .subtract(img.select('B3'));
    
  var evi_denominator = 
    img.select('B4')
      .add(img.select('B3').multiply(6))
      .subtract(img.select('B1').multiply(7.5))
      .add(1);
  
  var evi = 
    evi_numerator
      .divide(evi_denominator)
      .multiply(2.5);

  return ee.Image(evi);
};

// get_preFevi() maps over the collection of pre-fire images, calculates EVI on each, and takes the median for each pixel
var get_preFevi = function(feature) {
  
  var preFevi = get_preFireRaw(feature).map(get_EVI).median();
  
  preFevi = ee.Algorithms.If( preFevi.bandNames(),
                                    ee.Image(preFevi), 
                                    null);

  return ee.Image(preFevi);
};

// get_postFevi() maps over the collection of post-fire images, calculates EVI on each, and takes the median for each pixel
var get_postFevi = function(feature) {
  
  var postFevi = get_postFireRaw(feature).map(get_EVI).median();
  
  postFevi = ee.Algorithms.If( postFevi.bandNames(),
                                    ee.Image(postFevi), 
                                    null);

  return ee.Image(postFevi);
};

///////////////////
/* NBR */
///////////////////

// get_NBR() returns the normalized burn ratio (NBR) for each pixel of an image
var get_NBR = function(img) {
  var nbr = img.normalizedDifference(['B4', 'B7']);
  
  return ee.Image(nbr);
};

// get_preFnbr() maps over the collection of pre-fire images, calculates NBR on each, and takes the median for each pixel
var get_preFnbr = function(feature) {
  
  var preFnbr = get_preFireRaw(feature).map(get_NBR).median();
  
  preFnbr = ee.Algorithms.If( preFnbr.bandNames(),
                                    ee.Image(preFnbr), 
                                    null);

  return ee.Image(preFnbr);
};

// get_postFnbr() maps over the collection of post-fire images, calculates NBR on each, and takes the median for each pixel
var get_postFnbr = function(feature) {
  
  var postFnbr = get_postFireRaw(feature).map(get_NBR).median();
  
  postFnbr = ee.Algorithms.If( postFnbr.bandNames(),
                                    ee.Image(postFnbr), 
                                    null);

  return ee.Image(postFnbr);
};

////////////////////
/* NBR2 */
////////////////////

//  get_NBR2() returns the normalized burn ratio 2 (NBR2) value at each pixel for an image
var get_NBR2 = function(img) {
  var nbr2 = img.normalizedDifference(['B5', 'B7']);
  
  return ee.Image(nbr2);
};

// get_preFnbr2() maps over the collection of pre-fire images, calculates NBR2 on each, and takes the median for each pixel
var get_preFnbr2 = function(feature) {
  
  var preFnbr2 = get_preFireRaw(feature).map(get_NBR2).median();
  
  preFnbr2 = ee.Algorithms.If( preFnbr2.bandNames(),
                                  ee.Image(preFnbr2), 
                                  null);

  return ee.Image(preFnbr2);
};

// get_postFnbr2() maps over the collection of post-fire images, calculates NBR2 on each, and takes the median for each pixel
var get_postFnbr2 = function(feature) {
  
  var postFnbr2 = get_postFireRaw(feature).map(get_NBR2).median();
  
  postFnbr2 = ee.Algorithms.If( postFnbr2.bandNames(),
                                    ee.Image(postFnbr2), 
                                    null);

  return ee.Image(postFnbr2);
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////
//  END HELPER FUNCTIONS    /////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////

//
// DEPENDENT/RESPONSE VARIABLES
// Calculation of dependent variables for the remote-sensing-resistance
// study. 
//
// Variables of interest: dNBR, RdNBR, dNBR2, RdNBR2, dNDVI, RdNDVI, dEVI, RdEVI
//

// get_dNBR() returns the difference in NBR between just before the fire and 1 year later
var get_dNBR = function(feature) {
  
  var preFire_nbr = get_preFnbr(feature);
  var postFire_nbr = get_postFnbr(feature);

  var dNBR = ee.Algorithms.If( preFire_nbr,
                                ee.Algorithms.If( postFire_nbr, 
                                                  preFire_nbr.subtract(postFire_nbr), 
                                                  null),
                                null);

  return ee.Image(dNBR);
};

// get_RdNBR() returns the relative differenced normalized burn ratio for each pixel within the fire perimeter
// For calcuations, see Miller and Thode (2007)

var get_RdNBR = function(feature) {
  
  var preFire_nbr = get_preFnbr(feature);
  var delta_nbr = get_dNBR(feature);
  
  var RdNBR = ee.Algorithms.If( delta_nbr,
                                  delta_nbr.divide((preFire_nbr.abs().divide(1000)).sqrt()), 
                                  null);

  return ee.Image(RdNBR);
};

// get_dNBR2() returns the raw difference in NBR2 values between pre- and post-fire images
var get_dNBR2 = function(feature) {
  
  var preFire_nbr2 = get_preFnbr2(feature);
  var postFire_nbr2 = get_postFnbr2(feature);

  var dNBR2 = ee.Algorithms.If( preFire_nbr2,
                                ee.Algorithms.If( postFire_nbr2, 
                                                  preFire_nbr2.subtract(postFire_nbr2), 
                                                  null),
                                null);

  return ee.Image(dNBR2);
};

// get_RdNBR2() returns the relative differenced normalized burn ratio 2 for each pixel within a fire perimeter
// Same calculations as in Miller and Thode (2007), but using NBR2 instead of NBR
var get_RdNBR2 = function(feature) {
  
  var preFire_nbr2 = get_preFnbr2(feature);
  var delta_nbr2 = get_dNBR2(feature);
  
  var RdNBR2 = ee.Algorithms.If( delta_nbr2,
                                  delta_nbr2.divide((preFire_nbr2.abs().divide(1000)).sqrt()), 
                                  null);

  return ee.Image(RdNBR2);
};

// get_dNDVI() returns the raw difference in NDVI between pre- and post-fire images
var get_dNDVI = function(feature) {
  
  var preFire_ndvi = get_preFndvi(feature);
  var postFire_ndvi = get_postFndvi(feature);
  
  var dNDVI = ee.Algorithms.If( preFire_ndvi.bandNames(),
                                ee.Algorithms.If( postFire_ndvi.bandNames(), 
                                                  preFire_ndvi.subtract(postFire_ndvi), 
                                                  null),
                                null);

  return ee.Image(dNDVI);
};

// get_RdNDVI() returns the relative differenced normalized difference vegetation index for each pixel within a fire perimeter
// Same math as in Miller and Thode (2007), but using NDVI instead of NBR
var get_RdNDVI = function(feature) {
  
  var preFire_ndvi = get_preFndvi(feature);
  var delta_ndvi = get_dNDVI(feature);
  
  var RdNDVI = ee.Algorithms.If( delta_ndvi,
                                  delta_ndvi.divide((preFire_ndvi.abs().divide(1000)).sqrt()), 
                                  null);

  return ee.Image(RdNDVI);
};

// get_dEVI() returns the raw difference in EVI values between pre- and post-fire images
var get_dEVI = function(feature) {
  
  var preFire_evi = get_preFevi(feature);
  var postFire_evi = get_postFevi(feature);
  
  var dEVI = ee.Algorithms.If( preFire_evi.bandNames(),
                                ee.Algorithms.If( postFire_evi.bandNames(), 
                                                  preFire_evi.subtract(postFire_evi), 
                                                  null),
                                null);

  return ee.Image(dEVI);
};

// get_RdEVI() returns the relative differenced enhanced vegetation index
// Same math as in Miller and Thode (2007), but using EVI instead of NBR
var get_RdEVI = function(feature) {
  
  var preFire_evi = get_preFevi(feature);
  var delta_evi = get_dEVI(feature);
  
  var RdEVI = ee.Algorithms.If( delta_evi,
                                  delta_evi.divide((preFire_evi.abs().divide(1000)).sqrt()), 
                                  null);
                                  
  return ee.Image(RdEVI);
};

// get_RBR() returns the relative burn ratio from Parks et al. 2015. Remote Sensing of the Environment))
var get_RBR = function(feature) {
  
  var preFire_nbr = get_preFnbr(feature);
  var delta_nbr = get_dNBR(feature);
  
  var RBR = ee.Algorithms.If( delta_nbr,
                                delta_nbr.divide(preFire_nbr.add(1.001)), 
                                null);

  return ee.Image(RBR);
};

//
// INDEPENDENT/PREDICTOR VARIABLES
// Calculation of independent variables for the remote-sensing-resistance
// study. Calculation of each predictor variable is broken into two
// separate functions (similar to the framework for response variables) 
// so that we can check to make sure the pre-fire
// and post-fire image collections exist prior to trying to do 
// calculations on them. The "_internal" function does the calculations,
// and the "get_" function checks the imagery. Without the checks, errors
// are returned when trying to use the .normalizedDifference() method on
// non-existant imagery.
//
// Variables of interest: 
// + Neighborhood window sizes of 3, 5, 7, and 9 pixels in diameter
// + Focal NDVI, EVI, NDWI
// + Neighborhood NDVI, EVI, NDWI

//
// HETEROGENEITY COVARIATES
//

/* Heterogeneity of NDVI */

// get_hetNDVI() returns the heterogeneity of NDVI within a given pixel radius for each pixel in an image
var get_hetNDVI = function(feature, pixel_radius) {
  var kernel = create_kernel(pixel_radius);
  var preFireCol_ndvi = get_preFireRaw(feature).map(get_NDVI);
  
  var het = preFireCol_ndvi.map(function (img) {
          return img.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
        });
        
 het = ee.Algorithms.If( het.median().bandNames(),
                              het.median(),
                              null);

  return ee.Image(het);
};

// get_hetNDWI() returns the heterogeneity of NDWI within a given pixel radius for each pixel in an image
var get_hetNDWI = function(feature, pixel_radius) {
  var kernel = create_kernel(pixel_radius);
  var preFireCol_ndwi = get_preFireRaw(feature).map(get_NDWI);

  var het = preFireCol_ndwi.map(function (img) {
          return img.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
        });
  
  het = ee.Algorithms.If( het.median().bandNames(),
                              het.median(),
                              null);

  return ee.Image(het);
};

// get_hetEVI() returns the heterogeneity of EVI within a given pixel radius for each pixel in an image
var get_hetEVI = function(feature, pixel_radius) 
{
  var kernel = create_kernel(pixel_radius);
  var preFireCol_evi = get_preFireRaw(feature).map(get_EVI);

    var het = preFireCol_evi.map(function (img) {
      return img.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
    });

  het = ee.Algorithms.If( het.median().bandNames(),
                              het.median(),
                              null);

  return ee.Image(het);
};

//
// FOCAL MEAN COVARIATES
//

// get_focal_mean_NDVI() returns the neighborhood mean of the NDVI for a given pixel radius
// Could be valuable to account for this if using the neighborhood standard deviation at the same pixel radius
var get_focal_mean_NDVI = function(feature, pixel_radius) {
  var kernel = create_kernel(pixel_radius);
  var preFireCol_ndvi = get_preFireRaw(feature).map(get_NDVI);

  var focal_mean = preFireCol_ndvi.map(function (img) {
          return img.reduceNeighborhood(ee.Reducer.mean(), kernel);
        });
  
  focal_mean = ee.Algorithms.If( focal_mean.median().bandNames(),
                                  focal_mean.median(),
                                  null);

  return ee.Image(focal_mean);
};

// get_focal_mean_NDWI() returns the neighborhood mean of the NDWI for a given pixel radius
// Could be valuable to account for this if using the neighborhood standard deviation at the same pixel radius
var get_focal_mean_NDWI = function(feature, pixel_radius) 
{
  var kernel = create_kernel(pixel_radius);
  var preFireCol_ndwi = get_preFireRaw(feature).map(get_NDWI);

  var focal_mean = preFireCol_ndwi.map(function (img) {
          return img.reduceNeighborhood(ee.Reducer.mean(), kernel);
        });

  focal_mean = ee.Algorithms.If(focal_mean.median().bandNames(),
                                focal_mean.median(),
                                null);

  return ee.Image(focal_mean);
};

// get_focal_mean_EVI() returns the neighborhood mean of the EVI for a given pixel radius
// Could be valuable to account for this if using the neighborhood standard deviation at the same pixel radius
var get_focal_mean_EVI = function(feature, pixel_radius) {
  var kernel = create_kernel(pixel_radius);
  var preFireCol_evi = get_preFireRaw(feature).map(get_EVI);

  var focal_mean = preFireCol_evi.map(function (img) {
      return img.reduceNeighborhood(ee.Reducer.mean(), kernel);
    });
  
  focal_mean = ee.Algorithms.If(focal_mean.median().bandNames(),
                              focal_mean.median(),
                              null);

  return ee.Image(focal_mean);
};


// Topographic characteristics

var get_slope = function(feature) {
  var terrain = ee.Algorithms.Terrain(elev);
  var slope = terrain.select('slope');
  return(slope);
};

var get_aspect = function(feature) {
  var terrain = ee.Algorithms.Terrain(elev);
  var aspect = terrain.select('aspect');
  return(aspect);
};

// Topographic roughness values
// get_hetEVI() returns the heterogeneity of EVI within a given pixel radius for each pixel in an image
var get_roughness = function(feature, pixel_radius) 
{
  var kernel = create_kernel(pixel_radius);
  
  var roughness = elev
    .resample(resample_method)
    .reduceNeighborhood(ee.Reducer.stdDev(), kernel);
  
  roughness = ee.Algorithms.If( roughness.bandNames(),
                                roughness,
                                null);

  return ee.Image(roughness);
};

// // Weather/fuel condition variables
var get_erc = function(img) {
  var erc = ee.Image(img.select(['erc'])).resample(resample_method);
  
  return ee.Image(erc);
};

var get_preFerc = function(feature, gridmet_timeWindow) {
  var erc = get_preFireGridmet(feature, gridmet_timeWindow).map(get_erc).median();
  
  erc = ee.Algorithms.If( erc.bandNames(),
                                    ee.Image(erc), 
                                    null);

  return ee.Image(erc);
};

var get_fm100 = function(img) {
  var fm100 = ee.Image(img.select(['fm100'])).resample(resample_method);
  
  return ee.Image(fm100);
};

var get_preFfm100 = function(feature, gridmet_timeWindow) {
  var fm100 = get_preFireGridmet(feature, gridmet_timeWindow).map(get_fm100).median();
  
  fm100 = ee.Algorithms.If( fm100.bandNames(),
                                    ee.Image(fm100), 
                                    null);

  return ee.Image(fm100);
};

var get_tempMax = function(img) {
  var tempMax = ee.Image(img.select(['tmmx'])).subtract(273.15).resample(resample_method);
  
  return ee.Image(tempMax);
};

var get_preFcumulativeTempMax = function(feature, gridmet_timeWindow) {
  var cumulativeTempMax = get_preFireGridmet(feature, gridmet_timeWindow).map(get_tempMax).sum();
  
  cumulativeTempMax = ee.Algorithms.If( cumulativeTempMax.bandNames(),
                                            ee.Image(cumulativeTempMax),
                                            null);
  return ee.Image(cumulativeTempMax);
};

var get_precip = function(img) {
  var precip = ee.Image(img.select(['pr'])).resample(resample_method);
  
  return ee.Image(precip);
};

var get_preFcumulativePrecip = function(feature, gridmet_timeWindow) {
  var cumulativePrecip = get_preFireGridmet(feature, gridmet_timeWindow).map(get_precip).sum();
  
  cumulativePrecip = ee.Algorithms.If( cumulativePrecip.bandNames(),
                                            ee.Image(cumulativePrecip),
                                            null);
  return ee.Image(cumulativePrecip);
};



//
// Start get_variables function
//

var get_variables = function(feature) {   
    var geo = feature.geometry();
    // Static features of this particular dataset
    var satellite = ee.Image(sat);
    
    // Static features of the point itself
    var lonLat = ee.Image.pixelLonLat();
    var slope = get_slope(geo).resample(resample_method);
    var aspect = get_aspect(geo).resample(resample_method);
    var local_elev = elev.resample(resample_method);
    var conifer = mixed_conifer.select('b1');
    var rough1 = get_roughness(feature, 1);
    var rough2 = get_roughness(feature, 2);
    var rough3 = get_roughness(feature, 3);
    var rough4 = get_roughness(feature, 4);
    
    // Not dependent on neighborhood size, but derived from the fire information
    var date = ee.Image(
      ee.Number(
        feature.get('alarm_date')));
    
    var ordinal_day = ee.Image(
      ee.Number(
        ee.Date(
          feature
          .get('alarm_date'))
        .getRelative('day', 'year')));
    
    var preFnbr = get_preFnbr(feature);
    var postFnbr = get_postFnbr(feature);
    var dnbr = get_dNBR(feature);
    var rdnbr = get_RdNBR(feature);
    
    var preFnbr2 = get_preFnbr2(feature);
    var postFnbr2 = get_postFnbr2(feature);
    var dnbr2 = get_dNBR2(feature);
    var rdnbr2 = get_RdNBR2(feature);
    
    var preFndvi = get_preFndvi(feature);
    var postFndvi = get_postFndvi(feature);
    var dndvi = get_dNDVI(feature);
    var rdndvi = get_RdNDVI(feature);
    
    var preFevi = get_preFevi(feature);
    var postFevi = get_postFevi(feature);
    var devi = get_dEVI(feature);
    var rdevi = get_RdEVI(feature);
    
    var rbr = get_RBR(feature);
    
    var preFndwi = get_preFndwi(feature);
    var postFndwi = get_postFndwi(feature);

    // Variables that depend on neighborhood window size AND on fire information
    // Radius of 1 pixel = 3x3 window = 90m x 90m = 8100 m^2 = 0.81 ha
    var het_ndvi_1 = get_hetNDVI(feature, 1);
    var het_ndwi_1 = get_hetNDWI(feature, 1);
    var het_evi_1 = get_hetEVI(feature, 1);
    
    var focal_mean_ndvi_1 = get_focal_mean_NDVI(feature, 1);
    var focal_mean_ndwi_1 = get_focal_mean_NDWI(feature, 1);
    var focal_mean_evi_1 = get_focal_mean_EVI(feature, 1);
    
    // Radius of 2 pixels = 5x5 window = 150m x 150m = 22500 m^2 = 2.25 ha
    var het_ndvi_2 = get_hetNDVI(feature, 2);
    var het_ndwi_2 = get_hetNDWI(feature, 2);
    var het_evi_2 = get_hetEVI(feature, 2);
    
    var focal_mean_ndvi_2 = get_focal_mean_NDVI(feature, 2);
    var focal_mean_ndwi_2 = get_focal_mean_NDWI(feature, 2);
    var focal_mean_evi_2 = get_focal_mean_EVI(feature, 2);
    
    // Radius of 3 pixels = 7x7 window = 210m x 210m = 44100 m^2 = 4.41 ha
    var het_ndvi_3 = get_hetNDVI(feature, 3);
    var het_ndwi_3 = get_hetNDWI(feature, 3);
    var het_evi_3 = get_hetEVI(feature, 3);
    
    var focal_mean_ndvi_3 = get_focal_mean_NDVI(feature, 3);
    var focal_mean_ndwi_3 = get_focal_mean_NDWI(feature, 3);
    var focal_mean_evi_3 = get_focal_mean_EVI(feature, 3);

    // Radius of 4 pixels = 9x9 window = 270m x 270m = 72900 m^2 = 7.29 ha
    var het_ndvi_4 = get_hetNDVI(feature, 4);
    var het_ndwi_4 = get_hetNDWI(feature, 4);
    var het_evi_4 = get_hetEVI(feature, 4);
    
    var focal_mean_ndvi_4 = get_focal_mean_NDVI(feature, 4);
    var focal_mean_ndwi_4 = get_focal_mean_NDWI(feature, 4);
    var focal_mean_evi_4 = get_focal_mean_EVI(feature, 4);

    // weather/fuel condition variables
      
    var erc = get_preFerc(feature, 4); // Take the median ERC for the 3 days prior to the fire
    var fm100 = get_preFfm100(feature, 4); // Take the median 100 hour fuel moisture for 3 days prior to the fire
    var cumulativeTempMax = get_preFcumulativeTempMax(feature, 31); // Get sum of max temperature for 30 days before the fire (degrees C)
    var cumulativePrecip = get_preFcumulativePrecip(feature, 31); // Get sum of precip for 30 days before the fire (mm)
    
    var export_weatherFuel =
      ee.Algorithms.If(erc,
          erc
            .addBands(fm100)
            .addBands(cumulativeTempMax)
            .addBands(cumulativePrecip),
          null);

    // Create export image
    // If the rdnbr variable isn't null, then all other images should have been
    // created, since the rdnbr algorithm checks both prefire and postfire imagery
    // and returns a null if either aren't present
  
    var export_img =   
      ee.Algorithms.If(dnbr, 
        dnbr
        .addBands(preFnbr)
        .addBands(postFnbr)
        .addBands(rdnbr)
        .addBands(dnbr2)
        .addBands(preFnbr2)
        .addBands(postFnbr2)
        .addBands(rdnbr2)
        .addBands(dndvi)
        .addBands(rdndvi)
        .addBands(devi)
        .addBands(rdevi)
        .addBands(rbr)
        .addBands(preFndvi)
        .addBands(postFndvi)
        .addBands(preFndwi)
        .addBands(postFndwi)
        .addBands(preFevi)
        .addBands(postFevi)
        .addBands(het_ndvi_1)
        .addBands(het_ndwi_1)
        .addBands(het_evi_1)
        .addBands(focal_mean_ndvi_1)
        .addBands(focal_mean_ndwi_1)
        .addBands(focal_mean_evi_1)
        .addBands(het_ndvi_2)
        .addBands(het_ndwi_2)
        .addBands(het_evi_2)
        .addBands(focal_mean_ndvi_2)
        .addBands(focal_mean_ndwi_2)
        .addBands(focal_mean_evi_2)
        .addBands(het_ndvi_3)
        .addBands(het_ndwi_3)
        .addBands(het_evi_3)
        .addBands(focal_mean_ndvi_3)
        .addBands(focal_mean_ndwi_3)
        .addBands(focal_mean_evi_3)
        .addBands(het_ndvi_4)
        .addBands(het_ndwi_4)
        .addBands(het_evi_4)
        .addBands(focal_mean_ndvi_4)
        .addBands(focal_mean_ndwi_4)
        .addBands(focal_mean_evi_4)
        .addBands(satellite)
        .addBands(date)
        .addBands(ordinal_day)
        .addBands(lonLat)
        .addBands(conifer)
        .addBands(slope)
        .addBands(aspect)
        .addBands(rough1)
        .addBands(rough2)
        .addBands(rough3)
        .addBands(rough4)
        .addBands(local_elev)
        .rename(['dNBR',
          'preFire_nbr',
          'postFire_nbr',
          'RdNBR', 
          'dNBR2',
          'preFire_nbr2',
          'postFire_nbr2',
          'RdNBR2',
          'dNDVI',
          'RdNDVI',
          'dEVI',
          'RdEVI',
          'RBR',
          'preFire_ndvi',
          'postFire_ndvi',
          'preFire_ndwi',
          'postFire_ndwi',
          'preFire_evi',
          'postFire_evi',
          'het_ndvi_1',
          'het_ndwi_1',
          'het_evi_1',
          'focal_mean_ndvi_1',
          'focal_mean_ndwi_1',
          'focal_mean_evi_1',
          'het_ndvi_2',
          'het_ndwi_2',
          'het_evi_2',
          'focal_mean_ndvi_2',
          'focal_mean_ndwi_2',
          'focal_mean_evi_2',
          'het_ndvi_3',
          'het_ndwi_3',
          'het_evi_3',
          'focal_mean_ndvi_3',
          'focal_mean_ndwi_3',
          'focal_mean_evi_3',
          'het_ndvi_4',
          'het_ndwi_4',
          'het_evi_4',
          'focal_mean_ndvi_4',
          'focal_mean_ndwi_4',
          'focal_mean_evi_4',
          'satellite', 
          'date', 
          'ordinal_day', 
          'lon', 
          'lat', 
          'conifer_forest',
          'slope', 
          'aspect',
          'topo_roughness_1',
          'topo_roughness_2',
          'topo_roughness_3',
          'topo_roughness_4',
          'elev']),
      null);
    
    export_img = ee.Algorithms.If(export_img,
                    ee.Algorithms.If(export_weatherFuel,
                          ee.Image(export_img)
                            .addBands(export_weatherFuel)
                            .copyProperties(feature),
                          null),
                        null);

    return export_img;
};

var calibrate_cbi = function(feature) {
  
  var geo = feature.geometry();
  var var_img = get_variables(feature);

  var reduce_to_point_dict = ee.Algorithms.If(var_img,
                                      ee.Image(var_img)
                                          .reduceRegion({
                                             reducer: ee.Reducer.mean(),
                                             geometry: geo,
                                             scale: 30}),
                                      null);
                                      
  var reduce_to_point_ftr = ee.Algorithms.If(reduce_to_point_dict, 
                                      ee.Feature(geo, reduce_to_point_dict),
                                      null);

  var export_ftr = ee.Algorithms.If(reduce_to_point_ftr,
                                      feature.copyProperties(reduce_to_point_ftr),
                                      null);                                      

  return export_ftr;
  
};


var assess_whole_fire = function(feature) {
  
  var geo = feature.geometry();
  var var_img = get_variables(feature);
  
  var export_ftr = ee.Algorithms.If(var_img, 
                                      ee.Image(var_img).clip(geo).copyProperties(feature), 
                                      null);

  return export_ftr;
  
};

// Start of main program

// DEFINE ALL GLOBAL VARIABLES HERE
// Define which Landsat dataset to use for all calcuations
var raw = l5sr;

// The satellite being used
var sat = 5;

// Define the window of time (in months) that we will use to gather
// images prior to the fire. For example, if the window is 3 months,
// and the fire burned on June 2, 2010, we will collect all Landsat
// images taken between March 1, 2010 and June 1, 2010 to 
// calculate pre-fire metrics and all Landsat images between March 1,
// 2011 and June 1, 2011 to calculate post-fire metrics

var timeWindow = 1;
var resample_method = 'bicubic';

var target_cbi_plots = cbi_sn;

// Map the point reducer function (which also runs the variable retriever function)
// over all of the CBI point features; drop NULLs
var calibrated_cbi = target_cbi_plots.map(calibrate_cbi, true);

Map.addLayer(mixed_conifer);
Map.addLayer(target_cbi_plots, {color: "red"});

var cbi_calibration_description = "cbi-calibration_" + timeWindow + "-month-window_L5_" + resample_method + "-interp";

Export.table.toDrive({
  'collection': calibrated_cbi,
  'description': cbi_calibration_description,
  'folder': 'ee',
  'fileNamePrefix': cbi_calibration_description,
  'fileFormat': 'GeoJSON'
});

// var target_fires = perim;
// var assessed_fires = target_fires.map(assess_whole_fire, true);



// // Test mapping on a FeatureCollection that just contains one fire perimeter
var hamm = perim
            .filterMetadata('fire_name', 'equals', 'HAMM');
var hamm_assessed = hamm.map(assess_whole_fire, true);

var hamm_img = ee.Image(hamm.map(assess_whole_fire, true).first()).select(['RBR']);
var RBR_viz = {min: 0.036, max: 0.29, palette:['008000', 'ffff00',  'ffA500', 'ff0000']};
Map.addLayer(hamm_img, RBR_viz);
Map.centerObject(hamm_img);

var hamm_samps = hamm_assessed.map(get_samps, true);
print(hamm_samps.getInfo());


// // Test mapping on a FeatureCollection with many images.
// var start = ee.Date('1987-08-01').millis();
// var end = ee.Date('1987-09-01').millis();
// var test_ftr_col = perim
//                 .filterMetadata('alarm_date', 'greater_than', start)
//                 .filterMetadata('alarm_date', 'less_than', end);
// var test_fire_assessments = test_ftr_col.map(assess_whole_fire, true);



// Use this code for testing by subsetting some of the CBI data to a more manageable set
// var target_features =
//   cbi_sn
// //     .filterMetadata("alarm_date", "greater_than", start)
// //     .filterMetadata("alarm_date", "less_than", end)
//     .filterMetadata("fire_name", "equals", "Lost")
//     // .filterBounds(sn);

// Map.addLayer(target_features);
// Map.centerObject(target_features);
