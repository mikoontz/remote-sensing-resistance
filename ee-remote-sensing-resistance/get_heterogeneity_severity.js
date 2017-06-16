/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var elev = ee.Image("USGS/SRTMGL1_003"),
    l5SR = ee.ImageCollection("LANDSAT/LT5_SR"),
    mixed_conifer = ee.Image("users/mkoontz/mixed_conifer"),
    perim = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    sn = ee.FeatureCollection("ft:1vdDUTu09Rkw5qKR_DSfmFX-b_7kqy4E-pjxg9Sq6");
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
  var mask = img.select(['cfmask']).eq(0);
  return(img.updateMask(mask));
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

//
// START create_kernel
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

//
// END create_kernel
//

//
// START getSamps
//

// This function samples points from within an image at a specified scale and density
var getSamps = function(img) {
  
  img = ee.Image(img);
  var s = img.sample({
          scale: 30,
          factor: s_factor,
          seed: 727
  });

  return s;
};

//
// END getSamps
//

//
// START get_NDVI
//

var get_NDVI = function(img) {
  var ndvi = img.normalizedDifference(['B4', 'B3']);
  return ndvi;
};

//
// END get_NDVI
//

//
// START get_NDWI
//

var get_NDWI = function(img) {
  var ndwi = img.normalizedDifference(['B4', 'B5']);
  return ndwi;
};

//
// END get_NDWI
//

//
// START get_EVI
//
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

  return evi;
};

//
// END get_EVI
//

// 
// START get_NBR
//

var get_NBR = function(img) {
  var nbr = img.normalizedDifference(['B4', 'B7']);
  return nbr;
};

//
// END get_NBR
//

//
// START get_NBR2
//

var get_NBR2 = function(img) {
  var nbr2 = img.normalizedDifference(['B5', 'B7']);
  return nbr2;
};

//
// END get_NBR2
//

//
// DEPENDENT/RESPONSE VARIABLES
// Calculation of dependent variables for the remote-sensing-resistance
// study. Calculation of each response variable is broken into two
// separate functions so that we can check to make sure the pre-fire
// and post-fire image collections exist prior to trying to do 
// calculations on them. The "_internal" function does the calculations,
// and the "get_" function checks the imagery. Without the checks, errors
// are returned when trying to use the .normalizedDifference() method on
// non-existant imagery.
//
// Variables of interest: RdNBR, RdNBR2, RdNDVI, RdEVI
//

// 
// START RdNBR
//

// For calcuations, see Miller and Thode (2007)
var rdnbr_internal = function(preFire, postFire) {
  var preFire_nbr = get_NBR(preFire);
  var postFire_nbr = get_NBR(postFire);

  var denom_temporary = preFire_nbr.abs().divide(1000);
  var denom = denom_temporary.sqrt();
   
  var delta_nbr = preFire_nbr.subtract(postFire_nbr);
  
  var RdNBR = delta_nbr.divide(denom);
  
  return ee.Image(RdNBR);
};

// Check prefire and postfire imagery. This lets us drop those fire
// perimeters from the later stage data collection by letting the mapped
// function on my collection of perimeters dropNulls.
var get_RdNBR = function(feature) {
  
  var preFire = get_preFireRaw(feature).median();
  var postFire = get_postFireRaw(feature).median();

  var RdNBR = ee.Algorithms.If( preFire.bandNames(),
                                ee.Algorithms.If( postFire.bandNames(), 
                                                  rdnbr_internal(preFire, postFire).clip(feature), 
                                                  null),
                                null);

  return ee.Image(RdNBR);
};

//
// END RdNBR
//

//
// START RdNBR2
//

// Same math as in Miller and Thode (2007), but using NBR2 instead of NBR
var rdnbr2_internal = function(preFire, postFire) {
  
  var preFire_nbr2 = get_NBR2(preFire);
  var postFire_nbr2 = get_NBR2(postFire);

  var denom_temporary = preFire_nbr2.abs().divide(1000);
  var denom = denom_temporary.sqrt();
   
  var delta_nbr2 = preFire_nbr2.subtract(postFire_nbr2);
  
  var RdNBR2 = delta_nbr2.divide(denom);
  
  return ee.Image(RdNBR2);
};

// Check prefire and postfire imagery. This lets us drop those fire
// perimeters from the later stage data collection by letting the mapped
// function on my collection of perimeters dropNulls.
var get_RdNBR2 = function(feature) {
  
  var preFire = get_preFireRaw(feature).median();
  var postFire = get_postFireRaw(feature).median();

  var RdNBR2 = ee.Algorithms.If( preFire.bandNames(),
                                ee.Algorithms.If( postFire.bandNames(), 
                                                  rdnbr2_internal(preFire, postFire).clip(feature), 
                                                  null),
                                null);

  return ee.Image(RdNBR2);
};

//
// END RdNBR2
//

//
// START RdNDVI
//

// Same math as in Miller and Thode (2007), but using NDVI instead of NBR
var rdndvi_internal = function(preFire, postFire) {
  
  var preFire_ndvi = get_NDVI(preFire);
  var postFire_ndvi = get_NDVI(postFire);

  var denom_temporary = preFire_ndvi.abs().divide(1000);
  var denom = denom_temporary.sqrt();
   
  var delta_ndvi = preFire_ndvi.subtract(postFire_ndvi);
  
  var RdNDVI = delta_ndvi.divide(denom);
  
  return ee.Image(RdNDVI);
};

// Check prefire and postfire imagery. This lets us drop those fire
// perimeters from the later stage data collection by letting the mapped
// function on my collection of perimeters dropNulls.
var get_RdNDVI = function(feature) {
  
  var preFire = get_preFireRaw(feature).median();
  var postFire = get_postFireRaw(feature).median();

  var RdNDVI = ee.Algorithms.If( preFire.bandNames(),
                                ee.Algorithms.If( postFire.bandNames(), 
                                                  rdndvi_internal(preFire, postFire).clip(feature), 
                                                  null),
                                null);

  return ee.Image(RdNDVI);
};

//
// END RdNDVI
//

//
// START RdEVI
//

// Same math as in Miller and Thode (2007), but using EVI instead of NBR
var rdevi_internal = function(preFire, postFire) {
  
  var preFire_evi = get_EVI(preFire);
  var postFire_evi = get_EVI(postFire);

  var denom_temporary = preFire_evi.abs().divide(1000);
  var denom = denom_temporary.sqrt();
   
  var delta_evi = preFire_evi.subtract(postFire_evi);
  
  var RdEVI = delta_evi.divide(denom);
  
  return ee.Image(RdEVI);
};

// Check prefire and postfire imagery. This lets us drop those fire
// perimeters from the later stage data collection by letting the mapped
// function on my collection of perimeters dropNulls.
var get_RdEVI = function(feature) {
  
  var preFire = get_preFireRaw(feature).median();
  var postFire = get_postFireRaw(feature).median();

  var RdEVI = ee.Algorithms.If( preFire.bandNames(),
                                ee.Algorithms.If( postFire.bandNames(), 
                                                  rdevi_internal(preFire, postFire).clip(feature), 
                                                  null),
                                null);

  return ee.Image(RdEVI);
};

//
// END RdEVI
//

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

//
// START Heterogeneity of NDVI
//

var heterogeneityNDVI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var het = preFire.map(function (img) {
          var ndvi = get_NDVI(img);
          
          return ndvi.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
        });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var het_img = het.median();

  return ee.Image(het_img);
};

// Heterogeneity of NDVI function that checks pre and postfire imagery
var get_hetNDVI = function(feature, pixel_radius) {
  var preFireCol = get_preFireRaw(feature);

  var het = ee.Algorithms.If( preFireCol.median().bandNames(),
                              heterogeneityNDVI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(het);
};

//
// END Heterogeneity of NDVI
//

//
// START Heterogeneity of NDWI
//

var heterogeneityNDWI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var het = preFire.map(function (img) {
          var ndwi = get_NDWI(img);
          
          return ndwi.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
        });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var het_img = het.median();

  return ee.Image(het_img);
};

// Heterogeneity of NDWI function that checks pre and postfire imagery
var get_hetNDWI = function(feature, pixel_radius) {
  var preFireCol = get_preFireRaw(feature);

  var het = ee.Algorithms.If( preFireCol.median().bandNames(),
                              heterogeneityNDWI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(het);
};

//
// END Heterogeneity of NDWI
//

//
// START Heterogeneity of EVI
//

var heterogeneityEVI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var het = preFire.map(function (img) {
    
      var evi = get_EVI(img);

      return evi.reduceNeighborhood(ee.Reducer.stdDev(), kernel);
    });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var het_img = het.median();

  return ee.Image(het_img);
};

// Heterogeneity of EVI function that checks pre and post fire imagery
var get_hetEVI = function(feature, pixel_radius) 
{
  var preFireCol = get_preFireRaw(feature);

  var het = ee.Algorithms.If( preFireCol.median().bandNames(),
                              heterogeneityEVI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(het);
};

//
// END Heterogeneity of EVI
//


//
// FOCAL MEAN COVARIATES
//

//
// START Focal mean of NDVI
//

var focal_mean_NDVI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var focal_mean = preFire.map(function (img) {
          var ndvi = get_NDVI(img);
          
          return ndvi.reduceNeighborhood(ee.Reducer.mean(), kernel);
        });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var focal_mean_img = focal_mean.median();

  return ee.Image(focal_mean_img);
};

// Heterogeneity of NDVI function that checks pre and postfire imagery
var get_focal_mean_NDVI = function(feature, pixel_radius) {
  var preFireCol = get_preFireRaw(feature);

  var focal_mean = ee.Algorithms.If( preFireCol.median().bandNames(),
                              focal_mean_NDVI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(focal_mean);
};

//
// END Heterogeneity of NDVI
//

//
// START Heterogeneity of NDWI
//

var focal_mean_NDWI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var focal_mean = preFire.map(function (img) {
          var ndwi = get_NDWI(img);
          
          return ndwi.reduceNeighborhood(ee.Reducer.mean(), kernel);
        });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var focal_mean_img = focal_mean.median();

  return ee.Image(focal_mean_img);
};

// Heterogeneity of NDWI function that checks pre and postfire imagery
var get_focal_mean_NDWI = function(feature, pixel_radius) 
{
  var preFireCol = get_preFireRaw(feature);

  var focal_mean = ee.Algorithms.If( preFireCol.median().bandNames(),
                              focal_mean_NDWI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(focal_mean);
};

//
// END Heterogeneity of NDWI
//

//
// START Focal mean of EVI
//

var focal_mean_EVI_internal = function(preFire, pixel_radius) {
  
  var kernel = create_kernel(pixel_radius);

  // Make each image from preFire collection into its NDWI image,
  // then calculate heterogeneity for each pixel
  var focal_mean = preFire.map(function (img) {
      var evi = get_EVI(img);

      return evi.reduceNeighborhood(ee.Reducer.mean(), kernel);
    });
  
  // Overall heterogeneity is the median of the heterogeneity metrics
  // calculated using the collection of prefire images
  var focal_mean_img = focal_mean.median();

  return ee.Image(focal_mean_img);
};

// Heterogeneity of EVI function that checks pre and post fire imagery
var get_focal_mean_EVI = function(feature, pixel_radius) {
  var preFireCol = get_preFireRaw(feature);

  var focal_mean = ee.Algorithms.If( preFireCol.median().bandNames(),
                              focal_mean_EVI_internal(preFireCol, pixel_radius).clip(feature),
                              null);

  return ee.Image(focal_mean);
};

//
// END Heterogeneity of EVI
//

//
// MEDIAN VALUE OF EACH PIXEL COVARIATES
//

//
// START median_NDVI
//

var median_ndvi_internal = function(preFire) {
  // Make each image from preFire collection into its NDVI image
  var ndvi = preFire.map(get_NDVI);
  var ndvi_img = ndvi.median();

  return ee.Image(ndvi_img);
};

// Checks prefire imagery
var get_median_NDVI = function(feature) {
  var preFireCol = get_preFireRaw(feature);

  var ndvi = ee.Algorithms.If( preFireCol.median().bandNames(),
                              median_ndvi_internal(preFireCol).clip(feature),
                              null);


  return ee.Image(ndvi);
};

//
// END get_median_ndvi function
//
//

//
// START median NDWI
// 
var median_ndwi_internal = function(preFire) {
  // Make each image from preFire collection into its NDVI image
  var ndwi = preFire.map(get_NDWI);
  var ndwi_img = ndwi.median();

  return ee.Image(ndwi_img);
};

//  NDWI function. 
// Returns the NDWI of each pixel
var get_median_NDWI = function(feature) {
  var preFireCol = get_preFireRaw(feature);

  var ndwi = ee.Algorithms.If( preFireCol.median().bandNames(),
                              median_ndwi_internal(preFireCol).clip(feature),
                              null);


  return ee.Image(ndwi);
};

//
// END get_median_ndwi function
//

//
// START median EVI
// 
var median_evi_internal = function(preFire) {
  // Make each image from preFire collection into its NDVI image
  var evi = preFire.map(get_EVI);
  var evi_img = evi.median();

  return ee.Image(evi_img);
};

//  EVI function. 
// Returns the EVI of each pixel
var get_median_EVI = function(feature) {
  var preFireCol = get_preFireRaw(feature);

  var evi = ee.Algorithms.If( preFireCol.median().bandNames(),
                              median_evi_internal(preFireCol).clip(feature),
                              null);


  return ee.Image(evi);
};

//
// END get_median_evi function
//

var getSlope = function(feature) {
  var terrain = ee.Algorithms.Terrain(elev);
  var slope = terrain.select('slope');
  return(slope.clip(feature));
};

var getAspect = function(feature) {
  var terrain = ee.Algorithms.Terrain(elev);
  var aspect = terrain.select('aspect');
  return(aspect.clip(feature));
};

// Start getVariables function
var getVariables = function(feature) {   
    var geo = feature.geometry();
    // Static features of this particular dataset
    var satellite = ee.Image(sat).clip(geo);
    
    // Static features of the point itself
    var lonLat = ee.Image.pixelLonLat().clip(geo);
    var slope = getSlope(geo);
    var aspect = getAspect(geo);
    var local_elev = elev.clip(geo);

    // Not dependent on neighborhood size, but derived from the fire information
    var date = ee.Image(
      ee.Number(
        feature.get('alarm_date')))
      .clip(geo);
    
    var julian_day = ee.Image(
      ee.Number(
        ee.Date(
          feature
          .get('alarm_date'))
        .getRelative('day', 'year')))
      .clip(geo);
      
    var rdnbr = get_RdNBR(feature);
    var rdnbr2 = get_RdNBR2(feature);
    var rdndvi = get_RdNDVI(feature);
    var rdevi = get_RdEVI(feature);
    
    var median_ndvi = get_median_NDVI(feature);
    var median_ndwi = get_median_NDWI(feature);
    var median_evi = get_median_EVI(feature);
    
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

    // Create export image
    // If the rdnbr variable isn't null, then all other images should have been
    // created, since the rdnbr algorithm checks both prefire and postfire imagery
    // and returns a null if either aren't present
  
    var export_img =   
      ee.Algorithms.If(rdnbr, 
        rdnbr
        .addBands(rdnbr2)
        .addBands(rdndvi)
        .addBands(rdevi)
        .addBands(median_ndvi)
        .addBands(median_ndwi)
        .addBands(median_evi)
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
        .addBands(julian_day)
        .addBands(lonLat)
        .addBands(slope)
        .addBands(aspect)
        .addBands(local_elev)
        .rename(['RdNBR', 
          'RdNBR2',
          'RdNDVI',
          'RdEVI',
          'median_ndvi',
          'median_ndwi',
          'median_evi',
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
          'julian_day', 
          'lon', 
          'lat', 
          'slope', 
          'aspect',
          'elev']),
      null);
    
  
    return ee.Image(export_img);
};



// Start of main program

// DEFINE ALL GLOBAL VARIABLES HERE
// Define which Landsat dataset to use for all calcuations
var raw = l5SR;

// The satellite being used
var sat = 5;

// Define the window of time (in months) that we will use to gather
// images prior to the fire. For example, if the window is 3 months,
// and the fire burned on June 2, 2010, we will collect all Landsat
// images taken between March 1, 2010 and June 1, 2010 to 
// calculate pre-fire metrics and all Landsat images between March 1,
// 2011 and June 1, 2011 to calculate post-fire metrics

var timeWindow = 2;

// Define the date range of the analysis. Sticking to only fires that would
// use the Landsat 5 imagery until I can determine a good relationship between
// Landsat 5 and Landsat 8

var start = ee.Date('1984-04-01').millis();
var end = ee.Date('2011-05-04').millis();

// var start = ee.Date('2010-04-01').millis();
// var end = ee.Date('2011-05-04').millis();

// Sampling factor determines how much to sample from each image
var s_factor = 0.001;

// Filter entire collection of mapped fire perimenters to just the
// fires that started during the Landsat 5 period and those with an
// area greater than 900 m^2 (one Landsat pixel)
// We know we'll need some imagery a few months before each fire
// and a year after each fire, so we limit our fire selection to 
// those that burned starting a few months after Landsat 5 imagery
// begins and a year before Landsat 5 imagery ends

var l5perims =
  perim
    .filterMetadata("alarm_date", "greater_than", start)
    .filterMetadata("alarm_date", "less_than", end)
    .filterMetadata("shape_area", "greater_than", 900)
    .filterBounds(sn);
    
// var l5perims = l5perims.filterMetadata('fire_name', 'equals', 'HAMM');

// // Map the variable retrieval function over all of the features
var imgCol = ee.FeatureCollection(l5perims.map(getVariables, true));

// Mask nonForested pixels based on the Presettlement Fire Regimes from FRID database
imgCol = imgCol.map(maskNonForest);

// // Visualize a test image
// var RdNBR_viz = {min:0, max:32, palette:['008000', 'ffff00',  'ffA500', 'ff0000']};
// var test_img = ee.Image(imgCol.first());

// Map.addLayer(test_img.select(['RdEVI']), RdNBR_viz, 'rdnbr');
// Map.centerObject(test_img);

// Extract samples
// Sample from the images containing all response and predictor values
var samps = imgCol.map(getSamps).flatten();

// File name for .csv in a string
// var fileName = 'L5_1987HAMMCMPLX_remote_resistance_0_001_percent';
var fileName = 'L5_19840401_20110504_remote_resistance_0_001_percent';

// Export the tidy .csv to Google Drive
Export.table.toDrive({
  'collection': samps,
  'description': fileName,
  'folder': 'ee',
  'fileNamePrefix': fileName,
  'fileFormat': 'CSV'
});




// var getVariablesL5 = function(feature)
// {
//   var export_img = ee.Algorithms.If(feature.geometry().type() != 'Multipoint',
//                                     getVariablesL5_internal(feature),
//                                     null
//                                     );
  
//   return (ee.Image(export_img));
  
// };