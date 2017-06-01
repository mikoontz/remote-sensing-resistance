var elev = ee.Image("USGS/SRTMGL1_003"),
    l5raw = ee.ImageCollection("LANDSAT/LT5_L1T"),
    l5SR = ee.ImageCollection("LANDSAT/LT5_SR"),
    sn = ee.FeatureCollection("ft:1WMEQBWx7AjAfirtUM27nPUqnOas-jAnUGm1kEyL9"),
    NLCD = ee.ImageCollection("USGS/NLCD"),
    perim = ee.FeatureCollection("ft:1Pu1QahrbHUz3KRc87DmI55TSBdYC3qtmbsYfc4bk");
    
    // Original fusion table that might not be public.
// var perims2 = ee.FeatureCollection("ft:1ZfolTKW0AJWnXiM0wJJboYkGA8WCT40yqWbNxOj0")

// Same as fusion table in "Imports" screen above, but direct code saved for posterity
// var perim = ee.FeatureCollection("ft:1Pu1QahrbHUz3KRc87DmI55TSBdYC3qtmbsYfc4bk");

// This function masks any clouds in the images used by only using pixels that have a 0 in the cfmask band
var maskClouds = function(img)
{
  var mask = img.select(['cfmask']).eq(0);
  return(img.updateMask(mask));
};

// Mask out all nonforest pixels from an image based on the 1992
// National Landcover Database classification.

var maskNonForest = function(img)
{
  img = ee.Image(img);
  var geo = img.geometry();
  
  var land = ee.Image(NLCD.first()).select('landcover').clip(geo);
  var coverMask = land.gt(33).and(land.lt(61)).or(land.eq(71)).or(land.gte(91));

  // var noBurnMask = rdnbr.gt(0);
  var export_img = img
                   // .updateMask(noBurnMask)
                   .updateMask(coverMask);
    
  return export_img;
  
};

// This function will take in a feature (which has the fire perimeter and the fire date) and will
// return the pre-fire image to use for calculating RdNBR
var get_preFireRaw = function(feature)
{
  var fireDate = ee.Date(feature.get('fire_date'));
  var firePerim = feature.geometry();
  
  // This variable sets the number of months to look back before the fire started (it's negative 
  // for convenience when using the advance() function in the next line)
  var numImages = -4;
  
  // Prefire image comes from the median of each pixel within the fire of all images 3 months before
  // the fire. These variables define the time period to grab those images. 
  var prestart = fireDate.advance(numImages, 'month');
  var preend = fireDate.advance(-1, 'day');

 // Here is where we subset the raw Landsat imagery. We filter the whole collection to just the images
 // that were taken between 3 months before the fire started and 1 day before the fire started.
  var preFireCollection = raw.filterDate(prestart, preend)
                  .filterBounds(firePerim);
  
  // We apply the cloud mask over each of those images and then take the median
  var preFire = preFireCollection.map(maskClouds);

// Return the preFire image if the image exists, otherwise just return a blank object.
// preFire.bandNames() will be blank if there was no imagery in that time
  return ee.ImageCollection(preFire);
};

// Returns a post fire raw image collection. This time we look 1 year after the prefire image
var get_postFireRaw = function(feature)
{
  var fireDate = ee.Date(feature.get('fire_date'));
  var firePerim = feature.geometry();
  
  var numImages = -4;
  
  // Need to get pre fire reference date in order to advance to 1 year later
  var prestart = fireDate.advance(numImages, 'month');
  var preend = fireDate.advance(-1, 'day');

  // Post fire image comes from median of images exactly 1 year after pre-fire images
  var poststart = prestart.advance(1, 'year');
  var postend = preend.advance(1, 'year');
  
  var postFireCollection = raw.filterDate(poststart, postend)
                  .filterBounds(firePerim);
                  
  var postFire = postFireCollection.map(maskClouds);

  return ee.ImageCollection(postFire);
};

// This is how we calculate RdNBR using a prefire and a postfire image (see math in 
// Miller and Thode (2007). We only run the _internal methods if Landsat imagery is
// available. Otherwise errors are returned when trying to use the
// .normalizedDifference() method on non-existant imagery.
var rdnbr_internal = function(preFire, postFire)
{
  var preFire_nbr = preFire.normalizedDifference(['B4', 'B7']);
  var postFire_nbr = postFire.normalizedDifference(['B4', 'B7']);

  var denom_temporary = preFire_nbr.abs().divide(1000);
  var denom = denom_temporary.sqrt();
   
  var delta_nbr = preFire_nbr.subtract(postFire_nbr);
  
  var RdNBR = delta_nbr.divide(denom);
  
  return ee.Image(RdNBR);
};

// RdNBR function. This is how we can do a check on each pre and post fire image to make sure
// they exist before doing the actual operation on them.
// null is returned if imagery doesn't exist. This lets us drop those fire
// perimeters from the later stage data collection by letting the mapped
// function on my collection of perimeters dropNulls.
var get_RdNBR = function(feature)
{
  var preFire = get_preFireRaw(feature).median();
  var postFire = get_postFireRaw(feature).median();

  var RdNBR = ee.Algorithms.If( preFire.bandNames(),
                                ee.Algorithms.If( postFire.bandNames(), 
                                                  rdnbr_internal(preFire, postFire).clip(feature), 
                                                  null),
                                null);

  return ee.Image(RdNBR);
};
// end RdNBR function


// Heterogeneity in NDWI
// Get the PreFire forest heterogeneity as the variance in NDWI
var heterogeneity_internal = function(preF, kernel_radius)
{
  // Make each image from preFire collection into its NDWI image
  var het = preF.map(function (img) {
          var ndwi = ee.Image(img).normalizedDifference(['B4', 'B5']);
          
          return ndwi.reduceNeighborhood( ee.Reducer.stdDev(), 
                                          ee.Kernel.circle({radius:kernel_radius, units:'meters'}));
                                          
        });
  
  var het_img = het.median();

  return ee.Image(het_img);
};

// Heterogeneity of NDWI function. 
var get_het = function(feature, kernel_radius)
{
  var preFireCol = get_preFireRaw(feature);

  var het = ee.Algorithms.If( preFireCol.median().bandNames(),
                              heterogeneity_internal(preFireCol, kernel_radius).clip(feature),
                              null);

  return ee.Image(het);
};
// end get_het function
//
//


// Heterogeneity in NDVI
// Get the PreFire forest heterogeneity as the variance in NDVI
var heterogeneity_internal_ndvi = function(preF, kernel_radius)
{
  // Make each image from preFire collection into its NDVI image
  var het = preF.map(function (img) {
          var ndwi = ee.Image(img).normalizedDifference(['B4', 'B3']);
          
          return ndwi.reduceNeighborhood( ee.Reducer.stdDev(), 
                                          ee.Kernel.circle({radius:kernel_radius, units:'meters'}));
                                          
        });
  
  var het_img = het.median();

  return ee.Image(het_img);
};

// Heterogeneity of NDVI function. 
var get_het_ndvi = function(feature, kernel_radius)
{
  var preFireCol = get_preFireRaw(feature);

  var het = ee.Algorithms.If( preFireCol.median().bandNames(),
                              heterogeneity_internal_ndvi(preFireCol, kernel_radius).clip(feature),
                              null);


  return ee.Image(het);
};
// end get_het function
// 

// Focal mean of NDWI
// Get the PreFire forest neighborhood mean of NDWI
var focal_mean_internal = function(preF, kernel_radius)
{
  // Make each image from preFire collection into its NDWI image
  var focal_mean = preF.map(function (img) {
          var ndwi = ee.Image(img).normalizedDifference(['B4', 'B5']);
          
          return ndwi.reduceNeighborhood( ee.Reducer.mean(), 
                                          ee.Kernel.circle({radius:kernel_radius, units:'meters'}));
                                          
        });
  
  var focal_mean_img = focal_mean.median();

  return ee.Image(focal_mean_img);
};

// Focal mean of NDWI function. 
var get_focal_mean = function(feature, kernel_radius)
{
  var preFireCol = get_preFireRaw(feature);

  var focal_mean = ee.Algorithms.If( preFireCol.median().bandNames(),
                              focal_mean_internal(preFireCol, kernel_radius).clip(feature),
                              null);


  return ee.Image(focal_mean);
};
// end get_focal_mean function
//
//


// Focal mean of NDVI
// Get the PreFire forest heterogeneity as the variance in NDVI
var focal_mean_internal_ndvi = function(preF, kernel_radius)
{
  // Make each image from preFire collection into its NDVI image
  var focal_mean = preF.map(function (img) {
          var ndwi = ee.Image(img).normalizedDifference(['B4', 'B3']);
          
          return ndwi.reduceNeighborhood( ee.Reducer.mean(), 
                                          ee.Kernel.circle({radius:kernel_radius, units:'meters'}));
                                          
        });
  
  var focal_mean_img = focal_mean.median();

  return ee.Image(focal_mean_img);
};

// Focal mean of NDVI function. 
var get_focal_mean_ndvi = function(feature, kernel_radius)
{
  var preFireCol = get_preFireRaw(feature);

  var focal_mean = ee.Algorithms.If( preFireCol.median().bandNames(),
                              focal_mean_internal_ndvi(preFireCol, kernel_radius).clip(feature),
                              null);


  return ee.Image(focal_mean);
};
// end get_ function
// 
// 

// Mean NDVI
var ndvi_internal = function(preF)
{
  // Make each image from preFire collection into its NDVI image
  var ndvi = preF.map(function (img) {
          var ndvi = ee.Image(img).normalizedDifference(['B4', 'B3']);
          
          return ndvi;                     
        });
  
  var ndvi_img = ndvi.median();

  return ee.Image(ndvi_img);
};

//  NDVI function. 
// Returns the NDVI of each pixel
var get_ndvi = function(feature)
{
  var preFireCol = get_preFireRaw(feature);

  var ndvi = ee.Algorithms.If( preFireCol.median().bandNames(),
                              ndvi_internal(preFireCol).clip(feature),
                              null);


  return ee.Image(ndvi);
};
// end get_ndvi function
//
//

// Mean NDWI
var ndwi_internal = function(preF)
{
  // Make each image from preFire collection into its NDVI image
  var ndwi = preF.map(function (img) {
          var ndwi = ee.Image(img).normalizedDifference(['B4', 'B5']);
          
          return ndwi;                     
        });
  
  var ndwi_img = ndwi.median();

  return ee.Image(ndwi_img);
};

//  NDWI function. 
// Returns the NDWI of each pixel
var get_ndwi = function(feature)
{
  var preFireCol = get_preFireRaw(feature);

  var ndwi = ee.Algorithms.If( preFireCol.median().bandNames(),
                              ndwi_internal(preFireCol).clip(feature),
                              null);


  return ee.Image(ndwi);
};
// end get_ndwi function


var getSlope = function(firePerim)
{
  var terrain = ee.Algorithms.Terrain(elev);
  var slope = terrain.select('slope');
  return(slope.clip(firePerim));
};

var getAspect = function(firePerim)
{
  var terrain = ee.Algorithms.Terrain(elev);
  var aspect = terrain.select('aspect');
  return(aspect.clip(firePerim));
};

var getVariablesL5 = function(feature)
{
  var export_img = ee.Algorithms.If(feature.geometry().type() != 'Multipoint',
                                    getVariablesL5_internal(feature),
                                    null
                                    );
  
  return (ee.Image(export_img));
  
};

// // Start getVariables function
var getVariablesL5_internal = function(feature)
{   
    var geo = feature.geometry();
    
    var rdnbr = get_RdNBR(feature);
    var het030 = get_het(feature, 30.9); // About 0.3ha or about 3.3 pixels; upper ICO bound
    // var het045 = get_het(feature, 45); // About 0.6ha or about 7 pixels
    var het056 = get_het(feature, 56.4); // About 1ha or about 11 pixels
    // var het069 = get_het(feature, 69.1); // About 1.5ha or about 17 pixels
    var het079 = get_het(feature, 79.8); // About 2ha or about 22 pixels
    var het113 = get_het(feature, 113); // About 4 ha or about 44 pixels
    
    var ndvi_het030 = get_het_ndvi(feature, 30.9);
    // var ndvi_het045 = get_het_ndvi(feature, 45);
    var ndvi_het056 = get_het_ndvi(feature, 56.4);
    // var ndvi_het069 = get_het_ndvi(feature, 69.1);
    var ndvi_het079 = get_het_ndvi(feature, 79.8);
    var ndvi_het113 = get_het_ndvi(feature, 113);
    
    var focal_mean030 = get_focal_mean(feature, 30.9); // About 0.3ha or about 3.3 pixels; upper ICO bound
    // var focal_mean045 = get_focal_mean(feature, 45); // About 0.6ha or about 7 pixels
    var focal_mean056 = get_focal_mean(feature, 56.4); // About 1ha or about 11 pixels
    // var focal_mean069 = get_focal_mean(feature, 69.1); // About 1.5ha or about 17 pixels
    var focal_mean079 = get_focal_mean(feature, 79.8); // About 2ha or about 22 pixels
    var focal_mean113 = get_focal_mean(feature, 113); // About 4 ha or about 44 pixels
    
    var ndvi_focal_mean030 = get_focal_mean_ndvi(feature, 30.9);
    // var ndvi_focal_mean045 = get_focal_mean_ndvi(feature, 45);
    var ndvi_focal_mean056 = get_focal_mean_ndvi(feature, 56.4);
    // var ndvi_focal_mean069 = get_focal_mean_ndvi(feature, 69.1);
    var ndvi_focal_mean079 = get_focal_mean_ndvi(feature, 79.8);
    var ndvi_focal_mean113 = get_focal_mean_ndvi(feature, 113);

    var ndvi = get_ndvi(feature);
    var ndwi = get_ndwi(feature);
    
    var lonLat = ee.Image.pixelLonLat().clip(geo);
    var satellite = ee.Image(5).clip(geo);
    var date = ee.Image(ee.Number(feature.get('fire_date'))).clip(geo);
    var julian_day = ee.Image(ee.Number(ee.Date(feature.get('fire_date')).getRelative('day', 'year'))).clip(geo);
    var slope = getSlope(geo);
    var aspect = getAspect(geo);
    var local_elev = elev.clip(geo);

  var export_img =   
  ee.Algorithms.If(rdnbr, 
    rdnbr
      .addBands(het030)
      // .addBands(het045)
      .addBands(het056)
      // .addBands(het069)
      .addBands(het079)
      .addBands(het113)
      .addBands(ndvi_het030)
      // .addBands(ndvi_het045)
      .addBands(ndvi_het056)
      // .addBands(ndvi_het069)
      .addBands(ndvi_het079)
      .addBands(ndvi_het113)
      .addBands(focal_mean030)
      // .addBands(focal_mean045)
      .addBands(focal_mean056)
      // .addBands(focal_mean069)
      .addBands(focal_mean079)
      .addBands(focal_mean113)
      .addBands(ndvi_focal_mean030)
      // .addBands(ndvi_focal_mean045)
      .addBands(ndvi_focal_mean056)
      // .addBands(ndvi_focal_mean069)
      .addBands(ndvi_focal_mean079)
      .addBands(ndvi_focal_mean113)
      .addBands(ndvi)
      .addBands(ndwi)
      .addBands(satellite)
      .addBands(date)
      .addBands(julian_day)
      .addBands(lonLat)
      .addBands(slope)
      .addBands(aspect)
      .addBands(local_elev)
      .rename(['RdNBR', 
        'het030',
        // 'het045',
        'het056',
        // 'het069',
        'het079',
        'het113', 
        'ndvi_het030', 
        // 'ndvi_het045', 
        'ndvi_het056', 
        // 'ndvi_het069', 
        'ndvi_het079', 
        'ndvi_het113', 
        'focal_mean030',
        // 'focal_mean045',
        'focal_mean056',
        // 'focal_mean069',
        'focal_mean079',
        'focal_mean113', 
        'ndvi_focal_mean030', 
        // 'ndvi_focal_mean045', 
        'ndvi_focal_mean056', 
        // 'ndvi_focal_mean069', 
        'ndvi_focal_mean079', 
        'ndvi_focal_mean113', 
        'ndvi', 
        'ndwi',
        'satellite', 
        'date', 
        'julian_day', 
        'lon', 
        'lat', 
        'slope', 
        'aspect',
        // 'elev']);
        'elev']),
      null);
    
  
    return export_img;
};

// This function samples points from within an image at a specified scale and density
var getSamps = function(img)
{
  img = ee.Image(img);
  var s = img.sample({
          scale: 30,
          factor: 0.01
  });
  return s;
};


// No need to center or scale predictors when not trying to do the modeling in earth engine
// var centerScale = function(img)
// {
//   var mu = img.reduceRegion({reducer:ee.Reducer.mean(), scale:30, bestEffort:true}).toImage();
//   var sigma = img.reduceRegion({reducer:ee.Reducer.stdDev(), scale:30, bestEffort:true}).toImage();
  
//   var new_img = img.subtract(mu).divide(sigma);
//   return(new_img);
// };


// Isolating the Internal Error

// This date period gives me an "Internal error"
// var start = ee.Date('1986-06-01').millis();
// var end = ee.Date('2011-05-01').millis();

// This date gives an internal error
// var start = ee.Date('2005-05-01').millis();
// var end = ee.Date('2011-05-01').millis();

// Error
// var start = ee.Date('2008-05-01').millis();
// var end = ee.Date('2011-05-01').millis();

// Error
// var start = ee.Date('2009-05-01').millis();
// var end = ee.Date('2010-05-01').millis();

// Error
// var start = ee.Date('2009-05-01').millis();
// var end = ee.Date('2009-12-31').millis();

// Error
// var start = ee.Date('2009-07-01').millis();
// var end = ee.Date('2009-09-01').millis();

// Error
// var start = ee.Date('2009-08-01').millis();
// var end = ee.Date('2009-09-01').millis();

// Error
// var start = ee.Date('2009-08-01').millis();
// var end = ee.Date('2009-08-15').millis();

// Error
// var start = ee.Date('2009-08-15').millis();
// var end = ee.Date('2009-09-01').millis();

//
//
// FINAL ERROR REGION. IT'S THE YUBA FIRE ON 2009-08-15!
// var start = ee.Date('2009-08-15').millis();
// var end = ee.Date('2009-08-16').millis();
//
//
//

// This also works.
// var start = ee.Date('2009-08-11').millis();
// var end = ee.Date('2009-08-14').millis();

// This works!
// var start = ee.Date('2009-08-01').millis();
// var end = ee.Date('2009-08-10').millis();

// This works!!
// var start = ee.Date('2009-08-16').millis();
// var end = ee.Date('2009-08-20').millis();

// This works!!
// var start = ee.Date('2009-08-20').millis();
// var end = ee.Date('2009-09-01').millis();

// This period works!
// var start = ee.Date('2009-07-01').millis();
// var end = ee.Date('2009-08-01').millis();

// This period works!
// var start = ee.Date('2009-09-01').millis();
// var end = ee.Date('2009-12-31').millis();

// This period works!
// var start = ee.Date('2009-06-01').millis();
// var end = ee.Date('2009-07-01').millis();

// This period works!
// var start = ee.Date('2009-05-01').millis();
// var end = ee.Date('2009-06-01').millis();

// This period works!
// var start = ee.Date('2011-01-01').millis();
// var end = ee.Date('2011-12-31').millis();

// This period works!
// var start = ee.Date('2010-01-01').millis();
// var end = ee.Date('2010-12-31').millis();

// This period works.
// var start = ee.Date('2008-05-01').millis();
// var end = ee.Date('2009-05-01').millis();

// This date period seems to work fine.
// var start = ee.Date('2005-05-01').millis();
// var end = ee.Date('2008-05-01').millis();

// This date period seems to work fine.
// var start = ee.Date('2000-05-01').millis();
// var end = ee.Date('2005-05-01').millis();

// This date period seems to work fine.
// var start = ee.Date('1995-05-01').millis();
// var end = ee.Date('2000-05-01').millis();

// This date period seems to work fine.
// var start = ee.Date('1990-05-01').millis();
// var end = ee.Date('1995-05-01').millis();

// This date period seems to work fine.
// var start = ee.Date('1986-06-01').millis();
// var end = ee.Date('1990-05-01').millis();

// This date period also seems to work fine.
// var start = ee.Date('1990-06-01').millis();
// var end = ee.Date('1991-07-01').millis();


//
//
// Start of main program

// Define which Landsat dataset to use for all calcuations
var raw = l5SR;
// var raw = l5raw;

// Define the date range of the analysis. Sticking to only fires that would
// use the Landsat 5 imagery until I can determine a good relationship between
// Landsat 5 and Landsat 8

var start = ee.Date('1986-06-01').millis();
var end = ee.Date('2011-05-01').millis();

// Filter the FeatureCollection to the date range and exclude all fires
// that would be less than 1 pixel in area (30meters x 30meters = 900 meters^2)
var l5perims = perim
                .filter(ee.Filter.rangeContains({
                  field: 'fire_date',
                  minValue: start,
                  maxValue: end
                }))
                .filter(ee.Filter.gte({
                  name: 'area_m2',
                  value: 900
                }))
                .filterMetadata('name', 'not_equals', 'YUBA');
                // .map(function (ftr) {
                //     return ftr.buffer(-500);
                // });
 

// Testing why the YUBA fire breaks the code

// Filter to fire specified in error message
// var broken = l5perims.filterMetadata('system:index', 'equals', '638'); 
// // Another way to filter to the bad Feature
// var yuba = l5perims.filterMetadata('name', 'equals', 'YUBA');
// // Fitler to a good feature for comparison
var l5perims = l5perims.filterMetadata('name', 'equals', 'HAMM');

// // Printed objects look similar
// print(broken);
// print(hamm);

// // Printed geometries look similar
// print(broken.geometry());
// print(hamm.geometry());

// This does not properly add the geometry to the Map, even though it looks okay in the
// fusion table.
// Map.addLayer(broken.geometry());
// Map.centerObject(broken);

// // Map the variable retrieval function over all of the features
var imgCol = ee.FeatureCollection(l5perims.map(getVariablesL5, true));
// // Mask nonForested pixels based on the 1992 National Landcover Database (NLCD)
// imgCol = imgCol.map(maskNonForest);

// This looks fine.
// Map.addLayer(ee.Image(imgCol.first()).select(['RdNBR']));
// Map.centerObject(hamm, 9);



// // File name for .csv in a string
// var fileName = 'L5_19860601_20110501_het_resistance_001_00_percent_CALfire_data';

// // Sample from the images containing all response and predictor values
// var samps = imgCol.map(getSamps).flatten();

// // Export the tidy .csv to Google Drive
// Export.table.toDrive({
//   'collection': samps,
//   'description': fileName,
//   'folder': 'ee',
//   'fileNamePrefix': fileName,
//   'fileFormat': 'CSV'
// });


// Visualization, if desired
var RdNBR_viz = {min:0, max:25, palette:['aec3d4', '008000', 'ffff00',  'ffA500', 'ff0000']};
var test_img = ee.Image(imgCol.first());
Map.addLayer(test_img.select(['RdNBR']), RdNBR_viz, 'rdnbr');
Map.centerObject(test_img);












// Modeling efforts...

// var region = imgCol.first().get('system:footprint');

// // //
// // // Visualize
// // //

// Map.addLayer(sn);
// Map.addLayer(rdnbr, RdNBR_viz, 'RdNBR');
// Map.addLayer(het, het_viz, 'Heterogeneity');
// Map.centerObject(het, 14);

// MODELING EFFORT
// SCALE PARAMETERS
  // var mean_hetero = heterogeneity.mosaic().reduceRegion({
  //     reducer: ee.Reducer.mean(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();
  // var sd_hetero = heterogeneity.mosaic().reduceRegion({
  //     reducer: ee.Reducer.stdDev(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();

  // var mean_mu = neighbor_mu.mosaic().reduceRegion({
  //     reducer: ee.Reducer.mean(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();
  // var sd_mu = neighbor_mu.mosaic().reduceRegion({
  //     reducer: ee.Reducer.stdDev(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();

  // var mean_slope = slope.mosaic().reduceRegion({
  //     reducer: ee.Reducer.mean(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();
  // var sd_slope = slope.mosaic().reduceRegion({
  //     reducer: ee.Reducer.stdDev(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();

  // var mean_aspect = aspect.mosaic().divide(180).multiply(3.14159).cos().reduceRegion({
  //     reducer: ee.Reducer.mean(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();
  // var sd_aspect = aspect.mosaic().divide(180).multiply(3.14159).cos().reduceRegion({
  //     reducer: ee.Reducer.stdDev(), 
  //     geometry: overallPerims, 
  //     scale: 30,
  //     maxPixels: 1e9
  //     }).toImage();

  // var s_var = heterogeneity.mosaic().subtract(mean_hetero).divide(sd_hetero);
  // var s_mu = neighbor_mu.mosaic().subtract(mean_mu).divide(sd_mu);
  // var s_slope = slope.mosaic().subtract(mean_slope).divide(sd_slope);
  // var s_aspect = aspect.mosaic().divide(180).multiply(3.14159).cos().subtract(mean_aspect).divide(sd_aspect);
  
  // var model = ee.Image(1).clip(overallPerims);
  
  
  

// /*  
//   Map.addLayer(s_var, {min: -2, max: 2}, 'Scaled heterogeneity');
//   Map.addLayer(s_mu, {min: -2, max: 2}, 'Scaled neighborhood mu');
//   Map.addLayer(s_slope, {min: -2, max: 2}, 'Scaled slope');
//   Map.addLayer(s_aspect, {min: -2, max: 2}, 'Scaled aspect');
// */

  // var model_noResponse = model.addBands(s_var).addBands(s_mu).addBands(s_slope).addBands(s_aspect);
  // model = model_noResponse.addBands(rdnbr.mosaic());
  // model = model.rename(['intercept', 's_var', 's_mu', 's_slope', 's_aspect', 'severity']);
  
  // var model_collection = ee.ImageCollection(model);
  
  // var bands = [['intercept', 's_var', 's_mu', 's_slope', 's_aspect']];

  // var fit1 = model.reduceRegion({
  //     reducer: ee.Reducer.linearRegression({
  //       numX: 5, 
  //       numY: 1
  //       }),
  //     geometry: overallPerims,
  //     scale: 30,
  //     maxPixels: 1e9
  // });
  
// // Get the coefficients as a nested list, cast it to an array, and get
// // just the first column
// var coef = ee.Array(fit1.get('coefficients')).project([0]);

// // // Cast the array to an image, flatten it, and clip it to the extent
// var img = ee.Image(coef)
//       .arrayFlatten(bands)
//       .clip(overallPerims);
      
// var predictions = img.multiply(model_noResponse);
// var preds = predictions.reduce(ee.Reducer.sum());

// preds = preds.rename(['RdNBR']);

// var resids = rdnbr.mosaic().subtract(preds);

// var RdNBR_mean = ee.Image(ee.Number(rdnbr.mosaic().reduceRegion({
//     reducer: ee.Reducer.mean(), 
//     geometry: overallPerims, 
//     scale: 30,
//     maxPixels: 1e9
//     }).get('nd')));
    
// var SStot_sqrt = rdnbr.mosaic().subtract(RdNBR_mean);
// var SStot = ee.Number(SStot_sqrt.multiply(SStot_sqrt).reduceRegion({
//     reducer: ee.Reducer.sum(), 
//     geometry: overallPerims, 
//     scale: 30,
//     maxPixels: 1e9
//     }).get('nd'));

// var SSres = ee.Number(resids.multiply(resids).reduceRegion({
//     reducer: ee.Reducer.sum(), 
//     geometry: overallPerims, 
//     scale: 30,
//     maxPixels: 1e9
//     }).get('nd'));
    
// var R2 = ee.Number(1).subtract((SSres.divide(SStot)));

// print(R2);

// Export.table(coef, 'coef');
// Export.table(R2, 'R2');
// print(coef, 'coef');
// print(R2, 'R squared');


// var region = JSON.stringify(overallPerims.bounds().getInfo().coordinates);
 
  // print("Download preview:", h_visual.getThumbURL({region: region}))
  // print("Download preview:", hetero_visual.getThumbURL({region: region}))
  // print("Download preview:", neighbor_mu_visual.getThumbURL({region: region}))
  // print("Download preview:", distance_visual.getThumbURL({region: region}))
  
  // Export.image(rdnbr.mosaic().clip(overallPerims), 'RdNBR', {'region':region, 'scale':30});
  // Export.image(heterogeneity.mosaic().clip(overallPerims), 'hetero', {'region':region, 'scale':30});
  // Export.image(neighbor_mu.mosaic().clip(overallPerims), 'neighbor_mu', {'region':region, 'scale':30});
  // Export.image(slope.mosaic().clip(overallPerims), 'slope', {'region':region, 'scale':30});
  // Export.image(aspect.mosaic().clip(overallPerims), 'aspect', {'region':region, 'scale':30});
