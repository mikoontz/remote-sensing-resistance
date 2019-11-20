/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var sn = ee.FeatureCollection("users/mkoontz/SierraEcoregion_Jepson"),
    fires2017 = ee.FeatureCollection("users/mkoontz/fire17_1"),
    fires2016 = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    fires2018 = ee.FeatureCollection("users/mkoontz/fire18_1_sn_ypmc");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var rsr = require('users/mkoontz/ee-remote-sensing-resistance:rsr-functions.js');

// Purpose: Create an image of model variables given a feature collection (of points or polygons)
// and then visualize
// This is good for giving people .geotiffs of fire perimeters with variables mapped out

// Thresholds from a model using a 1-month time window and bicubic interpolation
// 42.12373 equates to a CBI of 0
// 45.09661 equates to a CBI of 0.1 -- threshold between "unchanged" and "low"
// 112.5589 equates to a CBI of 1.25 -- threshold between "low" and "medium"
// 282.3348 equates to a CBI of 2.25 -- threshold between "medium" and "high"

var RBR_viz = { min: 45.09661, max: 282.3348, 
                palette:['008000', 'ffff00',  'ffA500', 'ff0000']};

// Map.addLayer(rsr.sn, {}, 'Sierra Nevada perimeter (derived using Jepson ecoregions');
// Map.addLayer(rsr.mixed_conifer, {}, 'Mixed conifer forest (derived using pre-settlement fire regime from FRID)');
// Map.addLayer(cbi_sn, {color: 'red'}, 'CBI Plots (derived from 2 published sources');

// HAMM FIRE
var timeWindow = 16*3;
var resample_method = 'bicubic';
var sats_string = '45';
var sats = ee.List(sats_string.split(''));

var hamm = ee.Feature(fires2018
                        .filterBounds(sn)
                        .filter(ee.Filter.gte('alarm_date', ee.Date('1987-01-01').millis()))
                        .filter(ee.Filter.lt('alarm_date', ee.Date('1988-01-01').millis()))
                        .filterMetadata('fire_name', 'equals', 'HAMM').first());

var hamm_img = rsr.get_variables_lite(hamm, timeWindow, resample_method, sats).clip(hamm);

Map.addLayer(hamm_img.select(['RBR']), RBR_viz, 'The Hamm fire of 1987');

// Define the visualization parameters.
var natVizParamsPre = {
  bands: ['B3_pre', 'B2_pre', 'B1_pre'],
  min: 0,
  max: 2000,
  gamma: [0.9, 1.1, 1]
};

var natVizParamsPost = {
  bands: ['B3_post', 'B2_post', 'B1_post'],
  min: 0,
  max: 2000,
  gamma: [0.9, 1.1, 1]
};

// Define the visualization parameters.
var falseVizParamsPre = {
  bands: ['B4_pre', 'B3_pre', 'B2_pre'],
  min: 0,
  max: 10000,
  gamma: [1, 1, 1]
};

var falseVizParamsPost = {
  bands: ['B4_post', 'B3_post', 'B2_post'],
  min: 0,
  max: 10000,
  gamma: [1, 1, 1]
};

var fireVizParamsPre = {
  bands: ['B7_pre', 'B4_pre', 'B2_pre'],
  min: 0,
  max: 5000,
  gamma: [0.9, 0.8, 1]
};

var fireVizParamsPost = {
  bands: ['B7_post', 'B4_post', 'B2_post'],
  min: 0,
  max: 5000,
  gamma: [0.9, 0.8, 1]
};

Map.addLayer(hamm_img, natVizParamsPre, 'Prefire RGB');
Map.addLayer(hamm_img, natVizParamsPost, 'Postfire RGB');
Map.addLayer(hamm_img, falseVizParamsPre, 'Prefire false color');
Map.addLayer(hamm_img, falseVizParamsPost, 'Postfire false color');
Map.addLayer(hamm_img, fireVizParamsPre, 'Prefire fire viz');
Map.addLayer(hamm_img, fireVizParamsPost, 'Postfire fire viz');
Map.centerObject(hamm);

// Exports

// full resolution use a scale: 30
// med resolution use a scale: 100
// med-low-res versions use a scale: 200
// low resolution versions use a scale: 500 (?) 


Export.image.toDrive({
  image: hamm_img.float(),
  description: 'hamm-fire_1987_visualize_med-res',
  folder: 'ee',
  region: hamm,
  scale: 100
});

Export.image.toDrive({
  image: hamm_img.float(),
  description: 'hamm-fire_1987_visualize_full-res',
  folder: 'ee',
  region: hamm,
  scale: 30
});

// AMERICAN FIRE
var timeWindow = 16*3;
var resample_method = 'bicubic';
var sats_string = '78';
var sats = ee.List(sats_string.split(''));

var american = ee.Feature(fires2018
                        .filterBounds(sn)
                        .filter(ee.Filter.gte('alarm_date', ee.Date('2013-08-01').millis()))
                        .filter(ee.Filter.lt('alarm_date', ee.Date('2013-08-31').millis()))
                        .filterMetadata('fire_name', 'equals', 'AMERICAN').first());

var american_img = rsr.get_variables_lite(american, timeWindow, resample_method, sats).clip(american);

Map.addLayer(american_img.select(['RBR']).updateMask(american_img.select(['conifer_forest'])), RBR_viz, 'The American fire of 2013');
Map.addLayer(american_img.updateMask(american_img.select(['conifer_forest'])), natVizParamsPre, 'Prefire RGB American');
Map.addLayer(american_img.updateMask(american_img.select(['conifer_forest'])), natVizParamsPost, 'Postfire RGB American');

// Map.centerObject(american);

Export.image.toDrive({
  image: american_img.float(),
  description: 'american-fire_2013_visualize_med-res',
  folder: 'ee',
  region: american,
  scale: 100
});

Export.image.toDrive({
  image: american_img.float(),
  description: 'american-fire_2013_visualize_full-res',
  folder: 'ee',
  region: american,
  scale: 30
});

// Other fires of interest


// // KING FIRE

// timeWindow = 16*3;
// resample_method = 'bicubic';
// sats_string = '78';
// sats = ee.List(sats_string.split(''));

// var king = ee.Feature(fires2018
//                         .filterBounds(sn)
//                         .filter(ee.Filter.gte('alarm_date', ee.Date('2014-01-01').millis()))
//                         .filter(ee.Filter.lt('alarm_date', ee.Date('2015-01-01').millis()))
//                         .filterMetadata('fire_name', 'equals', 'KING').first());

// var king_img = rsr.get_variables_lite(king, timeWindow, resample_method, sats).clip(king);

// Map.addLayer(king_img.select(['RBR']), RBR_viz, 'The King fire of 2014');
// Map.centerObject(king_img);

// HIGHWAY FIRE
// timeWindow = 32;
// resample_method = 'bicubic';
// sats_string = '57';
// sats = ee.List(sats_string.split(''));

// var hwy = ee.Feature(fires2018
//                         .filterBounds(sn)
//                         .filter(ee.Filter.gte('alarm_date', ee.Date('2001-07-01').millis()))
//                         .filter(ee.Filter.lt('alarm_date', ee.Date('2001-08-01').millis()))
//                         .filterMetadata('fire_name', 'equals', 'HIGHWAY').first());

// var hwy_img = rsr.get_variables_lite(hwy, timeWindow, resample_method, sats).clip(hwy);

// Map.addLayer(hwy_img.select(['RBR']), RBR_viz, 'The Highway fire of 2001');
// Map.centerObject(hwy_img);


// MCNALLY FIRE
// timeWindow = 32;
// resample_method = 'bicubic';
// sats_string = '57';
// sats = ee.List(sats_string.split(''));

// var mcnally = ee.Feature(fires2018
//                         .filterBounds(sn)
//                         .filter(ee.Filter.gte('alarm_date', ee.Date('2002-07-01').millis()))
//                         .filter(ee.Filter.lt('alarm_date', ee.Date('2002-07-31').millis()))
//                         .filterMetadata('fire_name', 'equals', 'MCNALLY').first());

// var mcnally_img = rsr.get_variables_lite(mcnally, timeWindow, resample_method, sats).clip(mcnally);

// Map.addLayer(mcnally_img.select(['RBR']), RBR_viz, 'The McNally fire of 2002');
// Map.centerObject(mcnally_img);

// COTTONWOOD FIRE
// timeWindow = 48;
// resample_method = 'bicubic';
// sats_string = '57';
// sats = ee.List(sats_string.split(''));

// var cottonwood = ee.Feature(fires2018
//                         .filterBounds(sn)
//                         .filter(ee.Filter.gte('alarm_date', ee.Date('1994-07-31').millis()))
//                         .filter(ee.Filter.lt('alarm_date', ee.Date('1994-08-31').millis()))
//                         .filterMetadata('fire_name', 'equals', 'COTTONWOOD').first());
// var cottonwood_img = rsr.get_variables_lite(cottonwood, timeWindow, resample_method, sats).clip(cottonwood);

// Map.addLayer(cottonwood_img.select(['RBR']), RBR_viz, 'The Cottonwood fire of 1994');
// Map.centerObject(cottonwood_img);

// Export.image.toDrive({
//   image: cottonwood_img.float(),
//   description: 'cottonwood-fire_2009_visualize_full-res',
//   folder: 'ee',
//   region: cottonwood,
//   scale: 30
// });